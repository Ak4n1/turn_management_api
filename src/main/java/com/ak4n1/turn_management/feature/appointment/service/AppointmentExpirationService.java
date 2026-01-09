package com.ak4n1.turn_management.feature.appointment.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentHistoryRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService;
import com.ak4n1.turn_management.shared.scheduling.DistributedLockService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;

/**
 * Servicio para expiración automática de turnos no confirmados.
 * 
 * Implementa US-T015 y US-T033:
 * - Expira turnos en estado CREATED que han pasado su TTL
 * - Libera slots automáticamente
 * - Registra en auditoría
 * - Lock distribuido para ejecución en cluster (US-T033)
 * - Job idempotente (puede ejecutarse múltiples veces sin duplicar)
 * - **Pendiente:** Notificaciones WebSocket (FASE 9)
 */
@Service
public class AppointmentExpirationService {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentExpirationService.class);
    private static final String LOCK_KEY = "appointment-expiration";
    private static final int LOCK_TTL_MINUTES = 5; // Lock válido por 5 minutos

    private final AppointmentRepository appointmentRepository;
    private final AppointmentHistoryRepository historyRepository;
    private final DistributedLockService distributedLockService;
    private final WebSocketNotificationService webSocketNotificationService;

    public AppointmentExpirationService(
            AppointmentRepository appointmentRepository,
            AppointmentHistoryRepository historyRepository,
            DistributedLockService distributedLockService,
            WebSocketNotificationService webSocketNotificationService) {
        this.appointmentRepository = appointmentRepository;
        this.historyRepository = historyRepository;
        this.distributedLockService = distributedLockService;
        this.webSocketNotificationService = webSocketNotificationService;
    }

    /**
     * Job programado que expira automáticamente turnos no confirmados.
     * 
     * Se ejecuta cada minuto (60,000 ms).
     * 
     * Implementa US-T033:
     * - Lock distribuido para evitar ejecución simultánea en múltiples instancias
     * - Job idempotente (puede ejecutarse múltiples veces sin duplicar)
     * 
     * Expira turnos que:
     * - Están en estado CREATED
     * - Tienen expiresAt no nulo
     * - expiresAt <= ahora
     */
    @Scheduled(fixedRate = 60000) // Cada minuto (60,000 ms)
    public void expireUnconfirmedAppointments() {
        // Intentar adquirir lock distribuido
        if (!distributedLockService.tryAcquireLock(LOCK_KEY, LOCK_TTL_MINUTES)) {
            logger.debug("Job de expiración de turnos ya está siendo ejecutado por otra instancia. Saltando ejecución.");
            return;
        }

        try {
            // Ejecutar el job dentro de una transacción
            executeExpirationJob();
        } finally {
            // Liberar el lock al finalizar (incluso si hay error)
            distributedLockService.releaseLock(LOCK_KEY);
        }
    }

    /**
     * Ejecuta el trabajo de expiración de turnos.
     * Método separado para facilitar el manejo de transacciones y locks.
     */
    @Transactional
    private void executeExpirationJob() {
        logger.debug("Iniciando job de expiración de turnos no confirmados");

        // 1. Obtener fecha y hora actual en GMT-3
        LocalDateTime now = getNowGMT3();

        // 2. Buscar turnos expirados
        List<Appointment> expiredAppointments = appointmentRepository.findExpiredAppointments(now);

        if (expiredAppointments.isEmpty()) {
            logger.debug("No hay turnos expirados para procesar");
            return;
        }

        logger.info("Turnos expirados encontrados: {}", expiredAppointments.size());

        int expiredCount = 0;

        // 3. Expirar cada turno
        for (Appointment appointment : expiredAppointments) {
            try {
                // Validar que aún esté en estado CREATED (por si acaso)
                if (appointment.getState() != AppointmentState.CREATED) {
                    logger.warn("Turno {} no está en estado CREATED, saltando expiración. Estado actual: {}",
                        appointment.getId(), appointment.getState());
                    continue;
                }

                // Validar que el expiresAt sea anterior o igual a ahora
                if (appointment.getExpiresAt() == null || 
                    appointment.getExpiresAt().isAfter(now)) {
                    logger.warn("Turno {} no debería estar en la lista de expirados. expiresAt: {}, ahora: {}",
                        appointment.getId(), appointment.getExpiresAt(), now);
                    continue;
                }

                // 4. Expirar el turno
                AppointmentState previousState = appointment.getState();
                appointment.expire();
                appointmentRepository.save(appointment);
                expiredCount++;

                logger.info("Turno {} expirado automáticamente - Usuario: {}, Fecha: {}, Hora: {}",
                    appointment.getId(), appointment.getUserId(),
                    appointment.getAppointmentDate(), appointment.getStartTime());

                // 5. Registrar en auditoría
                recordExpirationInHistory(appointment, previousState);

                // 6. Enviar notificación WebSocket al usuario (asíncrono, no bloquea) - US-N001
                try {
                    webSocketNotificationService.sendNotificationToUser(
                        appointment.getUserId(),
                        com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_EXPIRED,
                        "Turno Expirado",
                        String.format("Tu turno para el %s a las %s ha expirado por no confirmarlo a tiempo.",
                            appointment.getAppointmentDate(), appointment.getStartTime()),
                        com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                        appointment.getId(),
                        appointment.getId()
                    );
                } catch (Exception e) {
                    logger.error("Error al enviar notificación WebSocket de turno expirado - Turno ID: {}. Error: {}", 
                        appointment.getId(), e.getMessage(), e);
                }

            } catch (Exception e) {
                logger.error("Error al expirar turno {}: {}", appointment.getId(), e.getMessage(), e);
                // Continuar con los demás turnos aunque uno falle
            }
        }

        logger.info("Job de expiración completado - Turnos expirados: {}", expiredCount);
    }

    /**
     * Registra la expiración en el historial de auditoría.
     */
    private void recordExpirationInHistory(Appointment appointment, AppointmentState previousState) {
        AppointmentHistory history = new AppointmentHistory(
            appointment.getId(),
            appointment.getUserId(),
            previousState,
            AppointmentState.EXPIRED,
            "EXPIRED",
            String.format("Turno expirado automáticamente por no confirmar dentro del TTL. " +
                "TTL: %s minutos, Expiró a las: %s",
                appointment.getExpiresAt() != null ? 
                    String.valueOf(java.time.Duration.between(appointment.getCreatedAt(), appointment.getExpiresAt()).toMinutes()) : 
                    "N/A",
                appointment.getExpiresAt() != null ? appointment.getExpiresAt().toString() : "N/A"),
            "SYSTEM"
        );
        historyRepository.save(history);
    }

    /**
     * Obtiene la fecha y hora actual en GMT-3 (Argentina).
     */
    private LocalDateTime getNowGMT3() {
        return LocalDateTime.now(ZoneId.of("America/Argentina/Buenos_Aires"));
    }
}

