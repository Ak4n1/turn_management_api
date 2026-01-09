package com.ak4n1.turn_management.feature.appointment.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.service.EmailService;
import com.ak4n1.turn_management.feature.notification.service.NotificationPreferenceService;
import com.ak4n1.turn_management.shared.scheduling.DistributedLockService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;

/**
 * Servicio programado para enviar recordatorios de turnos.
 * 
 * Implementa US-T030 y US-T032:
 * - Envía recordatorios automáticamente antes del turno (configurable, por defecto 12 horas)
 * - Solo para turnos CONFIRMED
 * - Evita duplicados usando campo reminderSent
 * - Respetando zona horaria GMT-3
 * - Lock distribuido para ejecución en cluster (US-T032)
 * - Job idempotente (puede ejecutarse múltiples veces sin duplicar)
 */
@Service
public class AppointmentReminderService {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentReminderService.class);
    private static final String LOCK_KEY = "appointment-reminders";
    private static final int LOCK_TTL_MINUTES = 10; // Lock válido por 10 minutos

    private final AppointmentRepository appointmentRepository;
    private final UserRepository userRepository;
    private final EmailService emailService;
    private final DistributedLockService distributedLockService;
    private final NotificationPreferenceService preferenceService;

    /**
     * Horas antes del turno para enviar el recordatorio (valor por defecto del sistema).
     * Configurable desde application.properties.
     * Por defecto: 12 horas.
     * 
     * NOTA: Este valor se usa como fallback si el usuario no tiene preferencias configuradas.
     * Si el usuario tiene preferencias, se usa el valor de su preferencia.
     */
    @Value("${appointment.reminder.hours-before:12}")
    private int defaultReminderHoursBefore;

    /**
     * Zona horaria del sistema (GMT-3, Argentina).
     */
    private static final ZoneId ZONE_ID = ZoneId.of("America/Argentina/Buenos_Aires");

    public AppointmentReminderService(
            AppointmentRepository appointmentRepository,
            UserRepository userRepository,
            EmailService emailService,
            DistributedLockService distributedLockService,
            NotificationPreferenceService preferenceService) {
        this.appointmentRepository = appointmentRepository;
        this.userRepository = userRepository;
        this.emailService = emailService;
        this.distributedLockService = distributedLockService;
        this.preferenceService = preferenceService;
    }

    /**
     * Job programado que se ejecuta cada hora para enviar recordatorios.
     * 
     * Busca turnos CONFIRMED que:
     * - No han recibido recordatorio (reminderSent = false)
     * - Están dentro del rango de tiempo para recordatorio (reminderHoursBefore horas antes)
     * 
     * Implementa US-T032:
     * - Lock distribuido para evitar ejecución simultánea en múltiples instancias
     * - Job idempotente (puede ejecutarse múltiples veces sin duplicar)
     * - No duplica envíos (verifica reminderSent antes y después)
     * 
     * Cron: "0 0 * * * ?" = cada hora en el minuto 0
     */
    @Scheduled(cron = "0 0 * * * ?") // Cada hora
    public void sendAppointmentReminders() {
        // Intentar adquirir lock distribuido
        if (!distributedLockService.tryAcquireLock(LOCK_KEY, LOCK_TTL_MINUTES)) {
            logger.debug("Job de recordatorios ya está siendo ejecutado por otra instancia. Saltando ejecución.");
            return;
        }

        try {
            // Ejecutar el job dentro de una transacción
            executeReminderJob();
        } finally {
            // Liberar el lock al finalizar (incluso si hay error)
            distributedLockService.releaseLock(LOCK_KEY);
        }
    }

    /**
     * Ejecuta el trabajo de envío de recordatorios.
     * Método separado para facilitar el manejo de transacciones y locks.
     */
    @Transactional
    private void executeReminderJob() {
        try {
            LocalDateTime now = LocalDateTime.now(ZONE_ID);
            
            // Calcular rango de tiempo para recordatorios usando el valor por defecto
            // NOTA: Cada usuario puede tener su propia preferencia, pero para la búsqueda inicial
            // usamos el valor por defecto. Luego verificamos las preferencias individuales.
            LocalDateTime reminderTimeStart = now.plusHours(defaultReminderHoursBefore - 1);
            LocalDateTime reminderTimeEnd = now.plusHours(defaultReminderHoursBefore + 1);

            logger.info("Ejecutando job de recordatorios - Rango: {} a {}", reminderTimeStart, reminderTimeEnd);

            // Buscar turnos que necesitan recordatorio
            // Nota: La query necesita ajustarse porque CONCAT no funciona bien con LocalDate y LocalTime
            // Vamos a buscar de otra manera
            List<Appointment> appointments = findAppointmentsForReminder(reminderTimeStart, reminderTimeEnd);

            if (appointments.isEmpty()) {
                logger.debug("No hay turnos que necesiten recordatorio en este momento");
                return;
            }

            logger.info("Encontrados {} turno(s) que necesitan recordatorio", appointments.size());

            int sentCount = 0;
            int errorCount = 0;

            for (Appointment appointment : appointments) {
                try {
                    // Re-validar que el turno sigue en estado CONFIRMED (puede haber cambiado)
                    if (appointment.getState() != AppointmentState.CONFIRMED) {
                        logger.debug("Turno {} ya no está CONFIRMED, estado actual: {}. Saltando recordatorio.",
                            appointment.getId(), appointment.getState());
                        continue;
                    }

                    // Re-validar que el recordatorio no se haya enviado (puede haber cambiado)
                    if (Boolean.TRUE.equals(appointment.getReminderSent())) {
                        logger.debug("Recordatorio ya enviado para turno {}. Saltando.", appointment.getId());
                        continue;
                    }

                    // Obtener usuario
                    User user = userRepository.findById(appointment.getUserId())
                        .orElse(null);

                    if (user == null) {
                        logger.warn("Usuario no encontrado para turno {}. Saltando recordatorio.", appointment.getId());
                        errorCount++;
                        continue;
                    }

                    // Verificar si el recordatorio debe enviarse ahora según las preferencias del usuario
                    int userReminderHours = preferenceService.getReminderHoursBefore(user.getId());
                    LocalDateTime appointmentDateTime = LocalDateTime.of(appointment.getAppointmentDate(), appointment.getStartTime());
                    LocalDateTime reminderTime = appointmentDateTime.minusHours(userReminderHours);
                    
                    // Verificar si estamos dentro de la ventana de tiempo para este usuario específico
                    // Ventana: 1 hora antes y 1 hora después del tiempo ideal de recordatorio
                    if (now.isBefore(reminderTime.minusHours(1)) || now.isAfter(reminderTime.plusHours(1))) {
                        logger.debug("Turno {} no está en la ventana de recordatorio para usuario {} (preferencia: {} horas antes). Saltando.",
                            appointment.getId(), user.getEmail(), userReminderHours);
                        continue;
                    }

                    // Enviar email de recordatorio (asíncrono)
                    // El EmailService ya verifica las preferencias del usuario antes de enviar
                    emailService.sendAppointmentReminderEmail(user, appointment);

                    // Marcar como enviado
                    appointment.setReminderSent(true);
                    appointmentRepository.save(appointment);

                    sentCount++;
                    logger.info("Recordatorio enviado exitosamente - Turno ID: {}, Usuario: {}, Fecha: {}, Hora: {}",
                        appointment.getId(), user.getEmail(), appointment.getAppointmentDate(), appointment.getStartTime());

                } catch (Exception e) {
                    errorCount++;
                    logger.error("Error al enviar recordatorio para turno ID: {}. Error: {}",
                        appointment.getId(), e.getMessage(), e);
                    // Continuar con el siguiente turno aunque haya error
                }
            }

            logger.info("Job de recordatorios completado - Enviados: {}, Errores: {}, Total procesados: {}",
                sentCount, errorCount, appointments.size());

        } catch (Exception e) {
            logger.error("Error crítico en job de recordatorios: {}", e.getMessage(), e);
        }
    }

    /**
     * Busca turnos que necesitan recordatorio.
     * 
     * Busca turnos CONFIRMED donde:
     * - reminderSent = false
     * - La fecha/hora del turno está dentro del rango especificado
     * 
     * @param reminderTimeStart Inicio del rango de tiempo
     * @param reminderTimeEnd Fin del rango de tiempo
     * @return Lista de turnos que necesitan recordatorio
     */
    private List<Appointment> findAppointmentsForReminder(LocalDateTime reminderTimeStart, LocalDateTime reminderTimeEnd) {
        // Convertir LocalDateTime a LocalDate para la búsqueda
        LocalDate startDate = reminderTimeStart.toLocalDate();
        LocalDate endDate = reminderTimeEnd.toLocalDate();

        // Buscar turnos en el rango de fechas
        List<Appointment> candidates = appointmentRepository.findAllByAppointmentDateBetween(startDate, endDate);

        // Filtrar manualmente por estado, reminderSent y rango de tiempo
        return candidates.stream()
            .filter(a -> a.getState() == AppointmentState.CONFIRMED)
            .filter(a -> !Boolean.TRUE.equals(a.getReminderSent()))
            .filter(a -> {
                LocalDateTime appointmentDateTime = LocalDateTime.of(a.getAppointmentDate(), a.getStartTime());
                return !appointmentDateTime.isBefore(reminderTimeStart) && !appointmentDateTime.isAfter(reminderTimeEnd);
            })
            .toList();
    }
}

