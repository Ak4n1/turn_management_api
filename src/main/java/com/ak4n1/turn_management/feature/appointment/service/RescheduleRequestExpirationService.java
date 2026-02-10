package com.ak4n1.turn_management.feature.appointment.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequest;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentHistoryRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.appointment.repository.RescheduleRequestRepository;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse;
import com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService;
import com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.List;

/**
 * Servicio para expiración automática de solicitudes de reprogramación.
 * 
 * Implementa US-T014.4:
 * - Expira solicitudes antiguas (TTL configurable, default: 7 días)
 * - Expira solicitudes cuyo slot ya no está disponible
 * - Registra en auditoría
 * - **Pendiente:** Notificaciones WebSocket (FASE 9)
 */
@Service
public class RescheduleRequestExpirationService {

    private static final Logger logger = LoggerFactory.getLogger(RescheduleRequestExpirationService.class);

    private final RescheduleRequestRepository rescheduleRequestRepository;
    private final AppointmentRepository appointmentRepository;
    private final AppointmentHistoryRepository historyRepository;
    private final CalendarConfigurationService configurationService;
    private final WebSocketNotificationService webSocketNotificationService;

    /**
     * TTL (Time To Live) para solicitudes de reprogramación (en días).
     * Configurable desde application.properties.
     * Default: 7 días.
     */
    @Value("${reschedule-request.ttl-days:7}")
    private int rescheduleRequestTTLDays;

    public RescheduleRequestExpirationService(
            RescheduleRequestRepository rescheduleRequestRepository,
            AppointmentRepository appointmentRepository,
            AppointmentHistoryRepository historyRepository,
            CalendarConfigurationService configurationService,
            WebSocketNotificationService webSocketNotificationService) {
        this.rescheduleRequestRepository = rescheduleRequestRepository;
        this.appointmentRepository = appointmentRepository;
        this.historyRepository = historyRepository;
        this.configurationService = configurationService;
        this.webSocketNotificationService = webSocketNotificationService;
    }

    /**
     * Job programado que expira automáticamente solicitudes de reprogramación pendientes.
     * 
     * Se ejecuta diariamente a las 2:00 AM (GMT-3).
     * 
     * Expira solicitudes que:
     * 1. Han estado pendientes más tiempo que el TTL configurado
     * 2. Tienen un slot solicitado que ya no está disponible
     */
    @Scheduled(cron = "0 0 2 * * ?") // Diariamente a las 2 AM
    @Transactional
    public void expireRescheduleRequests() {
        logger.info("Iniciando job de expiración de solicitudes de reprogramación");

        // 1. Calcular threshold de tiempo (solicitudes más antiguas que el TTL)
        LocalDateTime expirationThreshold = getNowGMT3().minusDays(rescheduleRequestTTLDays);
        logger.info("Threshold de expiración por tiempo: {} (TTL: {} días)", expirationThreshold, rescheduleRequestTTLDays);

        // 2. Buscar solicitudes pendientes más antiguas que el threshold
        List<RescheduleRequest> oldPendingRequests = rescheduleRequestRepository
            .findPendingRequestsOlderThan(RescheduleRequestState.PENDING_ADMIN_APPROVAL, expirationThreshold);

        logger.info("Solicitudes pendientes antiguas encontradas: {}", oldPendingRequests.size());

        int expiredByTime = 0;
        int expiredBySlot = 0;

        // 3. Expirar solicitudes antiguas
        for (RescheduleRequest request : oldPendingRequests) {
            try {
                // Verificar si el slot sigue disponible
                boolean slotAvailable = isSlotAvailable(
                    request.getRequestedDate(),
                    request.getRequestedStartTime());

                if (!slotAvailable) {
                    // Slot ya no disponible
                    request.expire("Slot ya no disponible");
                    expiredBySlot++;
                    logger.info("Solicitud {} expirada: Slot ya no disponible", request.getId());
                } else {
                    // Expirada por tiempo
                    request.expire("Solicitud expirada por tiempo (TTL: " + rescheduleRequestTTLDays + " días)");
                    expiredByTime++;
                    logger.info("Solicitud {} expirada: Por tiempo (creada: {})", 
                        request.getId(), request.getCreatedAt());
                }

                rescheduleRequestRepository.save(request);

                // 4. Registrar en auditoría
                recordExpirationInHistory(request);

                // 5. Enviar notificaciones WebSocket (asíncrono, no bloquea) - US-N001, US-N002
                try {
                    // Notificar al usuario
                    webSocketNotificationService.sendNotificationToUser(
                        request.getUserId(),
                        com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_REJECTED,
                        "Solicitud de Reprogramación Expirada",
                        String.format("Tu solicitud de reprogramación ha expirado. Motivo: %s", request.getExpirationReason()),
                        com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                        request.getId(),
                        null
                    );
                    
                    // Notificar a admins
                    webSocketNotificationService.sendNotificationToAdmins(
                        com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_CANCELLED,
                        "Solicitud de Reprogramación Expirada",
                        String.format("Una solicitud de reprogramación ha expirado automáticamente. Solicitud ID: %d. Motivo: %s",
                            request.getId(), request.getExpirationReason()),
                        com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                        request.getId()
                    );
                } catch (Exception e) {
                    logger.error("Error al enviar notificaciones WebSocket de solicitud expirada - Solicitud ID: {}. Error: {}", 
                        request.getId(), e.getMessage(), e);
                }

            } catch (Exception e) {
                logger.error("Error al expirar solicitud {}: {}", request.getId(), e.getMessage(), e);
                // Continuar con las demás solicitudes aunque una falle
            }
        }

        logger.info("Job de expiración completado - Expiradas por tiempo: {}, Expiradas por slot no disponible: {}, Total: {}",
            expiredByTime, expiredBySlot, oldPendingRequests.size());
    }

    /**
     * Verifica si un slot está disponible según la configuración del calendario.
     */
    private boolean isSlotAvailable(LocalDate date, LocalTime startTime) {
        try {
            // Verificar disponibilidad usando el servicio de configuración
            AvailabilityResponse availability = configurationService.checkAvailability(date);
            
            if (!availability.getIsAvailable()) {
                return false; // Día cerrado
            }

            // Verificar que el horario esté dentro de los rangos disponibles
            boolean timeInRange = availability.getTimeRanges().stream()
                .anyMatch(range -> {
                    LocalTime rangeStart = LocalTime.parse(range.getStart());
                    LocalTime rangeEnd = LocalTime.parse(range.getEnd());
                    return !startTime.isBefore(rangeStart) && !startTime.isAfter(rangeEnd);
                });

            if (!timeInRange) {
                return false; // Fuera de horarios
            }

            // Verificar si el slot está ocupado por otro turno (RESCHEDULED no ocupa)
            List<AppointmentState> occupyingStates = List.of(
                AppointmentState.CREATED,
                AppointmentState.CONFIRMED
            );

            // Buscar turnos que ocupan el slot exacto (misma fecha y hora de inicio)
            List<Appointment> conflictingAppointments = appointmentRepository
                .findByDateAndStartTimeAndStateIn(date, startTime, occupyingStates);

            if (!conflictingAppointments.isEmpty()) {
                logger.debug("Slot {} {} ocupado por {} turno(s)", date, startTime, conflictingAppointments.size());
                return false; // Slot ocupado
            }

            return true; // Slot disponible
        } catch (Exception e) {
            logger.warn("Error al verificar disponibilidad del slot {} {}: {}", 
                date, startTime, e.getMessage());
            return false; // En caso de error, consideramos no disponible
        }
    }

    /**
     * Registra la expiración en el historial de auditoría.
     */
    private void recordExpirationInHistory(RescheduleRequest request) {
        // Buscar el turno original
        var appointment = appointmentRepository.findById(request.getAppointmentId()).orElse(null);
        
        if (appointment != null) {
            AppointmentHistory history = new AppointmentHistory(
                appointment.getId(),
                request.getUserId(),
                appointment.getState(), // Estado anterior (no cambia)
                appointment.getState(), // Estado nuevo (no cambia)
                "RESCHEDULE_REQUEST_EXPIRED",
                String.format("Solicitud de reprogramación expirada automáticamente. Solicitud ID: %d. Motivo: %s",
                    request.getId(), request.getExpirationReason()),
                "SYSTEM"
            );
            historyRepository.save(history);
        }
    }

    /**
     * Obtiene la fecha y hora actual en GMT-3 (Argentina).
     */
    private LocalDateTime getNowGMT3() {
        return LocalDateTime.now(ZoneId.of("America/Argentina/Buenos_Aires"));
    }
}

