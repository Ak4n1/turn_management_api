package com.ak4n1.turn_management.feature.appointment.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequest;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;
import com.ak4n1.turn_management.feature.appointment.dto.request.AdminRescheduleAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CancelAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateOverrideAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateRescheduleRequestRequest;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminAppointmentResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminAppointmentsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminRescheduleRequestResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminRescheduleRequestsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentHistoryResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentsCalendarResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.CalendarAppointmentInfo;
import com.ak4n1.turn_management.feature.appointment.dto.response.CalendarDayResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.CalendarSlotResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.HistoryItemResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.MyAppointmentsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.MyRescheduleRequestsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.RescheduleRequestResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.feature.notification.service.EmailService;
import com.ak4n1.turn_management.feature.notification.service.SystemNotificationService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentHistoryRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.appointment.repository.RescheduleRequestRepository;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import jakarta.persistence.OptimisticLockException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

/**
 * Implementación del servicio de gestión de turnos.
 */
@Service
public class AppointmentServiceImpl implements AppointmentService {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentServiceImpl.class);

    private final AppointmentRepository appointmentRepository;
    private final AppointmentHistoryRepository historyRepository;
    private final RescheduleRequestRepository rescheduleRequestRepository;
    private final CalendarConfigurationRepository configurationRepository;
    private final CalendarConfigurationService configurationService;
    private final UserService userService;
    private final UserRepository userRepository;
    private final SystemNotificationService notificationService;
    private final EmailService emailService;
    private final com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService webSocketNotificationService;
    private final com.ak4n1.turn_management.feature.configuration.service.BusinessPolicyService businessPolicyService;

    /**
     * Cache de la política activa para evitar consultas excesivas a la BD.
     * Se invalida automáticamente después de 1 minuto.
     */
    private volatile com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy cachedActivePolicy;
    private volatile long cachedPolicyTimestamp = 0;
    private static final long CACHE_TTL_MS = 60000; // 1 minuto de cache

    /**
     * Valores por defecto si no hay política activa (fallback a application.properties).
     * Estos valores se usan solo si no existe una política activa en la BD.
     */
    @Value("${appointment.created.ttl-minutes:10}")
    private int defaultCreatedTtlMinutes;

    @Value("${appointment.minimum-anticipation-hours:2}")
    private int defaultMinimumAnticipationHours;

    @Value("${appointment.maximum-advance-days:30}")
    private int defaultMaximumAdvanceDays;

    @Value("${appointment.max-per-user-per-day:1}")
    private int defaultMaxAppointmentsPerUserPerDay;

    @Value("${appointment.max-per-user-per-week:5}")
    private int defaultMaxAppointmentsPerUserPerWeek;

    @Value("${appointment.minimum-cancellation-window-hours:2}")
    private int defaultMinimumCancellationWindowHours;

    public AppointmentServiceImpl(
            AppointmentRepository appointmentRepository,
            AppointmentHistoryRepository historyRepository,
            RescheduleRequestRepository rescheduleRequestRepository,
            CalendarConfigurationRepository configurationRepository,
            CalendarConfigurationService configurationService,
            UserService userService,
            UserRepository userRepository,
            SystemNotificationService notificationService,
            EmailService emailService,
            com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService webSocketNotificationService,
            com.ak4n1.turn_management.feature.configuration.service.BusinessPolicyService businessPolicyService) {
        this.appointmentRepository = appointmentRepository;
        this.historyRepository = historyRepository;
        this.rescheduleRequestRepository = rescheduleRequestRepository;
        this.configurationRepository = configurationRepository;
        this.configurationService = configurationService;
        this.userService = userService;
        this.userRepository = userRepository;
        this.notificationService = notificationService;
        this.emailService = emailService;
        this.webSocketNotificationService = webSocketNotificationService;
        this.businessPolicyService = businessPolicyService;
    }

    /**
     * Obtiene la política activa con cache (1 minuto de TTL).
     * Si no hay política activa, retorna null.
     */
    private com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy getActivePolicyWithCache() {
        long now = System.currentTimeMillis();
        
        // Verificar si el cache es válido
        if (cachedActivePolicy != null && (now - cachedPolicyTimestamp) < CACHE_TTL_MS) {
            return cachedActivePolicy;
        }
        
        // Obtener política activa de la BD
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = 
            businessPolicyService.getActivePolicyEntity();
        
        // Actualizar cache
        cachedActivePolicy = policy;
        cachedPolicyTimestamp = now;
        
        return policy;
    }

    /**
     * Obtiene el TTL de turnos creados (en minutos).
     * Usa la política activa si existe, sino usa el valor por defecto.
     */
    private int getCreatedTtlMinutes() {
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = getActivePolicyWithCache();
        return policy != null && policy.getCreatedAppointmentTtlMinutes() != null
            ? policy.getCreatedAppointmentTtlMinutes()
            : defaultCreatedTtlMinutes;
    }

    /**
     * Obtiene la anticipación mínima para crear turnos (en horas).
     * Usa la política activa si existe, sino usa el valor por defecto.
     */
    private int getMinimumAnticipationHours() {
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = getActivePolicyWithCache();
        return policy != null && policy.getMinimumAdvanceHours() != null
            ? policy.getMinimumAdvanceHours()
            : defaultMinimumAnticipationHours;
    }

    /**
     * Obtiene la anticipación máxima para crear turnos (en días).
     * Usa la política activa si existe, sino usa el valor por defecto (30 días = 1 mes).
     */
    private int getMaximumAdvanceDays() {
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = getActivePolicyWithCache();
        return policy != null && policy.getMaximumAdvanceDays() != null
            ? policy.getMaximumAdvanceDays()
            : defaultMaximumAdvanceDays;
    }

    /**
     * Obtiene el límite máximo de turnos por usuario por día.
     * Usa la política activa si existe, sino usa el valor por defecto.
     */
    private int getMaxAppointmentsPerUserPerDay() {
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = getActivePolicyWithCache();
        return policy != null && policy.getMaxAppointmentsPerUserPerDay() != null
            ? policy.getMaxAppointmentsPerUserPerDay()
            : defaultMaxAppointmentsPerUserPerDay;
    }

    /**
     * Obtiene el límite máximo de turnos por usuario por semana.
     * Usa la política activa si existe, sino usa el valor por defecto.
     */
    private int getMaxAppointmentsPerUserPerWeek() {
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = getActivePolicyWithCache();
        return policy != null && policy.getMaxAppointmentsPerUserPerWeek() != null
            ? policy.getMaxAppointmentsPerUserPerWeek()
            : defaultMaxAppointmentsPerUserPerWeek;
    }

    /**
     * Obtiene la ventana mínima para cancelar turnos (en horas).
     * Usa la política activa si existe, sino usa el valor por defecto.
     */
    private int getMinimumCancellationWindowHours() {
        com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy policy = getActivePolicyWithCache();
        return policy != null && policy.getMinimumCancellationWindowHours() != null
            ? policy.getMinimumCancellationWindowHours()
            : defaultMinimumCancellationWindowHours;
    }

    @Override
    @Transactional
    public AppointmentResponse createAppointment(CreateAppointmentRequest request, Long userId,
                                                 String idempotencyKey, String clientIp) {
        logger.info("Solicitud de creación de turno - Usuario: {}, Fecha: {}, Hora: {}, IdempotencyKey: {}",
            userId, request.getDate(), request.getStartTime(), idempotencyKey);

        // 1. Validar idempotencia
        if (idempotencyKey != null && !idempotencyKey.isBlank()) {
            Optional<Appointment> existingAppointment = appointmentRepository.findByIdempotencyKey(idempotencyKey);
            if (existingAppointment.isPresent()) {
                logger.info("Turno existente encontrado por Idempotency-Key: {}", idempotencyKey);
                return mapToResponse(existingAppointment.get());
            }
        }

        // 2. Validar fecha no pasada
        LocalDate today = getTodayGMT3();
        if (request.getDate().isBefore(today)) {
            throw new ApiException(
                "No se pueden crear turnos para fechas pasadas. Fecha solicitada: " + request.getDate(),
                HttpStatus.BAD_REQUEST);
        }

        // 3. Validar anticipación mínima (usando política dinámica)
        LocalDateTime appointmentDateTime = LocalDateTime.of(request.getDate(), 
            LocalTime.parse(request.getStartTime(), DateTimeFormatter.ofPattern("HH:mm")));
        LocalDateTime now = getNowGMT3();
        long hoursUntilAppointment = ChronoUnit.HOURS.between(now, appointmentDateTime);
        int minAnticipation = getMinimumAnticipationHours();
        
        if (hoursUntilAppointment < minAnticipation) {
            throw new ApiException(
                String.format("No se pueden crear turnos con menos de %d horas de anticipación. " +
                    "Fecha/hora solicitada: %s %s, Anticipación requerida: %d horas",
                    minAnticipation, request.getDate(), request.getStartTime(), minAnticipation),
                HttpStatus.BAD_REQUEST);
        }

        // 3.1. Validar anticipación máxima (usando política dinámica)
        long daysUntilAppointment = ChronoUnit.DAYS.between(today, request.getDate());
        int maxAdvanceDays = getMaximumAdvanceDays();
        
        if (daysUntilAppointment > maxAdvanceDays) {
            throw new ApiException(
                String.format("No se pueden crear turnos con más de %d días de anticipación. " +
                    "Fecha solicitada: %s, Máximo permitido: %d días desde hoy (%s)",
                    maxAdvanceDays, request.getDate(), maxAdvanceDays, today.plusDays(maxAdvanceDays)),
                HttpStatus.BAD_REQUEST);
        }

        // 3.2. Validar perfil completo (seguridad backend: no depender del frontend)
        User user = userRepository.findById(userId)
            .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
        if (!user.isProfileComplete()) {
            throw new ApiException(
                "Para pedir un turno debes completar tu información personal: teléfono, calle, número, piso, ciudad, código postal y fecha de nacimiento.",
                HttpStatus.BAD_REQUEST);
        }

        // 4. Obtener configuración activa
        CalendarConfiguration activeConfig = configurationRepository.findByActiveTrue()
            .orElseThrow(() -> new ApiException(
                "No existe una configuración activa. El sistema no está configurado.",
                HttpStatus.SERVICE_UNAVAILABLE));

        Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
        if (appointmentDurationMinutes == null) {
            throw new ApiException(
                "No hay duración de turnos configurada. Debe configurar la duración antes de crear turnos.",
                HttpStatus.BAD_REQUEST);
        }

        // 5. Calcular hora de fin del turno
        LocalTime startTime = LocalTime.parse(request.getStartTime(), DateTimeFormatter.ofPattern("HH:mm"));
        LocalTime endTime = startTime.plusMinutes(appointmentDurationMinutes);

        // 6. CAMBIO CRÍTICO: Validar disponibilidad del día PRIMERO (antes de límites de usuario)
        com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse availability = 
            configurationService.checkAvailability(request.getDate());
        
        if (!Boolean.TRUE.equals(availability.getIsAvailable())) {
            // El día no está disponible
            if (Boolean.TRUE.equals(availability.getHasExistingAppointments()) && 
                availability.getExistingAppointmentsCount() != null &&
                availability.getExistingAppointmentsCount() > 0) {
                // Día cerrado CON turnos existentes - mensaje específico
                throw new ApiException(
                    String.format(
                        "Este día está cerrado según la configuración actual, pero tiene %d turno(s) existente(s) " +
                        "creado(s) con una configuración anterior. No se pueden crear nuevos turnos. " +
                        "Si necesitas un turno, contacta al administrador.",
                        availability.getExistingAppointmentsCount()
                    ),
                    HttpStatus.BAD_REQUEST);
            } else {
                // Día cerrado SIN turnos existentes - mensaje genérico
                String message = availability.getMessage();
                if (message == null || message.isEmpty()) {
                    message = "Este día está cerrado según la configuración actual. No se pueden crear nuevos turnos. " +
                             "Si necesitas un turno, contacta al administrador.";
                }
                throw new ApiException(message, HttpStatus.BAD_REQUEST);
            }
        }

        // 7. Validar límites de turnos por usuario (SOLO si el día está disponible)
        validateUserLimits(userId, request.getDate());

        // 8. Validar disponibilidad real del slot (re-validación atómica con lock pesimista)
        validateSlotAvailability(request.getDate(), startTime, appointmentDurationMinutes);

        // 8. Calcular TTL (expiresAt) usando política dinámica
        LocalDateTime expiresAt = now.plusMinutes(getCreatedTtlMinutes());

        // 9. Crear turno con lock pesimista para evitar concurrencia
        try {
            Appointment appointment = new Appointment(
                userId,
                request.getDate(),
                startTime,
                endTime,
                appointmentDurationMinutes,
                AppointmentState.CREATED,
                activeConfig.getVersion(),
                expiresAt,
                idempotencyKey,
                userId
            );

            appointment = appointmentRepository.save(appointment);
            logger.info("Turno creado exitosamente - ID: {}, Usuario: {}, Fecha: {}, Hora: {}",
                appointment.getId(), userId, request.getDate(), request.getStartTime());

            // 10. Registrar en auditoría
            AppointmentHistory history = new AppointmentHistory(
                appointment.getId(),
                userId,
                null, // previousState es null para creación
                AppointmentState.CREATED,
                "CREATED",
                request.getNotes(),
                clientIp
            );
            historyRepository.save(history);

            // 11. Enviar notificación WebSocket (asíncrono, no bloquea) - US-N001
            try {
                webSocketNotificationService.sendNotificationToUser(
                    userId,
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_CREATED,
                    "Turno Creado",
                    String.format("Tu turno para el %s a las %s ha sido creado. Debes confirmarlo antes de que expire.",
                        request.getDate(), request.getStartTime()),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                    appointment.getId(),
                    appointment.getId()
                );
            } catch (Exception e) {
                // NO lanzar excepción - la notificación WebSocket no debe bloquear la creación del turno
                logger.error("Error al enviar notificación WebSocket de turno creado - Turno ID: {}. Error: {}", 
                    appointment.getId(), e.getMessage(), e);
            }

            // 13. Enviar actualización de disponibilidad en tiempo real a todos los usuarios - Tiempo Real
            try {
                webSocketNotificationService.broadcastAvailabilityUpdate(request.getDate());
            } catch (Exception e) {
                logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                    request.getDate(), e.getMessage(), e);
            }

            return mapToResponse(appointment);

        } catch (ObjectOptimisticLockingFailureException | OptimisticLockException e) {
            logger.warn("Conflicto de concurrencia al crear turno - Usuario: {}, Fecha: {}, Hora: {}",
                userId, request.getDate(), request.getStartTime());
            throw new ApiException(
                "El slot seleccionado ya no está disponible. Por favor, intenta con otro horario.",
                HttpStatus.CONFLICT);
        }
    }

    /**
     * Valida que el slot esté disponible (re-validación atómica).
     * Usa lock pesimista para evitar condiciones de carrera.
     */
    private void validateSlotAvailability(LocalDate date, LocalTime startTime, Integer durationMinutes) {
        // Estados que ocupan un slot (RESCHEDULED no ocupa: el turno ya fue movido a otro horario)
        List<AppointmentState> occupyingStates = List.of(
            AppointmentState.CREATED,
            AppointmentState.CONFIRMED
        );

        // Buscar turnos que ocupan el slot con lock pesimista
        List<Appointment> conflictingAppointments = appointmentRepository
            .findByDateAndStartTimeAndStateInWithLock(date, startTime, occupyingStates);

        if (!conflictingAppointments.isEmpty()) {
            throw new ApiException(
                "El slot seleccionado ya no está disponible",
                HttpStatus.CONFLICT);
        }

        // Validar que el slot existe y está disponible según la configuración del calendario
        // (esto valida reglas de calendario, bloqueos, excepciones, etc.)
        try {
            var availability = configurationService.checkAvailability(date);
            if (!Boolean.TRUE.equals(availability.getIsAvailable()) ||
                availability.getTimeRanges() == null ||
                availability.getTimeRanges().isEmpty()) {
                
                // CAMBIO: Usar el mensaje de disponibilidad si está disponible
                String errorMessage = availability.getMessage();
                if (errorMessage == null || errorMessage.isEmpty()) {
                    errorMessage = "El slot seleccionado no está disponible según la configuración del calendario";
                }
                
                throw new ApiException(errorMessage, HttpStatus.BAD_REQUEST);
            }

            // Validar que el slot específico está dentro de los rangos horarios disponibles
            boolean slotInRange = availability.getTimeRanges().stream()
                .anyMatch(range -> {
                    LocalTime rangeStart = LocalTime.parse(range.getStart(), DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime rangeEnd = LocalTime.parse(range.getEnd(), DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime slotEnd = startTime.plusMinutes(durationMinutes);
                    return (startTime.isAfter(rangeStart) || startTime.equals(rangeStart)) &&
                           (slotEnd.isBefore(rangeEnd) || slotEnd.equals(rangeEnd));
                });

            if (!slotInRange) {
                throw new ApiException(
                    "El slot seleccionado no está dentro de los horarios disponibles",
                    HttpStatus.BAD_REQUEST);
            }

        } catch (ApiException e) {
            throw e;
        } catch (Exception e) {
            logger.error("Error al validar disponibilidad: {}", e.getMessage());
            throw new ApiException(
                "Error al validar disponibilidad del slot",
                HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Valida los límites de turnos por usuario.
     */
    private void validateUserLimits(Long userId, LocalDate appointmentDate) {
        // Validar límite por día
        List<AppointmentState> activeStates = List.of(
            AppointmentState.CREATED,
            AppointmentState.CONFIRMED
        );

        long appointmentsToday = appointmentRepository.countByUserIdAndStateInAndDateRange(
            userId, activeStates, appointmentDate, appointmentDate);
        int maxPerDay = getMaxAppointmentsPerUserPerDay();

        if (appointmentsToday >= maxPerDay) {
            throw new ApiException(
                String.format("Has alcanzado el límite de %d turno(s) por día. Ya tienes %d turno(s) el %s",
                    maxPerDay, appointmentsToday, appointmentDate),
                HttpStatus.BAD_REQUEST);
        }

        // Validar límite por semana (desde el lunes de la semana del turno)
        LocalDate weekStart = appointmentDate.with(java.time.DayOfWeek.MONDAY);
        LocalDate weekEnd = weekStart.plusDays(6);

        long appointmentsThisWeek = appointmentRepository.countByUserIdAndStateInAndDateRange(
            userId, activeStates, weekStart, weekEnd);
        int maxPerWeek = getMaxAppointmentsPerUserPerWeek();

        if (appointmentsThisWeek >= maxPerWeek) {
            throw new ApiException(
                String.format("Has alcanzado el límite de %d turno(s) por semana. Ya tienes %d turno(s) esta semana",
                    maxPerWeek, appointmentsThisWeek),
                HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Mapea una entidad Appointment a un DTO AppointmentResponse.
     */
    private AppointmentResponse mapToResponse(Appointment appointment) {
        AppointmentResponse response = new AppointmentResponse();
        response.setId(appointment.getId());
        response.setUserId(appointment.getUserId());
        response.setDate(appointment.getAppointmentDate());
        response.setStartTime(appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setEndTime(appointment.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setDurationMinutes(appointment.getDurationMinutes());
        response.setState(appointment.getState());
        response.setCalendarConfigVersion(appointment.getCalendarConfigVersion());
        response.setExpiresAt(appointment.getExpiresAt());
        response.setConfirmedAt(appointment.getConfirmedAt());
        response.setPreviousAppointmentId(appointment.getPreviousAppointmentId());
        response.setOverridden(appointment.getOverridden());
        response.setOverrideJustification(appointment.getOverrideJustification());
        response.setReminderSent(appointment.getReminderSent());
        response.setCreatedAt(appointment.getCreatedAt());
        response.setUpdatedAt(appointment.getUpdatedAt());
        return response;
    }

    /**
     * Obtiene la fecha y hora actual en zona horaria GMT-3 (Argentina).
     */
    private LocalDate getTodayGMT3() {
        ZoneId gmtMinus3 = ZoneId.of("America/Argentina/Buenos_Aires");
        return ZonedDateTime.now(gmtMinus3).toLocalDate();
    }

    /**
     * Obtiene la fecha y hora actual en zona horaria GMT-3 (Argentina).
     */
    private LocalDateTime getNowGMT3() {
        ZoneId gmtMinus3 = ZoneId.of("America/Argentina/Buenos_Aires");
        return ZonedDateTime.now(gmtMinus3).toLocalDateTime();
    }

    @Override
    @Transactional
    public AppointmentResponse confirmAppointment(Long appointmentId, Long userId, String clientIp) {
        logger.info("Solicitud de confirmación de turno - ID: {}, Usuario: {}", appointmentId, userId);

        // 1. Buscar el turno
        Appointment appointment = appointmentRepository.findById(appointmentId)
            .orElseThrow(() -> new ApiException(
                "Turno no encontrado",
                HttpStatus.NOT_FOUND));

        // 2. Validar que el usuario sea el dueño del turno
        if (!appointment.getUserId().equals(userId)) {
            throw new ApiException(
                "No tienes permisos para confirmar este turno",
                HttpStatus.FORBIDDEN);
        }

        // 3. Validar que el turno esté en estado CREATED
        if (appointment.getState() != AppointmentState.CREATED) {
            if (appointment.getState() == AppointmentState.CONFIRMED) {
                throw new ApiException(
                    "El turno ya está confirmado",
                    HttpStatus.BAD_REQUEST);
            } else {
                throw new ApiException(
                    "Solo se pueden confirmar turnos en estado CREATED. Estado actual: " + appointment.getState(),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // 4. Validar que el turno no haya expirado
        if (appointment.isExpired()) {
            // Cambiar estado a EXPIRED si aún no lo está
            if (appointment.getState() != AppointmentState.EXPIRED) {
                appointment.expire();
                appointmentRepository.save(appointment);
                
                // Registrar en auditoría
                AppointmentHistory history = new AppointmentHistory(
                    appointment.getId(),
                    userId,
                    AppointmentState.CREATED,
                    AppointmentState.EXPIRED,
                    "EXPIRED",
                    "Turno expirado automáticamente",
                    clientIp
                );
                historyRepository.save(history);
            }
            
            throw new ApiException(
                "El turno ha expirado. Debe crear uno nuevo",
                HttpStatus.BAD_REQUEST);
        }

        // 5. Re-validar disponibilidad del slot
        try {
            var availability = configurationService.checkAvailability(appointment.getAppointmentDate());
            if (!Boolean.TRUE.equals(availability.getIsAvailable()) ||
                availability.getTimeRanges() == null ||
                availability.getTimeRanges().isEmpty()) {
                throw new ApiException(
                    "El slot ya no está disponible. El turno no puede confirmarse",
                    HttpStatus.CONFLICT);
            }

            // Validar que el slot específico sigue disponible
            boolean slotInRange = availability.getTimeRanges().stream()
                .anyMatch(range -> {
                    LocalTime rangeStart = LocalTime.parse(range.getStart(), DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime rangeEnd = LocalTime.parse(range.getEnd(), DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime slotEnd = appointment.getStartTime().plusMinutes(appointment.getDurationMinutes());
                    return (appointment.getStartTime().isAfter(rangeStart) || appointment.getStartTime().equals(rangeStart)) &&
                           (slotEnd.isBefore(rangeEnd) || slotEnd.equals(rangeEnd));
                });

            if (!slotInRange) {
                throw new ApiException(
                    "El slot ya no está disponible. El turno no puede confirmarse",
                    HttpStatus.CONFLICT);
            }

            // Validar que no haya otro turno confirmado o creado en el mismo slot (RESCHEDULED no ocupa)
            List<AppointmentState> occupyingStates = List.of(
                AppointmentState.CREATED,
                AppointmentState.CONFIRMED
            );

            List<Appointment> conflictingAppointments = appointmentRepository
                .findByDateAndStartTimeAndStateIn(
                    appointment.getAppointmentDate(),
                    appointment.getStartTime(),
                    occupyingStates
                );

            // Filtrar el turno actual
            conflictingAppointments = conflictingAppointments.stream()
                .filter(a -> !a.getId().equals(appointmentId))
                .toList();

            if (!conflictingAppointments.isEmpty()) {
                throw new ApiException(
                    "El slot ya no está disponible. El turno no puede confirmarse",
                    HttpStatus.CONFLICT);
            }

        } catch (ApiException e) {
            throw e;
        } catch (Exception e) {
            logger.error("Error al validar disponibilidad: {}", e.getMessage());
            throw new ApiException(
                "Error al validar disponibilidad del slot",
                HttpStatus.INTERNAL_SERVER_ERROR);
        }

        // 6. Confirmar el turno (actualizar estado, remover TTL, guardar timestamp)
        AppointmentState previousState = appointment.getState();
        appointment.confirm();
        Appointment confirmedAppointment = appointmentRepository.save(appointment);

        logger.info("Turno confirmado exitosamente - ID: {}, Usuario: {}", appointmentId, userId);

        // 7. Registrar en auditoría
        AppointmentHistory history = new AppointmentHistory(
            confirmedAppointment.getId(),
            userId,
            previousState,
            AppointmentState.CONFIRMED,
            "CONFIRMED",
            "Turno confirmado por el usuario",
            clientIp
        );
        historyRepository.save(history);

        // 8. Enviar email de notificación (asíncrono, no bloquea)
        try {
            User user = userRepository.findById(userId)
                .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
            emailService.sendAppointmentConfirmedEmail(user, confirmedAppointment);
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la confirmación del turno
            logger.error("Error al enviar email de turno confirmado - Turno ID: {}. Error: {}", 
                confirmedAppointment.getId(), e.getMessage(), e);
        }

        // 9. Enviar notificación WebSocket (asíncrono, no bloquea) - US-N001
        try {
            webSocketNotificationService.sendNotificationToUser(
                userId,
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_CONFIRMED,
                "Turno Confirmado",
                String.format("Tu turno para el %s a las %s ha sido confirmado.",
                    confirmedAppointment.getAppointmentDate(), confirmedAppointment.getStartTime()),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                confirmedAppointment.getId(),
                confirmedAppointment.getId()
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de turno confirmado - Turno ID: {}. Error: {}", 
                confirmedAppointment.getId(), e.getMessage(), e);
        }

        // 10. Enviar actualización de disponibilidad en tiempo real - Tiempo Real
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(confirmedAppointment.getAppointmentDate());
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                confirmedAppointment.getAppointmentDate(), e.getMessage(), e);
        }

        return mapToResponse(confirmedAppointment);
    }

    @Override
    @Transactional
    public AppointmentResponse cancelAppointment(Long appointmentId, CancelAppointmentRequest request,
                                                 Long userId, String clientIp) {
        logger.info("Solicitud de cancelación de turno - ID: {}, Usuario: {}, Motivo: {}",
            appointmentId, userId, request != null ? request.getReason() : "N/A");

        // 1. Buscar el turno
        Appointment appointment = appointmentRepository.findById(appointmentId)
            .orElseThrow(() -> new ApiException(
                "Turno no encontrado",
                HttpStatus.NOT_FOUND));

        // 2. Validar que el usuario sea el dueño del turno (o admin)
        // Por ahora solo validamos que sea el dueño, luego podemos agregar validación de admin
        if (!appointment.getUserId().equals(userId)) {
            throw new ApiException(
                "No tienes permisos para cancelar este turno",
                HttpStatus.FORBIDDEN);
        }

        // 3. Validar que el turno esté en estado CREATED o CONFIRMED
        if (appointment.getState() != AppointmentState.CREATED && 
            appointment.getState() != AppointmentState.CONFIRMED) {
            if (appointment.getState() == AppointmentState.CANCELLED) {
                throw new ApiException(
                    "El turno ya está cancelado",
                    HttpStatus.BAD_REQUEST);
            } else if (appointment.getState() == AppointmentState.COMPLETED) {
                throw new ApiException(
                    "No se pueden cancelar turnos completados",
                    HttpStatus.BAD_REQUEST);
            } else {
                throw new ApiException(
                    "Solo se pueden cancelar turnos en estado CREATED o CONFIRMED. Estado actual: " + appointment.getState(),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // 4. Validar que el turno sea futuro
        if (!appointment.isFuture()) {
            throw new ApiException(
                "No se pueden cancelar turnos pasados",
                HttpStatus.BAD_REQUEST);
        }

        // 5. Validar ventana mínima de cancelación (solo para turnos CONFIRMED)
        // Los turnos CREATED pueden cancelarse sin restricción de ventana mínima
        if (appointment.getState() == AppointmentState.CONFIRMED) {
            LocalDateTime appointmentDateTime = LocalDateTime.of(
                appointment.getAppointmentDate(), appointment.getStartTime());
            LocalDateTime now = getNowGMT3();
            long hoursUntilAppointment = ChronoUnit.HOURS.between(now, appointmentDateTime);
            int minCancellationWindow = getMinimumCancellationWindowHours();

            if (hoursUntilAppointment < minCancellationWindow) {
                throw new ApiException(
                    String.format("No se puede cancelar. La ventana mínima es de %d horas antes del turno. " +
                        "Fecha/hora del turno: %s %s, Tiempo restante: %d horas",
                        minCancellationWindow, appointment.getAppointmentDate(),
                        appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")), hoursUntilAppointment),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // 6. Cancelar el turno (actualizar estado, remover TTL)
        AppointmentState previousState = appointment.getState();
        appointment.cancel();
        Appointment cancelledAppointment = appointmentRepository.save(appointment);

        logger.info("Turno cancelado exitosamente - ID: {}, Usuario: {}", appointmentId, userId);

        // 7. Cancelar automáticamente solicitudes de reprogramación pendientes (si existen)
        // Implementa US-T013.1
        List<RescheduleRequest> pendingRequests = rescheduleRequestRepository
            .findByAppointmentIdAndState(appointmentId, RescheduleRequestState.PENDING_ADMIN_APPROVAL);

        for (RescheduleRequest pendingRequest : pendingRequests) {
            pendingRequest.cancel();
            rescheduleRequestRepository.save(pendingRequest);
            logger.info("Solicitud de reprogramación cancelada automáticamente - ID: {}, Turno ID: {}",
                pendingRequest.getId(), appointmentId);

            // Registrar en auditoría la cancelación automática de la solicitud
            AppointmentHistory rescheduleHistory = new AppointmentHistory(
                appointmentId,
                userId,
                AppointmentState.CANCELLED, // Estado del turno (ya cancelado)
                AppointmentState.CANCELLED, // Estado del turno no cambia
                "RESCHEDULE_REQUEST_AUTO_CANCELLED",
                String.format("Solicitud de reprogramación cancelada automáticamente al cancelar el turno. Solicitud ID: %d", 
                    pendingRequest.getId()),
                clientIp
            );
            historyRepository.save(rescheduleHistory);

            // 7.1. Notificar al admin cuando se cancela automáticamente una solicitud (FASE 9 - US-N002)
            try {
                webSocketNotificationService.sendNotificationToAdmins(
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_CANCELLED,
                    "Solicitud de Reprogramación Cancelada Automáticamente",
                    String.format("La solicitud de reprogramación ID %d fue cancelada automáticamente porque el usuario canceló el turno. Turno ID: %d",
                        pendingRequest.getId(), appointmentId),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                    pendingRequest.getId()
                );
            } catch (Exception e) {
                logger.error("Error al enviar notificación WebSocket a admins por cancelación automática - Solicitud ID: {}. Error: {}", 
                    pendingRequest.getId(), e.getMessage(), e);
            }
        }

        // 8. Registrar en auditoría la cancelación del turno
        String reason = request != null && request.getReason() != null && !request.getReason().isBlank()
            ? request.getReason()
            : "Sin motivo especificado";

        AppointmentHistory history = new AppointmentHistory(
            cancelledAppointment.getId(),
            userId,
            previousState,
            AppointmentState.CANCELLED,
            "CANCELLED",
            reason,
            clientIp
        );
        historyRepository.save(history);

        // 9. Enviar email de notificación (asíncrono, no bloquea)
        try {
            User user = userRepository.findById(userId)
                .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
            emailService.sendAppointmentCancelledEmail(user, cancelledAppointment, reason, false);
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la cancelación del turno
            logger.error("Error al enviar email de turno cancelado - Turno ID: {}. Error: {}", 
                cancelledAppointment.getId(), e.getMessage(), e);
        }

        // 10. Enviar notificación WebSocket al usuario (asíncrono, no bloquea) - US-N001
        try {
            webSocketNotificationService.sendNotificationToUser(
                userId,
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_CANCELLED,
                "Turno Cancelado",
                String.format("Tu turno para el %s a las %s ha sido cancelado. Motivo: %s",
                    cancelledAppointment.getAppointmentDate(), cancelledAppointment.getStartTime(), reason),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                cancelledAppointment.getId(),
                cancelledAppointment.getId()
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de turno cancelado - Turno ID: {}. Error: {}", 
                cancelledAppointment.getId(), e.getMessage(), e);
        }

        // 10.1. Notificar a admins cuando un usuario cancela su turno
        try {
            User user = userRepository.findById(userId)
                .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
            webSocketNotificationService.sendNotificationToAdmins(
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_CANCELLED,
                "Turno cancelado por usuario",
                String.format("%s %s canceló su turno para el %s a las %s. Motivo: %s",
                    user.getFirstName(), user.getLastName(), cancelledAppointment.getAppointmentDate(),
                    cancelledAppointment.getStartTime(), reason),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                cancelledAppointment.getId()
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket a admins por cancelación de usuario - Turno ID: {}. Error: {}",
                cancelledAppointment.getId(), e.getMessage(), e);
        }

        // 11. Enviar actualización de disponibilidad en tiempo real - Tiempo Real
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(cancelledAppointment.getAppointmentDate());
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                cancelledAppointment.getAppointmentDate(), e.getMessage(), e);
        }

        return mapToResponse(cancelledAppointment);
    }

    @Override
    @Transactional
    public RescheduleRequestResponse requestReschedule(Long appointmentId, CreateRescheduleRequestRequest request,
                                                      Long userId, String clientIp) {
        logger.info("Solicitud de reprogramación - Turno ID: {}, Usuario: {}, Nueva fecha: {}, Nueva hora: {}",
            appointmentId, userId, request.getNewDate(), request.getNewStartTime());

        // 1. Buscar el turno
        Appointment appointment = appointmentRepository.findById(appointmentId)
            .orElseThrow(() -> new ApiException(
                "Turno no encontrado",
                HttpStatus.NOT_FOUND));

        // 2. Validar que el usuario sea el dueño del turno
        if (!appointment.getUserId().equals(userId)) {
            throw new ApiException(
                "No tienes permisos para reprogramar este turno",
                HttpStatus.FORBIDDEN);
        }

        // 3. Validar que el turno esté en estado CONFIRMED
        if (appointment.getState() != AppointmentState.CONFIRMED) {
            throw new ApiException(
                "Solo se pueden reprogramar turnos confirmados. Estado actual: " + appointment.getState(),
                HttpStatus.BAD_REQUEST);
        }

        // 4. Validar que no exista otra solicitud pendiente para el mismo turno
        List<RescheduleRequest> pendingRequests = rescheduleRequestRepository
            .findByAppointmentIdAndState(appointmentId, RescheduleRequestState.PENDING_ADMIN_APPROVAL);

        if (!pendingRequests.isEmpty()) {
            throw new ApiException(
                "Ya existe una solicitud de reprogramación pendiente para este turno",
                HttpStatus.CONFLICT);
        }

        // 5. Validar que el nuevo slot esté disponible y cumpla todas las reglas
        LocalTime requestedStartTime = LocalTime.parse(request.getNewStartTime(), DateTimeFormatter.ofPattern("HH:mm"));
        validateSlotAvailability(request.getNewDate(), requestedStartTime, 
            appointment.getDurationMinutes());

        // 6. Obtener configuración activa para obtener duración
        CalendarConfiguration activeConfig = configurationRepository.findByActiveTrue()
            .orElseThrow(() -> new ApiException(
                "No existe una configuración activa. El sistema no está configurado.",
                HttpStatus.SERVICE_UNAVAILABLE));

        Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
        if (appointmentDurationMinutes == null) {
            throw new ApiException(
                "No hay duración de turnos configurada. Debe configurar la duración antes de solicitar reprogramación.",
                HttpStatus.BAD_REQUEST);
        }

        // 7. Validar que la nueva fecha no sea pasada
        LocalDate today = getTodayGMT3();
        if (request.getNewDate().isBefore(today)) {
            throw new ApiException(
                "No se pueden solicitar reprogramaciones para fechas pasadas. Fecha solicitada: " + request.getNewDate(),
                HttpStatus.BAD_REQUEST);
        }

        // 8. Validar anticipación mínima (usando política dinámica)
        LocalDateTime requestedDateTime = LocalDateTime.of(request.getNewDate(), requestedStartTime);
        LocalDateTime now = getNowGMT3();
        long hoursUntilRequested = ChronoUnit.HOURS.between(now, requestedDateTime);
        int minAnticipation = getMinimumAnticipationHours();

        if (hoursUntilRequested < minAnticipation) {
            throw new ApiException(
                String.format("No se pueden solicitar reprogramaciones con menos de %d horas de anticipación. " +
                    "Fecha/hora solicitada: %s %s, Anticipación requerida: %d horas",
                    minAnticipation, request.getNewDate(), request.getNewStartTime(), minAnticipation),
                HttpStatus.BAD_REQUEST);
        }

        // 8.1. Validar anticipación máxima (usando política dinámica)
        long daysUntilRequested = ChronoUnit.DAYS.between(today, request.getNewDate());
        int maxAdvanceDays = getMaximumAdvanceDays();

        if (daysUntilRequested > maxAdvanceDays) {
            throw new ApiException(
                String.format("No se pueden solicitar reprogramaciones con más de %d días de anticipación. " +
                    "Fecha solicitada: %s, Máximo permitido: %d días desde hoy (%s)",
                    maxAdvanceDays, request.getNewDate(), maxAdvanceDays, today.plusDays(maxAdvanceDays)),
                HttpStatus.BAD_REQUEST);
        }

        // 9. Crear solicitud de reprogramación
        RescheduleRequest rescheduleRequest = new RescheduleRequest(
            appointmentId,
            userId,
            appointment.getAppointmentDate(),
            appointment.getStartTime(),
            request.getNewDate(),
            requestedStartTime,
            request.getReason()
        );

        rescheduleRequest = rescheduleRequestRepository.save(rescheduleRequest);

        logger.info("Solicitud de reprogramación creada exitosamente - ID: {}, Turno ID: {}, Usuario: {}",
            rescheduleRequest.getId(), appointmentId, userId);

        // 10. Registrar en auditoría (en AppointmentHistory)
        AppointmentHistory history = new AppointmentHistory(
            appointment.getId(),
            userId,
            AppointmentState.CONFIRMED, // Estado anterior (permanece igual)
            AppointmentState.CONFIRMED, // Estado nuevo (no cambia aún)
            "RESCHEDULE_REQUESTED",
            String.format("Solicitud de reprogramación creada. Nueva fecha/hora: %s %s. Motivo: %s",
                request.getNewDate(), request.getNewStartTime(),
                request.getReason() != null && !request.getReason().isBlank() ? request.getReason() : "Sin motivo"),
            clientIp
        );
        historyRepository.save(history);

        // 11. Crear notificación en BD para cada admin (síncrono, así siempre aparece en la campanita). Sin email. No configurable.
        try {
            List<com.ak4n1.turn_management.feature.auth.domain.User> admins = userRepository.findAllAdmins();
            com.ak4n1.turn_management.feature.auth.domain.User requestingUser = userService.findById(userId)
                .orElse(null);
            String userName = requestingUser != null
                ? String.format("%s %s", requestingUser.getFirstName(), requestingUser.getLastName())
                : "Usuario";
            String title = "Nueva solicitud de reprogramación";
            String message = String.format(
                "%s solicita reprogramar turno del %s a las %s al %s a las %s",
                userName,
                appointment.getAppointmentDate(),
                appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")),
                request.getNewDate(),
                request.getNewStartTime()
            );
            for (com.ak4n1.turn_management.feature.auth.domain.User admin : admins) {
                notificationService.createNotification(
                    NotificationType.RESCHEDULE_REQUEST_PENDING,
                    admin.getId(),
                    title,
                    message,
                    RelatedEntityType.RESCHEDULE_REQUEST,
                    rescheduleRequest.getId()
                );
            }
            logger.info("Notificaciones creadas para {} administradores - Solicitud ID: {}", admins.size(), rescheduleRequest.getId());
        } catch (Exception e) {
            logger.warn("Error al crear notificaciones para administradores: {}", e.getMessage(), e);
        }

        // 12. Enviar WebSocket a admins (actualiza badge en tiempo real). Asíncrono.
        try {
            com.ak4n1.turn_management.feature.auth.domain.User requestingUser = userService.findById(userId)
                .orElse(null);
            String userName = requestingUser != null
                ? String.format("%s %s", requestingUser.getFirstName(), requestingUser.getLastName())
                : "Usuario";
            webSocketNotificationService.sendNotificationToAdmins(
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_PENDING,
                "Nueva Solicitud de Reprogramación",
                String.format("%s solicita reprogramar turno del %s a las %s al %s a las %s. Motivo: %s",
                    userName,
                    appointment.getAppointmentDate(), appointment.getStartTime(),
                    request.getNewDate(), request.getNewStartTime(),
                    request.getReason() != null ? request.getReason() : "Sin motivo"),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                rescheduleRequest.getId()
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket a admins - Solicitud ID: {}. Error: {}", 
                rescheduleRequest.getId(), e.getMessage(), e);
        }

        return mapToRescheduleRequestResponse(rescheduleRequest);
    }

    /**
     * Mapea una entidad RescheduleRequest a un DTO RescheduleRequestResponse.
     */
    private RescheduleRequestResponse mapToRescheduleRequestResponse(RescheduleRequest request) {
        RescheduleRequestResponse response = new RescheduleRequestResponse();
        response.setId(request.getId());
        response.setAppointmentId(request.getAppointmentId());
        response.setUserId(request.getUserId());
        response.setCurrentDate(request.getCurrentDate());
        response.setCurrentStartTime(request.getCurrentStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setRequestedDate(request.getRequestedDate());
        response.setRequestedStartTime(request.getRequestedStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setReason(request.getReason());
        response.setState(request.getState());
        response.setRejectionReason(request.getRejectionReason());
        response.setExpirationReason(request.getExpirationReason());
        response.setProcessedByAdminId(request.getProcessedByAdminId());
        response.setCreatedAt(request.getCreatedAt());
        response.setUpdatedAt(request.getUpdatedAt());
        response.setProcessedAt(request.getProcessedAt());
        return response;
    }

    /**
     * Obtiene todas las solicitudes de reprogramación del usuario autenticado.
     * 
     * Implementa US-T014.1:
     * - Filtrado por usuario (solo solicitudes propias)
     * - Filtrado opcional por estado
     * - Ordenamiento por fecha de creación (más recientes primero)
     * - Resumen con totales por estado
     */
    @Override
    @Transactional(readOnly = true)
    public MyRescheduleRequestsResponse getMyRescheduleRequests(Long userId, RescheduleRequestState state) {
        logger.info("Consultando solicitudes de reprogramación - Usuario: {}, Estado: {}", 
            userId, state != null ? state : "TODOS");

        // 1. Obtener todas las solicitudes del usuario
        List<RescheduleRequest> allRequests = rescheduleRequestRepository.findByUserIdOrderByCreatedAtDesc(userId);

        // 2. Filtrar por estado si se especificó
        List<RescheduleRequest> filteredRequests = state != null 
            ? allRequests.stream()
                .filter(r -> r.getState() == state)
                .toList()
            : allRequests;

        // 3. Mapear a DTOs
        List<RescheduleRequestResponse> requestResponses = filteredRequests.stream()
            .map(this::mapToRescheduleRequestResponse)
            .toList();

        // 4. Calcular resumen por estado
        int pending = (int) allRequests.stream()
            .filter(r -> r.getState() == RescheduleRequestState.PENDING_ADMIN_APPROVAL)
            .count();
        
        int approved = (int) allRequests.stream()
            .filter(r -> r.getState() == RescheduleRequestState.APPROVED)
            .count();
        
        int rejected = (int) allRequests.stream()
            .filter(r -> r.getState() == RescheduleRequestState.REJECTED)
            .count();
        
        int expired = (int) allRequests.stream()
            .filter(r -> r.getState() == RescheduleRequestState.EXPIRED)
            .count();
        
        int cancelled = (int) allRequests.stream()
            .filter(r -> r.getState() == RescheduleRequestState.CANCELLED)
            .count();

        int total = allRequests.size();

        logger.info("Solicitudes encontradas - Total: {}, Pendientes: {}, Aprobadas: {}, Rechazadas: {}, Expiradas: {}, Canceladas: {}",
            total, pending, approved, rejected, expired, cancelled);

        return new MyRescheduleRequestsResponse(
            requestResponses,
            total,
            pending,
            approved,
            rejected,
            expired,
            cancelled
        );
    }

    /**
     * Cancela una solicitud de reprogramación del usuario autenticado.
     * 
     * Implementa US-T014.2:
     * - Validación de que la solicitud esté en estado PENDING_ADMIN_APPROVAL
     * - Validación de que el usuario sea el dueño de la solicitud
     * - Actualización de estado a CANCELLED
     * - El turno original permanece sin cambios
     * - Auditoría completa
     * - Notificación WebSocket al admin (FASE 9 - US-N002) - Implementado
     */
    @Override
    @Transactional
    public RescheduleRequestResponse cancelMyRescheduleRequest(Long rescheduleRequestId, Long userId, String clientIp) {
        logger.info("Solicitud de cancelación de reprogramación - ID: {}, Usuario: {}", rescheduleRequestId, userId);

        // 1. Buscar la solicitud
        RescheduleRequest rescheduleRequest = rescheduleRequestRepository.findById(rescheduleRequestId)
            .orElseThrow(() -> {
                logger.warn("Solicitud de reprogramación no encontrada - ID: {}, Usuario: {}", rescheduleRequestId, userId);
                return new ApiException("Solicitud de reprogramación no encontrada", HttpStatus.NOT_FOUND);
            });

        // 2. Validar que el usuario sea el dueño
        if (!rescheduleRequest.getUserId().equals(userId)) {
            logger.warn("Usuario {} intentó cancelar solicitud de otro usuario - Solicitud ID: {}, Dueño: {}",
                userId, rescheduleRequestId, rescheduleRequest.getUserId());
            throw new ApiException(
                "No tienes permiso para cancelar esta solicitud",
                HttpStatus.FORBIDDEN);
        }

        // 3. Validar que la solicitud esté en estado PENDING_ADMIN_APPROVAL
        if (rescheduleRequest.getState() != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            logger.warn("Intento de cancelar solicitud ya procesada - ID: {}, Estado actual: {}",
                rescheduleRequestId, rescheduleRequest.getState());
            throw new ApiException(
                String.format("No se puede cancelar una solicitud ya procesada. Estado actual: %s", rescheduleRequest.getState()),
                HttpStatus.BAD_REQUEST);
        }

        // 4. Cancelar la solicitud
        try {
            rescheduleRequest.cancelByUser();
        } catch (IllegalStateException e) {
            logger.warn("Error al cancelar solicitud - ID: {}, Error: {}", rescheduleRequestId, e.getMessage());
            throw new ApiException(
                String.format("No se puede cancelar la solicitud: %s", e.getMessage()),
                HttpStatus.BAD_REQUEST);
        }

        rescheduleRequest = rescheduleRequestRepository.save(rescheduleRequest);

        logger.info("Solicitud de reprogramación cancelada exitosamente - ID: {}, Usuario: {}",
            rescheduleRequestId, userId);

        // 5. Registrar en auditoría (en AppointmentHistory)
        Appointment appointment = appointmentRepository.findById(rescheduleRequest.getAppointmentId())
            .orElse(null);
        
        if (appointment != null) {
            AppointmentHistory history = new AppointmentHistory(
                appointment.getId(),
                userId,
                appointment.getState(), // Estado anterior (no cambia)
                appointment.getState(), // Estado nuevo (no cambia)
                "RESCHEDULE_REQUEST_CANCELLED",
                String.format("Solicitud de reprogramación cancelada por el usuario. Solicitud ID: %d", rescheduleRequestId),
                clientIp
            );
            historyRepository.save(history);
        }

        // 6. Enviar notificación WebSocket a admins (asíncrono, no bloquea) - US-N002
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_CANCELLED,
                "Solicitud de Reprogramación Cancelada",
                String.format("El usuario canceló su solicitud de reprogramación. Solicitud ID: %d", rescheduleRequestId),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                rescheduleRequestId
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket a admins - Solicitud ID: {}. Error: {}", 
                rescheduleRequestId, e.getMessage(), e);
        }

        return mapToRescheduleRequestResponse(rescheduleRequest);
    }

    /**
     * Obtiene todas las solicitudes de reprogramación con filtros (solo admin).
     * 
     * Implementa US-T014.3:
     * - Solo accesible por admin
     * - Filtrado por estado, usuario, rango de fechas
     * - Paginación obligatoria
     * - Ordenamiento por fecha de creación (más recientes primero)
     * - Incluye información del usuario (email, nombre)
     */
    @Override
    @Transactional(readOnly = true)
    public AdminRescheduleRequestsResponse getAllRescheduleRequests(
            RescheduleRequestState state,
            Long userId,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            Pageable pageable) {
        logger.info("Consultando todas las solicitudes de reprogramación (admin) - Estado: {}, Usuario: {}, Desde: {}, Hasta: {}, Página: {}, Tamaño: {}",
            state, userId, fromDate, toDate, pageable.getPageNumber(), pageable.getPageSize());

        // Convertir LocalDate a LocalDateTime para las consultas
        LocalDateTime fromDateTime = fromDate != null ? fromDate.atStartOfDay() : null;
        LocalDateTime toDateTime = toDate != null ? toDate.atTime(23, 59, 59) : null;

        // Consultar con filtros y paginación
        Page<RescheduleRequest> page = rescheduleRequestRepository.findAllWithFilters(
            state, userId, fromDateTime, toDateTime, pageable);

        // Mapear a DTOs con información del usuario
        List<AdminRescheduleRequestResponse> requestResponses = page.getContent().stream()
            .map(this::mapToAdminRescheduleRequestResponse)
            .toList();

        // Calcular total de páginas
        int totalPages = page.getTotalPages();

        logger.info("Solicitudes encontradas - Total: {}, Página: {}, Tamaño: {}, Total páginas: {}",
            page.getTotalElements(), pageable.getPageNumber(), pageable.getPageSize(), totalPages);

        return new AdminRescheduleRequestsResponse(
            requestResponses,
            page.getTotalElements(),
            pageable.getPageNumber(),
            pageable.getPageSize(),
            totalPages
        );
    }

    /**
     * Mapea una entidad RescheduleRequest a un DTO AdminRescheduleRequestResponse.
     * Incluye información del usuario.
     */
    private AdminRescheduleRequestResponse mapToAdminRescheduleRequestResponse(RescheduleRequest request) {
        AdminRescheduleRequestResponse response = new AdminRescheduleRequestResponse();
        response.setId(request.getId());
        response.setAppointmentId(request.getAppointmentId());
        response.setUserId(request.getUserId());
        response.setCurrentDate(request.getCurrentDate());
        response.setCurrentStartTime(request.getCurrentStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setRequestedDate(request.getRequestedDate());
        response.setRequestedStartTime(request.getRequestedStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setReason(request.getReason());
        response.setState(request.getState());
        response.setRejectionReason(request.getRejectionReason());
        response.setExpirationReason(request.getExpirationReason());
        response.setProcessedByAdminId(request.getProcessedByAdminId());
        response.setCreatedAt(request.getCreatedAt());
        response.setUpdatedAt(request.getUpdatedAt());
        response.setProcessedAt(request.getProcessedAt());

        // Obtener información del usuario
        User user = userService.findById(request.getUserId()).orElse(null);
        if (user != null) {
            response.setUserEmail(user.getEmail());
            response.setUserFirstName(user.getFirstName());
            response.setUserLastName(user.getLastName());
        }

        return response;
    }

    /**
     * Aprueba una solicitud de reprogramación (solo admin).
     * 
     * Implementa US-T014:
     * - Solo accesible por admin
     * - Validación de que la solicitud esté en estado PENDING_ADMIN_APPROVAL
     * - Validación de que el turno original siga existiendo y esté en CONFIRMED
     * - Validación de que el slot solicitado siga disponible
     * - Turno original pasa a estado RESCHEDULED
     * - Nuevo turno creado en estado CONFIRMED
     * - Solicitud pasa a estado APPROVED
     * - Operación transaccional atómica
     * - Email y WebSocket al usuario
     * - Actualización en tiempo real de disponibilidad (ambas fechas)
     */
    @Override
    @Transactional
    public RescheduleRequestResponse approveRescheduleRequest(Long rescheduleRequestId, Long adminUserId, String clientIp) {
        logger.info("Aprobación de solicitud de reprogramación - Solicitud ID: {}, Admin: {}", rescheduleRequestId, adminUserId);

        // 1. Buscar la solicitud con lock pesimista para evitar condiciones de carrera
        RescheduleRequest rescheduleRequest = rescheduleRequestRepository.findByIdWithPessimisticLock(rescheduleRequestId)
            .orElseThrow(() -> {
                logger.warn("Solicitud de reprogramación no encontrada - ID: {}", rescheduleRequestId);
                return new ApiException("Solicitud de reprogramación no encontrada", HttpStatus.NOT_FOUND);
            });

        // 2. Validar que la solicitud esté en estado PENDING_ADMIN_APPROVAL
        if (rescheduleRequest.getState() != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            logger.warn("Intento de aprobar solicitud ya procesada - ID: {}, Estado actual: {}", 
                rescheduleRequestId, rescheduleRequest.getState());
            throw new ApiException(
                String.format("No se puede aprobar una solicitud ya procesada. Estado actual: %s", rescheduleRequest.getState()),
                HttpStatus.BAD_REQUEST);
        }

        // 3. Buscar el turno original con lock pesimista para evitar condiciones de carrera
        Appointment originalAppointment = appointmentRepository.findByIdWithPessimisticLock(rescheduleRequest.getAppointmentId())
            .orElseThrow(() -> {
                logger.warn("Turno original no encontrado para solicitud de reprogramación - Solicitud ID: {}, Turno ID: {}", 
                    rescheduleRequestId, rescheduleRequest.getAppointmentId());
                return new ApiException(
                    "El turno original no existe. La solicitud no puede ser aprobada.",
                    HttpStatus.BAD_REQUEST);
            });

        // 4. Validar que el turno original esté en estado CONFIRMED
        if (originalAppointment.getState() != AppointmentState.CONFIRMED) {
            logger.warn("Intento de aprobar solicitud para turno no confirmado - Solicitud ID: {}, Turno ID: {}, Estado: {}", 
                rescheduleRequestId, originalAppointment.getId(), originalAppointment.getState());
            throw new ApiException(
                String.format("No se puede aprobar la solicitud. El turno original está en estado %s. Solo se pueden aprobar solicitudes para turnos confirmados.", 
                    originalAppointment.getState()),
                HttpStatus.BAD_REQUEST);
        }

        // 5. Validar que el nuevo slot siga disponible (revalidar)
        validateSlotAvailability(
            rescheduleRequest.getRequestedDate(), 
            rescheduleRequest.getRequestedStartTime(), 
            originalAppointment.getDurationMinutes());

        // 6. Obtener configuración activa
        CalendarConfiguration activeConfig = configurationRepository.findByActiveTrue()
            .orElseThrow(() -> new ApiException(
                "No existe una configuración activa. El sistema no está configurado.",
                HttpStatus.SERVICE_UNAVAILABLE));

        Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
        if (appointmentDurationMinutes == null) {
            throw new ApiException(
                "No hay duración de turnos configurada. Debe configurar la duración antes de aprobar reprogramaciones.",
                HttpStatus.BAD_REQUEST);
        }

        // 7. Calcular hora de fin del nuevo turno
        LocalTime newStartTime = rescheduleRequest.getRequestedStartTime();
        LocalTime newEndTime = newStartTime.plusMinutes(appointmentDurationMinutes);

        // 8. Guardar estado anterior del turno original
        AppointmentState previousState = originalAppointment.getState();

        // 9. Actualizar turno original a estado RESCHEDULED
        originalAppointment.setState(AppointmentState.RESCHEDULED);
        originalAppointment.setUpdatedAt(LocalDateTime.now());
        Appointment updatedOriginal = appointmentRepository.save(originalAppointment);
        logger.info("Turno original actualizado a RESCHEDULED - ID: {}", updatedOriginal.getId());

        // 10. Crear nuevo turno en estado CONFIRMED
        Appointment newAppointment = new Appointment(
            originalAppointment.getUserId(),
            rescheduleRequest.getRequestedDate(),
            newStartTime,
            newEndTime,
            appointmentDurationMinutes,
            AppointmentState.CONFIRMED,
            activeConfig.getVersion(),
            null, // expiresAt es null para turnos confirmados
            null, // idempotencyKey no aplica para reprogramaciones
            adminUserId // Aprobado por el admin
        );
        newAppointment.setPreviousAppointmentId(originalAppointment.getId());
        newAppointment.setConfirmedAt(LocalDateTime.now());

        Appointment savedNewAppointment = appointmentRepository.save(newAppointment);
        logger.info("Nuevo turno creado por aprobación de solicitud - ID: {}, Usuario: {}, Fecha: {}, Hora: {}",
            savedNewAppointment.getId(), savedNewAppointment.getUserId(), 
            savedNewAppointment.getAppointmentDate(), savedNewAppointment.getStartTime());

        // 11. Aprobar la solicitud
        try {
            rescheduleRequest.approve(adminUserId);
        } catch (IllegalStateException e) {
            logger.warn("Error al aprobar solicitud - ID: {}, Error: {}", rescheduleRequestId, e.getMessage());
            throw new ApiException(
                String.format("No se puede aprobar la solicitud: %s", e.getMessage()),
                HttpStatus.BAD_REQUEST);
        }
        RescheduleRequest savedRescheduleRequest = rescheduleRequestRepository.save(rescheduleRequest);

        // 12. Registrar auditoría para el turno original (cambio a RESCHEDULED)
        AppointmentHistory originalHistory = new AppointmentHistory(
            originalAppointment.getId(),
            adminUserId,
            previousState,
            AppointmentState.RESCHEDULED,
            "RESCHEDULED_BY_APPROVAL",
            String.format("Reprogramación aprobada por administrador. Nueva fecha/hora: %s %s. Solicitud ID: %d. Motivo original: %s. Nuevo turno ID: %d",
                rescheduleRequest.getRequestedDate(), rescheduleRequest.getRequestedStartTime(), 
                rescheduleRequestId, rescheduleRequest.getReason(), savedNewAppointment.getId()),
            clientIp
        );
        historyRepository.save(originalHistory);

        // 13. Registrar auditoría para el nuevo turno (creación como CONFIRMED)
        AppointmentHistory newHistory = new AppointmentHistory(
            savedNewAppointment.getId(),
            adminUserId,
            null, // previousState es null para creación
            AppointmentState.CONFIRMED,
            "CREATED_BY_RESCHEDULE_APPROVAL",
            String.format("Turno creado por aprobación de solicitud de reprogramación. Turno original ID: %d. Solicitud ID: %d. Motivo: %s",
                originalAppointment.getId(), rescheduleRequestId, rescheduleRequest.getReason()),
            clientIp
        );
        historyRepository.save(newHistory);

        // 14. Enviar email de notificación (asíncrono, no bloquea)
        try {
            User user = userRepository.findById(originalAppointment.getUserId())
                .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
            emailService.sendAppointmentRescheduledEmail(user, originalAppointment, savedNewAppointment, rescheduleRequest.getReason());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la aprobación
            logger.error("Error al enviar email de aprobación de reprogramación - Solicitud ID: {}. Error: {}", 
                rescheduleRequestId, e.getMessage(), e);
        }

        // 15. Enviar notificación WebSocket al usuario (asíncrono, no bloquea) - US-N001
        try {
            webSocketNotificationService.sendNotificationToUser(
                originalAppointment.getUserId(),
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_APPROVED,
                "Solicitud de Reprogramación Aprobada",
                String.format("Tu solicitud de reprogramación ha sido aprobada. Tu turno del %s a las %s ha sido reprogramado al %s a las %s.",
                    originalAppointment.getAppointmentDate(), originalAppointment.getStartTime(),
                    savedNewAppointment.getAppointmentDate(), savedNewAppointment.getStartTime()),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                rescheduleRequestId,
                savedNewAppointment.getId()
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de aprobación - Solicitud ID: {}. Error: {}", 
                rescheduleRequestId, e.getMessage(), e);
        }

        // 16. Enviar notificación WebSocket a admins (asíncrono, no bloquea) - US-N002
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_APPROVED,
                "Solicitud de Reprogramación Aprobada",
                String.format("Solicitud de reprogramación ID %d aprobada. Usuario: %d, Turno original: %d, Nuevo turno: %d",
                    rescheduleRequestId, originalAppointment.getUserId(), originalAppointment.getId(), savedNewAppointment.getId()),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                rescheduleRequestId
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket a admins - Solicitud ID: {}. Error: {}", 
                rescheduleRequestId, e.getMessage(), e);
        }

        // 17. Enviar actualización de disponibilidad en tiempo real (ambas fechas) - Tiempo Real
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(originalAppointment.getAppointmentDate());
            webSocketNotificationService.broadcastAvailabilityUpdate(savedNewAppointment.getAppointmentDate());
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fechas: {}, {}. Error: {}", 
                originalAppointment.getAppointmentDate(), savedNewAppointment.getAppointmentDate(), e.getMessage(), e);
        }

        logger.info("Solicitud de reprogramación aprobada exitosamente - Solicitud ID: {}, Turno original ID: {}, Nuevo turno ID: {}", 
            rescheduleRequestId, originalAppointment.getId(), savedNewAppointment.getId());

        return mapToRescheduleRequestResponse(savedRescheduleRequest);
    }

    /**
     * Rechaza una solicitud de reprogramación (solo admin).
     * 
     * Implementa US-T014:
     * - Solo accesible por admin
     * - Validación de que la solicitud esté en estado PENDING_ADMIN_APPROVAL
     * - Solicitud pasa a estado REJECTED
     * - Motivo de rechazo guardado
     * - Turno original permanece sin cambios
     * - Email y WebSocket al usuario
     */
    @Override
    @Transactional
    public RescheduleRequestResponse rejectRescheduleRequest(Long rescheduleRequestId, 
                                                             com.ak4n1.turn_management.feature.appointment.dto.request.RejectRescheduleRequestRequest request,
                                                             Long adminUserId, String clientIp) {
        logger.info("Rechazo de solicitud de reprogramación - Solicitud ID: {}, Admin: {}, Motivo: {}", 
            rescheduleRequestId, adminUserId, request != null ? request.getRejectionReason() : "N/A");

        // 1. Buscar la solicitud con lock pesimista para evitar condiciones de carrera
        RescheduleRequest rescheduleRequest = rescheduleRequestRepository.findByIdWithPessimisticLock(rescheduleRequestId)
            .orElseThrow(() -> {
                logger.warn("Solicitud de reprogramación no encontrada - ID: {}", rescheduleRequestId);
                return new ApiException("Solicitud de reprogramación no encontrada", HttpStatus.NOT_FOUND);
            });

        // 2. Validar que la solicitud esté en estado PENDING_ADMIN_APPROVAL
        if (rescheduleRequest.getState() != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            logger.warn("Intento de rechazar solicitud ya procesada - ID: {}, Estado actual: {}", 
                rescheduleRequestId, rescheduleRequest.getState());
            throw new ApiException(
                String.format("No se puede rechazar una solicitud ya procesada. Estado actual: %s", rescheduleRequest.getState()),
                HttpStatus.BAD_REQUEST);
        }

        // 3. Buscar el turno original (para auditoría y notificaciones)
        Appointment originalAppointment = appointmentRepository.findById(rescheduleRequest.getAppointmentId())
            .orElse(null); // No es crítico si no existe, pero preferible tenerlo

        // 4. Rechazar la solicitud
        String rejectionReason = request != null && request.getRejectionReason() != null && !request.getRejectionReason().isBlank()
            ? request.getRejectionReason()
            : "Sin motivo especificado";

        try {
            rescheduleRequest.reject(adminUserId, rejectionReason);
        } catch (IllegalStateException e) {
            logger.warn("Error al rechazar solicitud - ID: {}, Error: {}", rescheduleRequestId, e.getMessage());
            throw new ApiException(
                String.format("No se puede rechazar la solicitud: %s", e.getMessage()),
                HttpStatus.BAD_REQUEST);
        }
        RescheduleRequest savedRescheduleRequest = rescheduleRequestRepository.save(rescheduleRequest);

        logger.info("Solicitud de reprogramación rechazada exitosamente - Solicitud ID: {}, Motivo: {}", 
            rescheduleRequestId, rejectionReason);

        // 5. Registrar auditoría (en AppointmentHistory si el turno existe)
        if (originalAppointment != null) {
            AppointmentHistory history = new AppointmentHistory(
                originalAppointment.getId(),
                adminUserId,
                originalAppointment.getState(), // Estado anterior (no cambia)
                originalAppointment.getState(), // Estado nuevo (no cambia)
                "RESCHEDULE_REQUEST_REJECTED",
                String.format("Solicitud de reprogramación rechazada por administrador. Solicitud ID: %d. Motivo: %s",
                    rescheduleRequestId, rejectionReason),
                clientIp
            );
            historyRepository.save(history);
        }

        // 6. Enviar email de notificación (asíncrono, no bloquea)
        if (originalAppointment != null) {
            try {
                User user = userRepository.findById(originalAppointment.getUserId())
                    .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
                emailService.sendRescheduleRejectedEmail(user, originalAppointment, rejectionReason);
            } catch (Exception e) {
                // NO lanzar excepción - el email no debe bloquear el rechazo
                logger.error("Error al enviar email de rechazo de reprogramación - Solicitud ID: {}. Error: {}", 
                    rescheduleRequestId, e.getMessage(), e);
            }
        }

        // 7. Enviar notificación WebSocket al usuario (asíncrono, no bloquea) - US-N001
        if (originalAppointment != null) {
            try {
                webSocketNotificationService.sendNotificationToUser(
                    originalAppointment.getUserId(),
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_REJECTED,
                    "Solicitud de Reprogramación Rechazada",
                    String.format("Tu solicitud de reprogramación ha sido rechazada. Tu turno del %s a las %s permanece sin cambios. Motivo: %s",
                        originalAppointment.getAppointmentDate(), originalAppointment.getStartTime(), rejectionReason),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                    rescheduleRequestId,
                    originalAppointment.getId()
                );
            } catch (Exception e) {
                logger.error("Error al enviar notificación WebSocket de rechazo - Solicitud ID: {}. Error: {}", 
                    rescheduleRequestId, e.getMessage(), e);
            }
        }

        // 8. Enviar notificación WebSocket a admins (asíncrono, no bloquea) - US-N002
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                com.ak4n1.turn_management.feature.notification.domain.NotificationType.RESCHEDULE_REQUEST_REJECTED,
                "Solicitud de Reprogramación Rechazada",
                String.format("Solicitud de reprogramación ID %d rechazada. Usuario: %d, Motivo: %s",
                    rescheduleRequestId, rescheduleRequest.getUserId(), rejectionReason),
                com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.RESCHEDULE_REQUEST,
                rescheduleRequestId
            );
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket a admins - Solicitud ID: {}. Error: {}", 
                rescheduleRequestId, e.getMessage(), e);
        }

        return mapToRescheduleRequestResponse(savedRescheduleRequest);
    }

    /**
     * Marca un turno como no-show (ausente) - Solo admin.
     * 
     * Implementa US-T016:
     * - Solo accesible por admin
     * - Validación de que el turno esté en estado CONFIRMED
     * - Validación de que el horario del turno ya pasó
     * - Actualización de estado a NO_SHOW
     * - Registro en auditoría
     * - Notificación WebSocket al usuario (FASE 9 - US-N001) - Implementado
     */
    @Override
    @Transactional
    public AppointmentResponse markAsNoShow(Long appointmentId, Long adminUserId, String clientIp) {
        logger.info("Solicitud de marcar turno como no-show - ID: {}, Admin: {}", appointmentId, adminUserId);

        // 1. Buscar el turno
        Appointment appointment = appointmentRepository.findById(appointmentId)
            .orElseThrow(() -> {
                logger.warn("Turno no encontrado para marcar como no-show - ID: {}", appointmentId);
                return new ApiException("Turno no encontrado", HttpStatus.NOT_FOUND);
            });

        // 2. Validar que el turno esté en estado CONFIRMED
        if (appointment.getState() != AppointmentState.CONFIRMED) {
            logger.warn("Intento de marcar como no-show un turno no confirmado - ID: {}, Estado: {}",
                appointmentId, appointment.getState());
            throw new ApiException(
                String.format("Solo turnos confirmados pueden marcarse como no-show. Estado actual: %s", 
                    appointment.getState()),
                HttpStatus.BAD_REQUEST);
        }

        // 3. Validar que el horario del turno ya pasó
        if (!appointment.isPast()) {
            logger.warn("Intento de marcar como no-show antes del horario - ID: {}, Fecha: {}, Hora: {}",
                appointmentId, appointment.getAppointmentDate(), appointment.getStartTime());
            throw new ApiException(
                "No se puede marcar como no-show antes del horario del turno",
                HttpStatus.BAD_REQUEST);
        }

        // 4. Marcar como no-show
        try {
            AppointmentState previousState = appointment.getState();
            appointment.markAsNoShow();
            Appointment noShowAppointment = appointmentRepository.save(appointment);

            logger.info("Turno marcado como no-show exitosamente - ID: {}, Usuario: {}, Admin: {}",
                appointmentId, appointment.getUserId(), adminUserId);

            // 5. Registrar en auditoría
            AppointmentHistory history = new AppointmentHistory(
                noShowAppointment.getId(),
                adminUserId,
                previousState,
                AppointmentState.NO_SHOW,
                "NO_SHOW",
                String.format("Turno marcado como no-show por el administrador. Usuario: %d, Fecha: %s, Hora: %s",
                    appointment.getUserId(), appointment.getAppointmentDate(), 
                    appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm"))),
                clientIp
            );
            historyRepository.save(history);

            // 6. Enviar notificación WebSocket (asíncrono, no bloquea) - US-N001
            try {
                webSocketNotificationService.sendNotificationToUser(
                    appointment.getUserId(),
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_NO_SHOW,
                    "Turno Marcado como No-Show",
                    String.format("Tu turno del %s a las %s ha sido marcado como no-show (ausente).",
                        appointment.getAppointmentDate(), appointment.getStartTime()),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                    appointment.getId(),
                    appointment.getId()
                );
            } catch (Exception e) {
                logger.error("Error al enviar notificación WebSocket de no-show - Turno ID: {}. Error: {}", 
                    appointment.getId(), e.getMessage(), e);
            }

            return mapToResponse(noShowAppointment);

        } catch (IllegalStateException e) {
            logger.warn("Error al marcar como no-show - ID: {}, Error: {}", appointmentId, e.getMessage());
            throw new ApiException(
                String.format("No se puede marcar como no-show: %s", e.getMessage()),
                HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Obtiene los turnos del usuario autenticado con filtros y paginación.
     * 
     * Implementa US-T011.1:
     * - Solo muestra turnos del usuario autenticado
     * - Filtros opcionales: estado, rango de fechas, upcoming, past
     * - Paginación obligatoria
     * - Ordenamiento por fecha y hora ascendente
     * - upcoming y past son mutuamente excluyentes
     */
    @Override
    public MyAppointmentsResponse getMyAppointments(
            Long userId,
            AppointmentState status,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            java.util.List<Integer> daysOfWeek,
            Boolean upcoming,
            Boolean past,
            String sortOrder,
            int page,
            int size) {
        
        logger.info("Consultando turnos del usuario - ID: {}, Estado: {}, Desde: {}, Hasta: {}, Días: {}, Upcoming: {}, Past: {}, SortOrder: {}, Página: {}, Tamaño: {}",
            userId, status, fromDate, toDate, daysOfWeek, upcoming, past, sortOrder, page, size);

        // 1. Validar que upcoming y past no sean ambos true
        if (Boolean.TRUE.equals(upcoming) && Boolean.TRUE.equals(past)) {
            throw new ApiException(
                "Los parámetros 'upcoming' y 'past' son mutuamente excluyentes. No se pueden usar ambos a la vez",
                HttpStatus.BAD_REQUEST);
        }

        // 2. Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // 3. Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }

        // 4. Validar rango de fechas
        if (fromDate != null && toDate != null && fromDate.isAfter(toDate)) {
            throw new ApiException(
                "La fecha 'fromDate' no puede ser posterior a 'toDate'",
                HttpStatus.BAD_REQUEST);
        }

        // 5. Si upcoming=true, ajustar fromDate a hoy si no está especificado
        LocalDate adjustedFromDate = fromDate;
        LocalDate adjustedToDate = toDate;
        LocalDateTime now = getNowGMT3();
        LocalDate today = now.toLocalDate();

        if (Boolean.TRUE.equals(upcoming)) {
            // Solo turnos futuros: fecha >= hoy Y (si hay hora) hora >= ahora si es hoy
            if (adjustedFromDate == null || adjustedFromDate.isBefore(today)) {
                adjustedFromDate = today;
            }
        } else if (Boolean.TRUE.equals(past)) {
            // Solo turnos pasados: fecha < hoy O (fecha = hoy Y hora < ahora)
            if (adjustedToDate == null || adjustedToDate.isAfter(today) || adjustedToDate.equals(today)) {
                adjustedToDate = today.minusDays(1); // Solo fechas anteriores a hoy
            }
        }

        // 6. Crear Pageable sin Sort ya que el ordenamiento está en la query nativa
        // El ORDER BY ya está definido en la query SQL con nombres de columna (appointment_date, start_time)
        Pageable pageable = PageRequest.of(page, size, Sort.unsorted());

        // 7. Buscar turnos con filtros (orden: asc = más viejos primero, desc = más recientes primero)
        List<Integer> daysOfWeekParam = (daysOfWeek != null && !daysOfWeek.isEmpty()) ? daysOfWeek : null;
        int daysOfWeekCount = (daysOfWeekParam != null) ? daysOfWeekParam.size() : 0;
        boolean orderDesc = sortOrder != null && "desc".equalsIgnoreCase(sortOrder.trim());
        // Cancelados unificados: CANCELLED incluye usuario y admin
        java.util.List<String> statesParam = null;
        int statesCount = 0;
        if (status != null) {
            if (status == AppointmentState.CANCELLED) {
                statesParam = java.util.List.of(AppointmentState.CANCELLED.name(), AppointmentState.CANCELLED_BY_ADMIN.name());
                statesCount = 2;
            } else {
                statesParam = java.util.List.of(status.name());
                statesCount = 1;
            }
        }
        String stateStr = (status != null && statesCount <= 1) ? status.name() : null;

        Page<Appointment> appointmentPage;
        if (statesCount > 1) {
            appointmentPage = orderDesc
                ? appointmentRepository.findByUserIdWithFiltersStatesInOrderByDateDesc(
                    userId, statesParam, statesCount, adjustedFromDate, adjustedToDate, daysOfWeekParam, daysOfWeekCount, pageable)
                : appointmentRepository.findByUserIdWithFiltersStatesIn(
                    userId, statesParam, statesCount, adjustedFromDate, adjustedToDate, daysOfWeekParam, daysOfWeekCount, pageable);
        } else {
            appointmentPage = orderDesc
                ? appointmentRepository.findByUserIdWithFiltersOrderByDateDesc(
                    userId, stateStr, adjustedFromDate, adjustedToDate, daysOfWeekParam, daysOfWeekCount, pageable)
                : appointmentRepository.findByUserIdWithFilters(
                    userId, stateStr, adjustedFromDate, adjustedToDate, daysOfWeekParam, daysOfWeekCount, pageable);
        }

        // 8. Si upcoming=true, filtrar en memoria para incluir solo turnos que aún no pasaron
        List<Appointment> filteredAppointments = appointmentPage.getContent();
        
        if (Boolean.TRUE.equals(upcoming)) {
            filteredAppointments = filteredAppointments.stream()
                .filter(appointment -> {
                    LocalDateTime appointmentDateTime = LocalDateTime.of(
                        appointment.getAppointmentDate(), appointment.getStartTime());
                    return appointmentDateTime.isAfter(now) || appointmentDateTime.isEqual(now);
                })
                .toList();
        } else if (Boolean.TRUE.equals(past)) {
            filteredAppointments = filteredAppointments.stream()
                .filter(appointment -> {
                    LocalDateTime appointmentDateTime = LocalDateTime.of(
                        appointment.getAppointmentDate(), appointment.getStartTime());
                    return appointmentDateTime.isBefore(now);
                })
                .toList();
        }

        // 9. Mapear a DTOs
        List<AppointmentResponse> appointmentResponses = filteredAppointments.stream()
            .map(this::mapToResponse)
            .toList();

        // 10. Calcular total ajustado si se filtró en memoria
        long total;
        if (Boolean.TRUE.equals(upcoming) || Boolean.TRUE.equals(past)) {
            // Si filtramos en memoria, necesitamos contar todos los que cumplen el filtro
            // Por simplicidad, usamos el total de la página y ajustamos si es necesario
            total = appointmentPage.getTotalElements();
            // Si el filtro en memoria redujo resultados, necesitaríamos recalcular
            // Por ahora, mantenemos el total de la query y ajustamos si es la última página
            if (filteredAppointments.size() < appointmentPage.getContent().size() && 
                appointmentPage.isLast()) {
                // Es la última página y algunos fueron filtrados, ajustar total
                total = appointmentPage.getTotalElements() - 
                    (appointmentPage.getContent().size() - filteredAppointments.size());
            }
        } else {
            total = appointmentPage.getTotalElements();
        }

        logger.info("Turnos encontrados - Total: {}, Página: {}, Tamaño: {}, Resultados filtrados: {}",
            total, page, size, filteredAppointments.size());

        return new MyAppointmentsResponse(appointmentResponses, total, page, size);
    }

    /**
     * Obtiene el historial completo de un turno.
     * 
     * Implementa US-T011.3:
     * - Solo muestra historial de turnos del usuario autenticado
     * - Ordenado por timestamp descendente (más recientes primero)
     * - Incluye información del usuario que realizó cada acción
     * - Incluye motivos y detalles
     */
    @Override
    public AppointmentHistoryResponse getAppointmentHistory(Long appointmentId, Long userId) {
        logger.info("Consultando historial de turno - ID: {}, Usuario: {}", appointmentId, userId);

        // 1. Buscar el turno y validar que pertenezca al usuario
        Appointment appointment = appointmentRepository.findById(appointmentId)
            .orElseThrow(() -> {
                logger.warn("Turno no encontrado para historial - ID: {}", appointmentId);
                return new ApiException("Turno no encontrado", HttpStatus.NOT_FOUND);
            });

        // 2. Validar que el turno pertenezca al usuario
        if (!appointment.getUserId().equals(userId)) {
            logger.warn("Intento de acceder al historial de turno ajeno - Turno ID: {}, Usuario solicitante: {}, Dueño: {}",
                appointmentId, userId, appointment.getUserId());
            throw new ApiException(
                "No tienes permisos para ver el historial de este turno",
                HttpStatus.FORBIDDEN);
        }

        // 3. Buscar historial del turno ordenado por fecha descendente (más recientes primero)
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory> historyList =
            historyRepository.findByAppointmentIdOrderByCreatedAtDesc(appointmentId);

        logger.info("Historial encontrado - Turno ID: {}, Eventos: {}", appointmentId, historyList.size());

        // 4. Mapear a DTOs con información del usuario
        List<HistoryItemResponse> historyItems = historyList.stream()
            .map(history -> mapToHistoryItemResponse(history))
            .toList();

        return new AppointmentHistoryResponse(appointmentId, historyItems);
    }

    /**
     * Obtiene el historial completo de un turno para administrador.
     * 
     * Implementa US-T017.2:
     * - Solo accesible por admin
     * - No valida propiedad del turno (admin puede ver cualquier turno)
     * - Incluye todos los cambios de estado
     * - Incluye información de quién hizo cada cambio
     * - Incluye fechas y motivos
     * - Incluye relación con otros turnos (reprogramaciones)
     * - Incluye solicitudes de reprogramación asociadas
     */
    @Override
    public AppointmentHistoryResponse getAppointmentHistoryForAdmin(Long appointmentId) {
        logger.info("Consultando historial de turno (admin) - ID: {}", appointmentId);

        // 1. Validar que el turno exista
        Appointment appointment = appointmentRepository.findById(appointmentId)
            .orElseThrow(() -> new ApiException(
                String.format("Turno no encontrado: %d", appointmentId),
                HttpStatus.NOT_FOUND));

        logger.info("Turno encontrado - ID: {}, Estado: {}, Usuario: {}", 
            appointmentId, appointment.getState(), appointment.getUserId());

        // 2. Buscar historial del turno ordenado por fecha descendente (más recientes primero)
        List<AppointmentHistory> historyList =
            historyRepository.findByAppointmentIdOrderByCreatedAtDesc(appointmentId);

        logger.info("Historial encontrado - Turno ID: {}, Eventos: {}", appointmentId, historyList.size());

        // 3. Buscar solicitudes de reprogramación asociadas (para información adicional)
        List<RescheduleRequest> rescheduleRequests = rescheduleRequestRepository
            .findByAppointmentIdOrderByCreatedAtDesc(appointmentId);

        logger.info("Solicitudes de reprogramación encontradas - Turno ID: {}, Solicitudes: {}", 
            appointmentId, rescheduleRequests.size());

        // 4. Mapear a DTOs con información del usuario
        // Para admin, no necesitamos información adicional diferente a la de usuario normal,
        // pero la implementación permite futuras extensiones
        List<HistoryItemResponse> historyItems = historyList.stream()
            .map(history -> mapToHistoryItemResponseForAdmin(history, appointment, rescheduleRequests))
            .toList();

        return new AppointmentHistoryResponse(appointmentId, historyItems);
    }

    /**
     * Mapea AppointmentHistory a HistoryItemResponse para admin, incluyendo información adicional.
     * Similar al método de usuario, pero sin validar propiedad del turno.
     */
    private HistoryItemResponse mapToHistoryItemResponseForAdmin(
            AppointmentHistory history,
            Appointment appointment,
            List<RescheduleRequest> rescheduleRequests) {
        
        HistoryItemResponse response = new HistoryItemResponse();
        response.setId(history.getId());
        response.setAction(history.getAction());
        response.setPreviousState(history.getPreviousState());
        response.setNewState(history.getNewState());
        response.setTimestamp(history.getCreatedAt().toString());
        response.setPerformedByUserId(history.getUserId());
        response.setReason(history.getReason());

        // Obtener email del usuario que realizó la acción
        Optional<User> user = userService.findById(history.getUserId());
        if (user.isPresent()) {
            response.setPerformedByEmail(user.get().getEmail());
        }

        // Si el turno tiene previousAppointmentId y la acción es RESCHEDULED, incluir esa relación
        // Esto muestra la relación con otros turnos (reprogramaciones)
        if ("RESCHEDULED".equals(history.getAction()) && appointment.getPreviousAppointmentId() != null) {
            response.setRelatedAppointmentId(appointment.getPreviousAppointmentId());
        }

        // Si hay solicitudes de reprogramación y la acción está relacionada, podemos agregar información
        // Por ahora, la información de solicitudes de reprogramación está disponible en las solicitudes
        // mismas, pero no se muestra directamente en cada item del historial
        // (esto podría extenderse en el futuro si se necesita)

        return response;
    }

    /**
     * Mapea AppointmentHistory a HistoryItemResponse, incluyendo información del usuario.
     */
    private HistoryItemResponse mapToHistoryItemResponse(
            com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory history) {
        
        HistoryItemResponse response = new HistoryItemResponse();
        response.setId(history.getId());
        response.setAction(history.getAction());
        response.setPreviousState(history.getPreviousState());
        response.setNewState(history.getNewState());
        response.setTimestamp(history.getCreatedAt().toString());
        response.setPerformedByUserId(history.getUserId());
        response.setReason(history.getReason());

        // Obtener email del usuario que realizó la acción
        Optional<User> user = userService.findById(history.getUserId());
        if (user.isPresent()) {
            response.setPerformedByEmail(user.get().getEmail());
        }

        // Si la acción es RESCHEDULED, buscar el turno relacionado
        if ("RESCHEDULED".equals(history.getAction()) || "RESCHEDULE_REQUEST_AUTO_CANCELLED".equals(history.getAction())) {
            // Intentar extraer el ID del turno relacionado desde el motivo si está disponible
            // Por ahora, el relatedAppointmentId se puede obtener del appointment.previousAppointmentId
            // pero no está directamente en el historial, así que lo dejamos null por ahora
            response.setRelatedAppointmentId(null);
        }

        return response;
    }

    /**
     * Obtiene todos los turnos con filtros para administrador.
     * 
     * Implementa US-T017:
     * - Solo accesible por admin
     * - Filtros opcionales: estado, usuario, rango de fechas, fecha específica
     * - Búsqueda por email o nombre de usuario
     * - Paginación obligatoria
     * - Ordenamiento por fecha descendente y hora ascendente
     * - Incluye información del usuario (email, nombre)
     */
    @Override
    public AdminAppointmentsResponse getAllAppointments(
            AppointmentState state,
            Long userId,
            String search,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            java.time.LocalDate date,
            java.util.List<Integer> daysOfWeek,
            int page,
            int size) {
        
        logger.info("Consultando turnos (admin) - Estado: {}, Usuario: {}, Búsqueda: {}, Desde: {}, Hasta: {}, Fecha: {}, Días: {}, Página: {}, Tamaño: {}",
            state, userId, search, fromDate, toDate, date, daysOfWeek, page, size);

        // 1. Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // 2. Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }

        // 3. Validar rango de fechas
        if (fromDate != null && toDate != null && fromDate.isAfter(toDate)) {
            throw new ApiException(
                "La fecha 'fromDate' no puede ser posterior a 'toDate'",
                HttpStatus.BAD_REQUEST);
        }

        // 4. Validar que date y fromDate/toDate no se usen juntos
        if (date != null && (fromDate != null || toDate != null)) {
            throw new ApiException(
                "No se pueden usar los parámetros 'date' y 'fromDate'/'toDate' al mismo tiempo",
                HttpStatus.BAD_REQUEST);
        }

        // 5. Si hay búsqueda por término, obtener IDs de usuarios que coincidan
        List<Long> userIds = null;
        if (search != null && !search.isBlank()) {
            userIds = userRepository.findUserIdsBySearchTerm(search);
            if (userIds.isEmpty()) {
                // Si no hay usuarios que coincidan, retornar lista vacía
                logger.info("No se encontraron usuarios que coincidan con el término de búsqueda: {}", search);
                return new AdminAppointmentsResponse(
                    List.of(), 0, 0, page, size);
            }
        }

        // 6. Buscar turnos con filtros
        Page<Appointment> appointmentPage;
        
        // Convertir estado a String para query nativa
        String stateStr = state != null ? state.toString() : null;
        
        // Preparar días de la semana para la query (null si está vacío)
        List<Integer> daysOfWeekParam = (daysOfWeek != null && !daysOfWeek.isEmpty()) ? daysOfWeek : null;
        int daysOfWeekCount = (daysOfWeekParam != null) ? daysOfWeekParam.size() : 0;
        
        // Crear Pageable con paginación correcta
        // NO usar Sort aquí porque la query nativa ya tiene ORDER BY y Spring agregaría nombres de propiedades Java
        Pageable pageable = PageRequest.of(page, size);
        
        if (userIds != null) {
            appointmentPage = appointmentRepository.findByUserIdsWithFilters(
                userIds, stateStr, fromDate, toDate, date, daysOfWeekParam, daysOfWeekCount, pageable);
        } else {
            appointmentPage = appointmentRepository.findAllWithFilters(
                stateStr, userId, fromDate, toDate, date, daysOfWeekParam, daysOfWeekCount, pageable);
        }
        
        // 7. Mapear a DTOs con información del usuario
        List<AdminAppointmentResponse> appointmentResponses = appointmentPage.getContent().stream()
            .map(this::mapToAdminResponse)
            .toList();

        logger.info("Turnos encontrados (admin) - Total: {}, Página: {}, Tamaño: {}, Total páginas: {}",
            appointmentPage.getTotalElements(), page, size, appointmentPage.getTotalPages());

        return new AdminAppointmentsResponse(
            appointmentResponses,
            appointmentPage.getTotalElements(),
            appointmentPage.getTotalPages(),
            page,
            size);
    }

    /**
     * NUEVO: Obtiene turnos activos que están en un día cerrado según la configuración actual.
     */
    @Override
    @Transactional(readOnly = true)
    public AdminAppointmentsResponse getAppointmentsAffectedByClosedDay(java.time.LocalDate date, int page, int size) {
        logger.info("Consultando turnos afectados por día cerrado - Fecha: {}, Página: {}, Tamaño: {}", 
            date, page, size);
        
        // 1. Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // 2. Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }
        
        // 3. Obtener turnos activos para esa fecha (CREATED y CONFIRMED)
        List<AppointmentState> activeStates = Arrays.asList(
            AppointmentState.CREATED,
            AppointmentState.CONFIRMED
        );
        
        Pageable pageable = PageRequest.of(page, size, Sort.by("startTime").ascending());
        Page<Appointment> appointments = appointmentRepository
            .findByDateAndStateIn(date, activeStates, pageable);
        
        // 4. Mapear a respuesta
        List<AdminAppointmentResponse> appointmentResponses = appointments.getContent().stream()
            .map(this::mapToAdminResponse)
            .toList();
        
        logger.info("Turnos afectados encontrados - Total: {}, Página: {}, Tamaño: {}, Total páginas: {}", 
            appointments.getTotalElements(), page, size, appointments.getTotalPages());
        
        return new AdminAppointmentsResponse(
            appointmentResponses,
            appointments.getTotalElements(),
            appointments.getTotalPages(),
            page,
            size);
    }

    /**
     * Mapea Appointment a AdminAppointmentResponse, incluyendo información del usuario.
     */
    private AdminAppointmentResponse mapToAdminResponse(Appointment appointment) {
        AdminAppointmentResponse response = new AdminAppointmentResponse();
        response.setId(appointment.getId());
        response.setUserId(appointment.getUserId());
        // Enviar fecha con zona horaria local (GMT-3) para evitar problemas de interpretación en el frontend
        // Formato: "2026-02-09T00:00:00-03:00" (con zona horaria explícita)
        // Esto asegura que todos los clientes (web, móvil, etc.) interpreten la fecha correctamente
        response.setDate(appointment.getAppointmentDate().toString());
        response.setStartTime(appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setEndTime(appointment.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm")));
        response.setDurationMinutes(appointment.getDurationMinutes());
        response.setState(appointment.getState().toString());
        response.setExpiresAt(appointment.getExpiresAt() != null ? appointment.getExpiresAt().toString() : null);
        response.setConfirmedAt(appointment.getConfirmedAt() != null ? appointment.getConfirmedAt().toString() : null);
        // cancelledAt y cancellationReason no existen en Appointment, solo en historial
        response.setCancelledAt(null);
        response.setCancellationReason(null);
        response.setPreviousAppointmentId(appointment.getPreviousAppointmentId());
        response.setCalendarConfigVersion(appointment.getCalendarConfigVersion());
        response.setCreatedAt(appointment.getCreatedAt().toString());
        response.setUpdatedAt(appointment.getUpdatedAt().toString());

        // Si el turno fue reprogramado (RESCHEDULED), incluir la nueva fecha del turno
        if (appointment.getState() == AppointmentState.RESCHEDULED) {
            appointmentRepository.findByPreviousAppointmentId(appointment.getId())
                .ifPresent(next -> {
                    response.setNextAppointmentId(next.getId());
                    response.setNextAppointmentDate(next.getAppointmentDate().toString());
                    response.setNextAppointmentStartTime(next.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")));
                    response.setNextAppointmentEndTime(next.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm")));
                });
        }

        // Obtener información del usuario
        Optional<User> user = userService.findById(appointment.getUserId());
        if (user.isPresent()) {
            response.setUserEmail(user.get().getEmail());
            response.setUserFirstName(user.get().getFirstName());
            response.setUserLastName(user.get().getLastName());
        }

        return response;
    }


    /**
     * Obtiene el calendario de turnos agendados para un rango de fechas (solo admin).
     * 
     * Implementa US-T017.1:
     * - Solo accesible por admin
     * - Agrupa turnos por día
     * - Genera slots disponibles usando la configuración del calendario
     * - Marca slots ocupados con información del turno
     * - Incluye información del usuario en cada turno
     * - Muestra disponibilidad vs ocupación
     */
    @Override
    public AppointmentsCalendarResponse getAppointmentsCalendar(
            java.time.LocalDate startDate, java.time.LocalDate endDate) {
        
        logger.info("Consultando calendario de turnos (admin) - Desde: {}, Hasta: {}", startDate, endDate);

        // 1. Validar rango de fechas
        if (startDate == null || endDate == null) {
            throw new ApiException(
                "Las fechas 'startDate' y 'endDate' son obligatorias",
                HttpStatus.BAD_REQUEST);
        }

        if (startDate.isAfter(endDate)) {
            throw new ApiException(
                "La fecha 'startDate' no puede ser posterior a 'endDate'",
                HttpStatus.BAD_REQUEST);
        }

        // 2. Validar que el rango no exceda 3 meses (90 días)
        long daysBetween = ChronoUnit.DAYS.between(startDate, endDate);
        if (daysBetween > 90) {
            throw new ApiException(
                String.format("El rango de fechas no puede exceder 3 meses (90 días). Rango solicitado: %d días", daysBetween),
                HttpStatus.BAD_REQUEST);
        }

        // 3. Buscar todos los turnos en el rango (solo CONFIRMED para el calendario general)
        // El admin necesita ver solo turnos confirmados para gestionar el calendario
        List<AppointmentState> occupyingStates = List.of(
            AppointmentState.CONFIRMED
        );

        List<Appointment> appointmentsInRange = new ArrayList<>();
        LocalDate currentDate = startDate;
        while (!currentDate.isAfter(endDate)) {
            List<Appointment> dayAppointments = appointmentRepository.findByDateAndStateIn(
                currentDate, occupyingStates);
            appointmentsInRange.addAll(dayAppointments);
            currentDate = currentDate.plusDays(1);
        }

        logger.info("Turnos encontrados en el rango: {}", appointmentsInRange.size());

        // 4. Crear mapa de turnos por fecha y hora de inicio para búsqueda rápida
        Map<String, Appointment> appointmentsByDateAndTime = new HashMap<>();
        for (Appointment appointment : appointmentsInRange) {
            String key = appointment.getAppointmentDate().toString() + "_" + 
                        appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm"));
            appointmentsByDateAndTime.put(key, appointment);
        }

        // 5. Generar calendario día por día
        List<CalendarDayResponse> calendarDays = new ArrayList<>();
        int totalSlots = 0;
        int totalAvailableSlots = 0;
        int totalOccupiedSlots = 0;

        currentDate = startDate;
        while (!currentDate.isAfter(endDate)) {
            CalendarDayResponse dayResponse = generateCalendarDay(
                currentDate, 
                appointmentsByDateAndTime);
            
            calendarDays.add(dayResponse);
            totalSlots += dayResponse.getTotalSlots();
            totalAvailableSlots += dayResponse.getAvailableSlots();
            totalOccupiedSlots += dayResponse.getOccupiedSlots();

            currentDate = currentDate.plusDays(1);
        }

        logger.info("Calendario generado - Días: {}, Total slots: {}, Disponibles: {}, Ocupados: {}",
            calendarDays.size(), totalSlots, totalAvailableSlots, totalOccupiedSlots);

        return new AppointmentsCalendarResponse(
            startDate.toString(),
            endDate.toString(),
            calendarDays,
            calendarDays.size(),
            totalSlots,
            totalAvailableSlots,
            totalOccupiedSlots
        );
    }

    /**
     * Genera la información del calendario para un día específico.
     */
    private CalendarDayResponse generateCalendarDay(
            LocalDate date,
            Map<String, Appointment> appointmentsByDateAndTime) {
        
        List<CalendarSlotResponse> slots = new ArrayList<>();

        // 1. Obtener slots disponibles para este día usando CalendarConfigurationService
        try {
            SlotsResponse slotsResponse = configurationService.getAvailableSlots(date);
            List<SlotResponse> availableSlots = slotsResponse.getSlots();

            // 2. Para cada slot, verificar si está ocupado por un turno
            for (SlotResponse slot : availableSlots) {
                String key = date.toString() + "_" + slot.getStart();
                Appointment appointment = appointmentsByDateAndTime.get(key);

                CalendarAppointmentInfo appointmentInfo = null;
                boolean isAvailable = Boolean.TRUE.equals(slot.getAvailable()) && appointment == null;

                if (appointment != null) {
                    // Obtener información del usuario
                    Optional<User> user = userService.findById(appointment.getUserId());
                    if (user.isPresent()) {
                        appointmentInfo = new CalendarAppointmentInfo(
                            appointment.getId(),
                            appointment.getUserId(),
                            user.get().getEmail(),
                            user.get().getFirstName(),
                            user.get().getLastName(),
                            appointment.getState().toString()
                        );
                    } else {
                        appointmentInfo = new CalendarAppointmentInfo(
                            appointment.getId(),
                            appointment.getUserId(),
                            null,
                            null,
                            null,
                            appointment.getState().toString()
                        );
                    }
                    isAvailable = false;
                }

                CalendarSlotResponse calendarSlot = new CalendarSlotResponse(
                    slot.getStart(),
                    slot.getEnd(),
                    appointmentInfo,
                    isAvailable
                );

                slots.add(calendarSlot);
            }

        } catch (ApiException e) {
            // Si no hay slots disponibles (día cerrado), retornar lista vacía
            logger.debug("Día cerrado o sin slots disponibles - Fecha: {}, Mensaje: {}", date, e.getMessage());
        }

        // 3. Calcular estadísticas
        int totalSlots = slots.size();
        int availableSlots = (int) slots.stream().filter(CalendarSlotResponse::isAvailable).count();
        int occupiedSlots = totalSlots - availableSlots;

        return new CalendarDayResponse(
            date.toString(),
            slots,
            totalSlots,
            availableSlots,
            occupiedSlots
        );
    }

    /**
     * Exporta turnos en formato CSV o Excel para administrador.
     * 
     * Implementa US-T017.3:
     * - Solo accesible por admin
     * - Aplica los mismos filtros que getAllAppointments
     * - Formato CSV o XLSX (por ahora solo CSV)
     * - Límite de 10,000 registros por request
     * - Para más registros, se debe usar exportación asíncrona (futuro)
     */
    @Override
    public ResponseEntity<byte[]> exportAppointments(
            String format,
            AppointmentState state,
            Long userId,
            String search,
            LocalDate fromDate,
            LocalDate toDate,
            LocalDate date) {
        
        logger.info("Exportando turnos (admin) - Formato: {}, Estado: {}, Usuario: {}, Búsqueda: {}, Desde: {}, Hasta: {}, Fecha: {}",
            format, state, userId, search, fromDate, toDate, date);

        // 1. Validar formato
        if (format == null || format.isBlank()) {
            throw new ApiException(
                "El formato de exportación es obligatorio. Formatos válidos: CSV, XLSX",
                HttpStatus.BAD_REQUEST);
        }

        String formatUpper = format.toUpperCase();
        if (!"CSV".equals(formatUpper) && !"XLSX".equals(formatUpper)) {
            throw new ApiException(
                String.format("Formato inválido: %s. Formatos válidos: CSV, XLSX", format),
                HttpStatus.BAD_REQUEST);
        }

        // Por ahora solo implementamos CSV
        if (!"CSV".equals(formatUpper)) {
            throw new ApiException(
                "Formato Excel (XLSX) aún no está implementado. Por favor use CSV.",
                HttpStatus.NOT_IMPLEMENTED);
        }

        // 2. Obtener todos los turnos como DTOs para generar CSV
        List<AdminAppointmentResponse> appointments = new ArrayList<>();
        int maxRecords = 10000;
        int pageSize = 100; // Máximo permitido por getAllAppointments
        int currentPage = 0;

        while (appointments.size() < maxRecords) {
            AdminAppointmentsResponse response = getAllAppointments(
                state, userId, search, fromDate, toDate, date, null, currentPage, pageSize);
            
            appointments.addAll(response.getContent());

            // Si no hay más páginas o se alcanzó el límite, salir
            if (response.getContent().isEmpty() || response.getPage() >= response.getTotalPages() - 1) {
                break;
            }

            // Si ya tenemos muchos registros y la siguiente página podría exceder el límite, salir
            if (appointments.size() + pageSize > maxRecords) {
                break;
            }

            currentPage++;
        }

        // 3. Validar límite de registros
        if (appointments.size() >= maxRecords) {
            throw new ApiException(
                String.format("La exportación excede el límite de %d registros. Use exportación asíncrona para más registros.", maxRecords),
                HttpStatus.BAD_REQUEST);
        }

        logger.info("Turnos a exportar - Total: {}, Formato: {}", appointments.size(), formatUpper);

        // 4. Generar CSV
        byte[] csvBytes = generateCsv(appointments);

        // 5. Crear nombre de archivo con timestamp
        String filename = String.format("turnos_export_%s.csv", 
            LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd")));

        // 6. Crear respuesta con headers apropiados para descarga
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.parseMediaType("text/csv; charset=UTF-8"));
        headers.setContentDispositionFormData("attachment", filename);
        headers.setContentLength(csvBytes.length);

        logger.info("Archivo CSV generado - Nombre: {}, Tamaño: {} bytes", filename, csvBytes.length);

        return ResponseEntity.ok()
            .headers(headers)
            .body(csvBytes);
    }

    /**
     * Genera el contenido CSV desde una lista de turnos.
     */
    private byte[] generateCsv(List<AdminAppointmentResponse> appointments) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();

            // Escribir BOM para UTF-8 (para Excel)
            baos.write(0xEF);
            baos.write(0xBB);
            baos.write(0xBF);

            PrintWriter writer = new PrintWriter(baos, true, StandardCharsets.UTF_8);

            // Escribir encabezados CSV
            writer.println("ID,Usuario ID,Email,Nombre,Apellido,Fecha,Hora Inicio,Hora Fin,Duración (min),Estado,Confirmado En,Creado En,Actualizado En");

            // Escribir datos
            for (AdminAppointmentResponse app : appointments) {
                writer.printf("%d,%d,%s,%s,%s,%s,%s,%s,%d,%s,%s,%s,%s%n",
                    app.getId(),
                    app.getUserId(),
                    escapeCsv(app.getUserEmail()),
                    escapeCsv(app.getUserFirstName()),
                    escapeCsv(app.getUserLastName()),
                    app.getDate(),
                    app.getStartTime(),
                    app.getEndTime(),
                    app.getDurationMinutes(),
                    app.getState(),
                    escapeCsv(app.getConfirmedAt()),
                    app.getCreatedAt(),
                    app.getUpdatedAt()
                );
            }

            writer.flush();
            writer.close();
            return baos.toByteArray();
        } catch (Exception e) {
            logger.error("Error al generar CSV: {}", e.getMessage(), e);
            throw new ApiException(
                "Error al generar archivo CSV: " + e.getMessage(),
                HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Escapa valores para CSV (si contienen comas o comillas).
     */
    private String escapeCsv(String value) {
        if (value == null) {
            return "";
        }
        // Si contiene comas, comillas o saltos de línea, envolver en comillas y escapar comillas internas
        if (value.contains(",") || value.contains("\"") || value.contains("\n")) {
            return "\"" + value.replace("\"", "\"\"") + "\"";
        }
        return value;
    }

    /**
     * Reprograma un turno directamente como administrador.
     * 
     * Implementa US-T023:
     * - Solo accesible por admin
     * - Puede reprogramar cualquier turno futuro
     * - Motivo obligatorio (mínimo 10 caracteres)
     * - Validación de disponibilidad del nuevo slot
     * - Turno original pasa a estado RESCHEDULED
     * - Nuevo turno creado con previousAppointmentId
     * - Nuevo turno en estado CONFIRMED
     * - Operación transaccional atómica
     * - Registro en auditoría
     */
    @Override
    @Transactional
    public AppointmentResponse rescheduleAppointmentByAdmin(Long appointmentId, 
                                                              AdminRescheduleAppointmentRequest request,
                                                              Long adminUserId, String clientIp) {
        logger.info("Reprogramación por admin - Turno ID: {}, Admin: {}, Nueva fecha: {}, Nueva hora: {}",
            appointmentId, adminUserId, request.getNewDate(), request.getNewStartTime());

        try {
            // 1. Buscar el turno original
            Appointment originalAppointment = appointmentRepository.findById(appointmentId)
                .orElseThrow(() -> new ApiException(
                    "Turno no encontrado",
                    HttpStatus.NOT_FOUND));

            // 2. Validar que el turno sea futuro
            LocalDate today = getTodayGMT3();
            LocalDateTime appointmentDateTime = LocalDateTime.of(
                originalAppointment.getAppointmentDate(), 
                originalAppointment.getStartTime());
            LocalDateTime now = getNowGMT3();
            
            if (appointmentDateTime.isBefore(now)) {
                throw new ApiException(
                    "No se pueden reprogramar turnos pasados",
                    HttpStatus.BAD_REQUEST);
            }

            // 3. Validar que el turno esté en un estado que permita reprogramación
            // Permitir reprogramar: CREATED, CONFIRMED, RESCHEDULED (pero no CANCELLED, EXPIRED, NO_SHOW, COMPLETED)
            if (originalAppointment.getState() == AppointmentState.CANCELLED ||
                originalAppointment.getState() == AppointmentState.EXPIRED ||
                originalAppointment.getState() == AppointmentState.NO_SHOW ||
                originalAppointment.getState() == AppointmentState.COMPLETED) {
                throw new ApiException(
                    String.format("No se puede reprogramar un turno en estado %s", originalAppointment.getState()),
                    HttpStatus.BAD_REQUEST);
            }

            // 4. Validar que el motivo tenga al menos 10 caracteres
            if (request.getReason() == null || request.getReason().trim().length() < 10) {
                throw new ApiException(
                    "El motivo debe tener al menos 10 caracteres",
                    HttpStatus.BAD_REQUEST);
            }

            // 5. Validar que la nueva fecha no sea pasada
            if (request.getNewDate().isBefore(today)) {
                throw new ApiException(
                    "No se pueden reprogramar turnos a fechas pasadas. Fecha solicitada: " + request.getNewDate(),
                    HttpStatus.BAD_REQUEST);
            }

            // 6. Validar que el nuevo slot esté disponible
            // Usar lock pesimista para evitar condiciones de carrera
            validateSlotAvailability(request.getNewDate(), request.getNewStartTime(), 
                originalAppointment.getDurationMinutes());

            // 7. Obtener configuración activa
            CalendarConfiguration activeConfig = configurationRepository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                    "No existe una configuración activa. El sistema no está configurado.",
                    HttpStatus.SERVICE_UNAVAILABLE));

            Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
            if (appointmentDurationMinutes == null) {
                throw new ApiException(
                    "No hay duración de turnos configurada. Debe configurar la duración antes de reprogramar.",
                    HttpStatus.BAD_REQUEST);
            }

            // 8. Calcular hora de fin del nuevo turno
            LocalTime newStartTime = request.getNewStartTime();
            LocalTime newEndTime = newStartTime.plusMinutes(appointmentDurationMinutes);

            // 9. Guardar estado anterior del turno original
            AppointmentState previousState = originalAppointment.getState();

            // 10. Actualizar turno original a estado RESCHEDULED
            originalAppointment.setState(AppointmentState.RESCHEDULED);
            originalAppointment.setUpdatedAt(LocalDateTime.now());
            Appointment updatedOriginal = appointmentRepository.save(originalAppointment);
            logger.info("Turno original actualizado a RESCHEDULED - ID: {}", updatedOriginal.getId());

            // 11. Crear nuevo turno en estado CONFIRMED (admin lo programa directamente)
            Appointment newAppointment = new Appointment(
                originalAppointment.getUserId(),
                request.getNewDate(),
                newStartTime,
                newEndTime,
                appointmentDurationMinutes,
                AppointmentState.CONFIRMED, // Estado CONFIRMED directamente
                activeConfig.getVersion(),
                null, // expiresAt es null para turnos confirmados
                null, // idempotencyKey no aplica para reprogramaciones
                adminUserId // Creado por el admin
            );
            newAppointment.setPreviousAppointmentId(originalAppointment.getId());
            newAppointment.setConfirmedAt(LocalDateTime.now()); // Confirmado inmediatamente

            Appointment savedNewAppointment = appointmentRepository.save(newAppointment);
            logger.info("Nuevo turno creado por admin - ID: {}, Usuario: {}, Fecha: {}, Hora: {}",
                savedNewAppointment.getId(), savedNewAppointment.getUserId(), 
                savedNewAppointment.getAppointmentDate(), savedNewAppointment.getStartTime());

            // 12. Registrar auditoría para el turno original (cambio a RESCHEDULED)
            AppointmentHistory originalHistory = new AppointmentHistory(
                originalAppointment.getId(),
                adminUserId,
                previousState,
                AppointmentState.RESCHEDULED,
                "RESCHEDULED_BY_ADMIN",
                String.format("Reprogramado por administrador. Nueva fecha/hora: %s %s. Motivo: %s. Nuevo turno ID: %d",
                    request.getNewDate(), request.getNewStartTime(), request.getReason(), savedNewAppointment.getId()),
                clientIp
            );
            historyRepository.save(originalHistory);

            // 13. Registrar auditoría para el nuevo turno (creación como CONFIRMED)
            AppointmentHistory newHistory = new AppointmentHistory(
                savedNewAppointment.getId(),
                adminUserId,
                null, // previousState es null para creación
                AppointmentState.CONFIRMED,
                "CREATED_BY_ADMIN_RESCHEDULE",
                String.format("Turno creado por reprogramación administrativa. Turno original ID: %d. Motivo: %s",
                    originalAppointment.getId(), request.getReason()),
                clientIp
            );
            historyRepository.save(newHistory);

            // 14. Cancelar cualquier solicitud de reprogramación pendiente para el turno original
            List<RescheduleRequest> pendingRequests = rescheduleRequestRepository
                .findByAppointmentIdAndState(originalAppointment.getId(), RescheduleRequestState.PENDING_ADMIN_APPROVAL);
            
            for (RescheduleRequest pendingRequest : pendingRequests) {
                pendingRequest.setState(RescheduleRequestState.CANCELLED);
                pendingRequest.setExpirationReason("Turno reprogramado directamente por administrador");
                rescheduleRequestRepository.save(pendingRequest);
                
                // Registrar auditoría de cancelación de solicitud
                AppointmentHistory rescheduleHistory = new AppointmentHistory(
                    originalAppointment.getId(),
                    adminUserId,
                    AppointmentState.CONFIRMED,
                    AppointmentState.CONFIRMED, // El estado del turno no cambia, solo se cancela la solicitud
                    "RESCHEDULE_REQUEST_CANCELLED",
                    String.format("Solicitud de reprogramación cancelada automáticamente. Turno reprogramado por admin. Solicitud ID: %d",
                        pendingRequest.getId()),
                    clientIp
                );
                historyRepository.save(rescheduleHistory);
            }

            // 15. Enviar email de notificación (asíncrono, no bloquea)
            try {
                User user = userRepository.findById(originalAppointment.getUserId())
                    .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
                emailService.sendAppointmentRescheduledEmail(user, originalAppointment, savedNewAppointment, request.getReason());
            } catch (Exception e) {
                // NO lanzar excepción - el email no debe bloquear la reprogramación del turno
                logger.error("Error al enviar email de turno reprogramado - Turno original ID: {}, Nuevo turno ID: {}. Error: {}", 
                    originalAppointment.getId(), savedNewAppointment.getId(), e.getMessage(), e);
            }

            // 16. Enviar notificación WebSocket (asíncrono, no bloquea) - US-N001
            try {
                webSocketNotificationService.sendNotificationToUser(
                    originalAppointment.getUserId(),
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_RESCHEDULED,
                    "Turno Reprogramado",
                    String.format("Tu turno ha sido reprogramado. Nueva fecha: %s a las %s. Motivo: %s",
                        savedNewAppointment.getAppointmentDate(), savedNewAppointment.getStartTime(), request.getReason()),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                    savedNewAppointment.getId(),
                    savedNewAppointment.getId()
                );
            } catch (Exception e) {
                logger.error("Error al enviar notificación WebSocket de turno reprogramado - Turno ID: {}. Error: {}", 
                    savedNewAppointment.getId(), e.getMessage(), e);
            }

            // 17. Enviar actualización de disponibilidad en tiempo real (ambas fechas) - Tiempo Real
            try {
                webSocketNotificationService.broadcastAvailabilityUpdate(originalAppointment.getAppointmentDate());
                webSocketNotificationService.broadcastAvailabilityUpdate(savedNewAppointment.getAppointmentDate());
            } catch (Exception e) {
                logger.error("Error al enviar actualización de disponibilidad WebSocket - Fechas: {}, {}. Error: {}", 
                    originalAppointment.getAppointmentDate(), savedNewAppointment.getAppointmentDate(), e.getMessage(), e);
            }

            logger.info("Reprogramación completada - Turno original ID: {}, Nuevo turno ID: {}", 
                originalAppointment.getId(), savedNewAppointment.getId());

            return mapToResponse(savedNewAppointment);

        } catch (ObjectOptimisticLockingFailureException | OptimisticLockException e) {
            logger.warn("Conflicto de concurrencia al reprogramar turno - ID: {}", appointmentId);
            throw new ApiException(
                "El slot seleccionado ya no está disponible. Por favor, intenta con otro horario.",
                HttpStatus.CONFLICT);
        }
    }

    /**
     * Cancela un turno directamente como administrador.
     * 
     * Implementa US-T024:
     * - Solo accesible por admin
     * - Puede cancelar cualquier turno (sin validar ventana mínima)
     * - Motivo obligatorio (mínimo 10 caracteres)
     * - Turno pasa a estado CANCELLED_BY_ADMIN
     * - No se respetan ventanas de cancelación
     * - Operación transaccional atómica
     * - Registro en auditoría
     */
    @Override
    @Transactional
    public AppointmentResponse cancelAppointmentByAdmin(Long appointmentId, 
                                                          CancelAppointmentRequest request,
                                                          Long adminUserId, String clientIp) {
        logger.info("Cancelación por admin - Turno ID: {}, Admin: {}, Motivo: {}",
            appointmentId, adminUserId, request != null ? request.getReason() : "N/A");

        try {
            // 1. Buscar el turno
            Appointment appointment = appointmentRepository.findById(appointmentId)
                .orElseThrow(() -> new ApiException(
                    "Turno no encontrado",
                    HttpStatus.NOT_FOUND));

            // 2. Validar que el motivo tenga al menos 10 caracteres
            if (request == null || request.getReason() == null || request.getReason().trim().length() < 10) {
                throw new ApiException(
                    "El motivo debe tener al menos 10 caracteres",
                    HttpStatus.BAD_REQUEST);
            }

            // 3. Validar que el turno no esté ya cancelado o completado
            if (appointment.getState() == AppointmentState.CANCELLED || 
                appointment.getState() == AppointmentState.CANCELLED_BY_ADMIN) {
                throw new ApiException(
                    "El turno ya está cancelado",
                    HttpStatus.BAD_REQUEST);
            }

            if (appointment.getState() == AppointmentState.COMPLETED) {
                throw new ApiException(
                    "No se pueden cancelar turnos completados",
                    HttpStatus.BAD_REQUEST);
            }

            // 4. Guardar estado anterior
            AppointmentState previousState = appointment.getState();

            // 5. Cancelar el turno por admin (sin validar ventana mínima)
            appointment.cancelByAdmin();
            Appointment cancelledAppointment = appointmentRepository.save(appointment);
            logger.info("Turno cancelado por admin - ID: {}, Estado anterior: {}, Nuevo estado: {}",
                appointmentId, previousState, cancelledAppointment.getState());

            // 6. Cancelar automáticamente solicitudes de reprogramación pendientes (si existen)
            List<RescheduleRequest> pendingRequests = rescheduleRequestRepository
                .findByAppointmentIdAndState(appointmentId, RescheduleRequestState.PENDING_ADMIN_APPROVAL);

            for (RescheduleRequest pendingRequest : pendingRequests) {
                pendingRequest.cancel();
                rescheduleRequestRepository.save(pendingRequest);
                logger.info("Solicitud de reprogramación cancelada automáticamente - ID: {}, Turno ID: {}",
                    pendingRequest.getId(), appointmentId);

                // Registrar en auditoría la cancelación automática de la solicitud
                AppointmentHistory rescheduleHistory = new AppointmentHistory(
                    appointmentId,
                    adminUserId,
                    AppointmentState.CANCELLED_BY_ADMIN, // Estado del turno (ya cancelado por admin)
                    AppointmentState.CANCELLED_BY_ADMIN, // Estado del turno no cambia
                    "RESCHEDULE_REQUEST_AUTO_CANCELLED",
                    String.format("Solicitud de reprogramación cancelada automáticamente. Turno cancelado por admin. Solicitud ID: %d",
                        pendingRequest.getId()),
                    clientIp
                );
                historyRepository.save(rescheduleHistory);
            }

            // 7. Registrar en auditoría la cancelación del turno por admin
            AppointmentHistory history = new AppointmentHistory(
                cancelledAppointment.getId(),
                adminUserId,
                previousState,
                AppointmentState.CANCELLED_BY_ADMIN,
                "CANCELLED_BY_ADMIN",
                request.getReason(),
                clientIp
            );
            historyRepository.save(history);

            // 8. Enviar email de notificación (asíncrono, no bloquea)
            try {
                User user = userRepository.findById(cancelledAppointment.getUserId())
                    .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND));
                emailService.sendAppointmentCancelledEmail(user, cancelledAppointment, request.getReason(), true);
            } catch (Exception e) {
                // NO lanzar excepción - el email no debe bloquear la cancelación del turno
                logger.error("Error al enviar email de turno cancelado por admin - Turno ID: {}. Error: {}", 
                    cancelledAppointment.getId(), e.getMessage(), e);
            }

            // 9. Enviar notificación WebSocket (asíncrono, no bloquea) - US-N001
            try {
                webSocketNotificationService.sendNotificationToUser(
                    cancelledAppointment.getUserId(),
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_CANCELLED_BY_ADMIN,
                    "Turno Cancelado por Administrador",
                    String.format("Tu turno para el %s a las %s ha sido cancelado por un administrador. Motivo: %s",
                        cancelledAppointment.getAppointmentDate(), cancelledAppointment.getStartTime(), request.getReason()),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                    cancelledAppointment.getId(),
                    cancelledAppointment.getId()
                );
            } catch (Exception e) {
                logger.error("Error al enviar notificación WebSocket de turno cancelado por admin - Turno ID: {}. Error: {}", 
                    cancelledAppointment.getId(), e.getMessage(), e);
            }

            // 10. Enviar actualización de disponibilidad en tiempo real - Tiempo Real
            try {
                webSocketNotificationService.broadcastAvailabilityUpdate(cancelledAppointment.getAppointmentDate());
            } catch (Exception e) {
                logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                    cancelledAppointment.getAppointmentDate(), e.getMessage(), e);
            }

            logger.info("Cancelación por admin completada - Turno ID: {}, Motivo: {}",
                appointmentId, request.getReason());

            return mapToResponse(cancelledAppointment);

        } catch (IllegalStateException e) {
            // Capturar excepciones del dominio (ej: estado no válido)
            throw new ApiException(
                e.getMessage(),
                HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Crea un turno forzando las reglas normales (solo admin).
     * 
     * Implementa US-T025:
     * - Solo accesible por admin
     * - Permite crear turnos fuera de horario, en días cerrados, etc.
     * - Justificación obligatoria (mínimo 20 caracteres)
     * - Turno se marca como overridden = true
     * - Turno se crea directamente en estado CONFIRMED
     * - No se validan reglas normales
     */
    @Override
    @Transactional
    public AppointmentResponse createAppointmentOverride(CreateOverrideAppointmentRequest request,
                                                          Long adminUserId, String clientIp) {
        logger.info("Creación forzada de turno por admin - Usuario: {}, Fecha: {}, Hora: {}, Justificación: {}",
            request.getUserId(), request.getDate(), request.getStartTime(), request.getJustification());

        try {
            // 1. Validar que el usuario existe
            userService.findById(request.getUserId())
                .orElseThrow(() -> new ApiException(
                    "Usuario no encontrado",
                    HttpStatus.NOT_FOUND));

            // 2. Obtener configuración activa para obtener duración de turnos
            CalendarConfiguration activeConfig = configurationRepository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                    "No existe una configuración activa. El sistema no está configurado.",
                    HttpStatus.SERVICE_UNAVAILABLE));

            Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
            if (appointmentDurationMinutes == null) {
                throw new ApiException(
                    "No hay duración de turnos configurada. Debe configurar la duración antes de crear turnos.",
                    HttpStatus.BAD_REQUEST);
            }

            // 3. Calcular hora de fin del turno
            LocalTime startTime = request.getStartTime();
            LocalTime endTime = startTime.plusMinutes(appointmentDurationMinutes);

            // 4. Crear el turno en estado CONFIRMED directamente (sin pasar por CREATED)
            // NO se validan reglas normales: horarios, días cerrados, anticipación, límites, etc.
            Appointment appointment = new Appointment(
                request.getUserId(),
                request.getDate(),
                startTime,
                endTime,
                appointmentDurationMinutes,
                AppointmentState.CONFIRMED, // Estado CONFIRMED directamente
                activeConfig.getVersion(),
                null, // expiresAt es null para turnos confirmados
                null, // idempotencyKey no aplica para turnos forzados
                adminUserId // Creado por el admin
            );
            appointment.setOverridden(true);
            appointment.setOverrideJustification(request.getJustification());
            appointment.setConfirmedAt(LocalDateTime.now()); // Confirmado inmediatamente

            // 5. Guardar el turno
            Appointment savedAppointment = appointmentRepository.save(appointment);
            logger.info("Turno forzado creado por admin - ID: {}, Usuario: {}, Fecha: {}, Hora: {}",
                savedAppointment.getId(), savedAppointment.getUserId(),
                savedAppointment.getAppointmentDate(), savedAppointment.getStartTime());

            // 6. Registrar en auditoría
            AppointmentHistory history = new AppointmentHistory(
                savedAppointment.getId(),
                adminUserId,
                null, // previousState es null para creación
                AppointmentState.CONFIRMED,
                "CREATED_OVERRIDE",
                String.format("Turno creado forzando reglas normales por administrador. Justificación: %s",
                    request.getJustification()),
                clientIp
            );
            historyRepository.save(history);

            // 7. Enviar notificación WebSocket (asíncrono, no bloquea) - US-N001
            try {
                webSocketNotificationService.sendNotificationToUser(
                    request.getUserId(),
                    com.ak4n1.turn_management.feature.notification.domain.NotificationType.APPOINTMENT_OVERRIDE_CREATED,
                    "Turno Creado (Especial)",
                    String.format("Se ha creado un turno especial para ti el %s a las %s. Justificación: %s",
                        request.getDate(), request.getStartTime(), request.getJustification()),
                    com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType.APPOINTMENT,
                    appointment.getId(),
                    appointment.getId()
                );
            } catch (Exception e) {
                logger.error("Error al enviar notificación WebSocket de turno forzado - Turno ID: {}. Error: {}", 
                    appointment.getId(), e.getMessage(), e);
            }

            // 8. Enviar actualización de disponibilidad en tiempo real - Tiempo Real
            try {
                webSocketNotificationService.broadcastAvailabilityUpdate(request.getDate());
            } catch (Exception e) {
                logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                    request.getDate(), e.getMessage(), e);
            }

            logger.info("Creación forzada de turno completada - ID: {}, Justificación: {}",
                savedAppointment.getId(), request.getJustification());

            return mapToResponse(savedAppointment);

        } catch (Exception e) {
            logger.error("Error al crear turno forzado: {}", e.getMessage(), e);
            throw new ApiException(
                "Error al crear turno forzado: " + e.getMessage(),
                HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}

