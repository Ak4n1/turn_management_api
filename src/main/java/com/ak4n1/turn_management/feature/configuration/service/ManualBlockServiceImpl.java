package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.dto.request.ManualBlockRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.TimeRangeRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.ManualBlockResponse;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo;
import com.ak4n1.turn_management.feature.configuration.service.cancellation.AppointmentCancellationService;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.service.EmailService;
import com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de bloqueos operativos.
 * 
 * Responsabilidades:
 * - Validar bloqueos
 * - Detectar turnos existentes afectados
 * - Asegurar que no se creen bloqueos si affectsExistingAppointments = false y hay turnos afectados
 * - Mantener integridad de datos
 */
@Service
public class ManualBlockServiceImpl implements ManualBlockService {

    private static final Logger logger = LoggerFactory.getLogger(ManualBlockServiceImpl.class);
    
    /**
     * Zona horaria del sistema: GMT-3 (Argentina)
     */
    private static final ZoneId ZONE_ID = ZoneId.of("America/Argentina/Buenos_Aires");

    private final ManualBlockRepository repository;
    private final CalendarConfigurationMapper mapper;
    private final AppointmentRepository appointmentRepository;
    private final WebSocketNotificationService webSocketNotificationService;
    private final AppointmentCancellationService appointmentCancellationService;
    private final EmailService emailService;
    private final UserRepository userRepository;

    public ManualBlockServiceImpl(ManualBlockRepository repository,
                                 CalendarConfigurationMapper mapper,
                                 AppointmentRepository appointmentRepository,
                                 WebSocketNotificationService webSocketNotificationService,
                                 AppointmentCancellationService appointmentCancellationService,
                                 EmailService emailService,
                                 UserRepository userRepository) {
        this.repository = repository;
        this.mapper = mapper;
        this.appointmentRepository = appointmentRepository;
        this.webSocketNotificationService = webSocketNotificationService;
        this.appointmentCancellationService = appointmentCancellationService;
        this.emailService = emailService;
        this.userRepository = userRepository;
    }

    @Override
    @Transactional
    public ManualBlockResponse createBlock(ManualBlockRequest request, Long userId) {
        logger.info("Creando bloqueo operativo - Usuario: {}, Fecha: {}, IsFullDay: {}", 
            userId, request.getDate(), request.getIsFullDay());

        // 1. Validar request básico
        validateManualBlockRequest(request);

        // 2. Validar que la fecha no sea pasada (GMT-3)
        validateDateNotInPast(request.getDate());

        // 3. Validar reglas de isFullDay y timeRange
        validateIsFullDayAndTimeRange(request);

        // 4. Validar timeRange si está presente
        if (!request.getIsFullDay() && request.getTimeRange() != null) {
            validateTimeRange(request.getTimeRange());
        }

        // 5. Detectar turnos existentes afectados
        List<ManualBlockResponse.AffectedAppointmentInfo> affectedAppointments = 
            detectAffectedAppointments(request);

        // 6. Validar affectsExistingAppointments
        if (!request.getAffectsExistingAppointments() && !affectedAppointments.isEmpty()) {
            throw new ApiException(
                "Existen turnos en el rango bloqueado. Debe resolverlos primero o establecer 'affectsExistingAppointments' en true.",
                HttpStatus.CONFLICT);
        }

        // 7. Convertir request a entidad
        ManualBlock block = toEntity(request, userId);

        // 8. Guardar
        ManualBlock saved = repository.save(block);
        logger.info("Bloqueo operativo creado exitosamente - ID: {}, Fecha: {}, Turnos afectados: {}", 
            saved.getId(), saved.getBlockDate(), affectedAppointments.size());

        // 9. (Campanita) Se elimina la notificación de plataforma por creación de bloqueo.
        // Se mantienen: cancelaciones/emails y actualización de disponibilidad (más abajo).

        // Procesar turnos afectados: cancelar o solo notificar por email (igual que reglas de atención)
        try {
            java.util.List<Long> appointmentIdsToCancel = request.getAppointmentIdsToCancel();
            Boolean autoCancel = Boolean.TRUE.equals(request.getAutoCancelAffectedAppointments());
            String cancellationReason = request.getCancellationReason();
            if (cancellationReason == null || cancellationReason.trim().isEmpty()) {
                cancellationReason = "Bloqueo operativo programado";
            }
            if (appointmentIdsToCancel == null || appointmentIdsToCancel.isEmpty()) {
                return toResponse(saved, affectedAppointments);
            }
            logger.info("Procesando {} turno(s) afectados por bloqueo - AutoCancel: {}", appointmentIdsToCancel.size(), autoCancel);
            List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> affectedAppointmentsList = appointmentRepository
                    .findAllById(appointmentIdsToCancel)
                    .stream()
                    .filter(a -> a.getState() == com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED
                            || a.getState() == com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED)
                    .collect(java.util.stream.Collectors.toList());
            if (affectedAppointmentsList.isEmpty()) {
                return toResponse(saved, affectedAppointments);
            }
            java.util.Map<Long, java.util.List<UserAffectedAppointmentInfo>> userAffectedAppointments = new java.util.HashMap<>();
            java.util.List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointmentsToCancel = new java.util.ArrayList<>();
            for (com.ak4n1.turn_management.feature.appointment.domain.Appointment appointment : affectedAppointmentsList) {
                Long affectedUserId = appointment.getUserId();
                userAffectedAppointments.computeIfAbsent(affectedUserId, k -> new java.util.ArrayList<>())
                        .add(new UserAffectedAppointmentInfo(
                                appointment.getId(),
                                appointment.getAppointmentDate(),
                                appointment.getStartTime().toString(),
                                appointment.getEndTime().toString()));
                if (autoCancel) {
                    appointmentsToCancel.add(appointment);
                }
            }
            if (autoCancel && !appointmentsToCancel.isEmpty()) {
                appointmentCancellationService.cancelAffectedAppointmentsByDayClosure(appointmentsToCancel, userId, cancellationReason);
            }
            java.util.Set<Long> affectedUserIds = userAffectedAppointments.keySet();
            java.util.Map<Long, User> usersMap = new java.util.HashMap<>();
            if (!affectedUserIds.isEmpty()) {
                List<User> affectedUsers = userRepository.findAllById(affectedUserIds);
                for (User user : affectedUsers) {
                    usersMap.put(user.getId(), user);
                }
            }
            for (java.util.Map.Entry<Long, java.util.List<UserAffectedAppointmentInfo>> entry : userAffectedAppointments.entrySet()) {
                try {
                    Long affectedUserId = entry.getKey();
                    java.util.List<UserAffectedAppointmentInfo> appointments = entry.getValue();
                    User affectedUser = usersMap.get(affectedUserId);
                    if (affectedUser == null) {
                        continue;
                    }
                    if (autoCancel) {
                        emailService.sendMassCancellationEmail(affectedUser, appointments, cancellationReason);
                        String message = appointments.size() == 1
                                ? String.format("Tu turno para el %s a las %s ha sido cancelado. Motivo: %s. Por favor, vuelva a solicitar un turno.",
                                        appointments.get(0).getDate(), appointments.get(0).getStartTime(), cancellationReason)
                                : String.format("Tus %d turno(s) han sido cancelados. Motivo: %s. Por favor, vuelva a solicitar turnos.", appointments.size(), cancellationReason);
                        webSocketNotificationService.sendNotificationToUser(
                                affectedUser.getId(),
                                NotificationType.APPOINTMENT_CANCELLED_BY_ADMIN,
                                appointments.size() == 1 ? "Turno Cancelado" : "Turnos Cancelados",
                                message,
                                RelatedEntityType.APPOINTMENT,
                                appointments.get(0).getAppointmentId(),
                                appointments.get(0).getAppointmentId());
                    } else {
                        emailService.sendDaysClosedNotificationEmail(affectedUser, appointments);
                    }
                } catch (Exception e) {
                    logger.error("Error al enviar notificación/email por bloqueo - Usuario ID: {}, Error: {}", entry.getKey(), e.getMessage(), e);
                }
            }
        } catch (Exception e) {
            logger.error("Error al procesar turnos afectados por bloqueo: {}", e.getMessage(), e);
        }

        // 10. Enviar actualización de disponibilidad en tiempo real para la fecha afectada - Tiempo Real
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(saved.getBlockDate());
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                saved.getBlockDate(), e.getMessage(), e);
        }

        // 11. Convertir a response
        return toResponse(saved, affectedAppointments);
    }

    @Override
    @Transactional(readOnly = true)
    public List<ManualBlockResponse> getActiveBlocks() {
        List<ManualBlock> blocks = repository.findByActiveTrue();
        if (blocks == null) {
            return List.of();
        }
        return blocks.stream()
            .map(b -> toResponse(b, List.of()))
            .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public ManualBlockResponse updateBlock(Long id, ManualBlockRequest request, Long userId) {
        ManualBlock block = repository.findById(id)
            .orElseThrow(() -> new ApiException("Bloqueo no encontrado", HttpStatus.NOT_FOUND));
        if (!Boolean.TRUE.equals(block.getActive())) {
            throw new ApiException("No se puede editar un bloqueo eliminado", HttpStatus.BAD_REQUEST);
        }
        validateManualBlockRequest(request);
        validateDateNotInPast(request.getDate());
        validateIsFullDayAndTimeRange(request);
        if (!request.getIsFullDay() && request.getTimeRange() != null) {
            validateTimeRange(request.getTimeRange());
        }
        List<ManualBlockResponse.AffectedAppointmentInfo> affectedAppointments = detectAffectedAppointments(request);
        if (!request.getAffectsExistingAppointments() && !affectedAppointments.isEmpty()) {
            throw new ApiException(
                "Existen turnos en el rango bloqueado. Debe resolverlos primero o establecer 'affectsExistingAppointments' en true.",
                HttpStatus.CONFLICT);
        }
        block.setBlockDate(request.getDate());
        block.setIsFullDay(request.getIsFullDay());
        block.setReason(request.getReason());
        block.setAffectsExistingAppointments(request.getAffectsExistingAppointments());
        if (request.getIsFullDay()) {
            block.setTimeRange(null);
        } else if (request.getTimeRange() != null) {
            block.setTimeRange(mapper.toTimeRange(request.getTimeRange()));
        }
        ManualBlock saved = repository.save(block);
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(saved.getBlockDate());
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}", saved.getBlockDate(), e);
        }
        return toResponse(saved, affectedAppointments);
    }

    @Override
    @Transactional
    public void deactivateBlock(Long id) {
        ManualBlock block = repository.findById(id)
            .orElseThrow(() -> new ApiException("Bloqueo no encontrado", HttpStatus.NOT_FOUND));
        LocalDate blockDate = block.getBlockDate();
        repository.delete(block);
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(blockDate);
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}", blockDate, e);
        }
        logger.info("Bloqueo eliminado - ID: {}", id);
    }

    /**
     * Valida que el request tenga la estructura correcta.
     */
    private void validateManualBlockRequest(ManualBlockRequest request) {
        if (request == null) {
            throw new ApiException("El bloqueo no puede ser nulo", HttpStatus.BAD_REQUEST);
        }

        if (request.getDate() == null) {
            throw new ApiException("La fecha es obligatoria", HttpStatus.BAD_REQUEST);
        }

        if (request.getIsFullDay() == null) {
            throw new ApiException("El campo isFullDay es obligatorio", HttpStatus.BAD_REQUEST);
        }

        if (request.getReason() == null || request.getReason().trim().isEmpty()) {
            throw new ApiException("El motivo es obligatorio", HttpStatus.BAD_REQUEST);
        }

        if (request.getAffectsExistingAppointments() == null) {
            throw new ApiException("El campo affectsExistingAppointments es obligatorio", HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida que la fecha no sea pasada.
     * Usa la zona horaria GMT-3 para determinar la fecha actual.
     */
    private void validateDateNotInPast(LocalDate date) {
        LocalDate today = ZonedDateTime.now(ZONE_ID).toLocalDate();
        
        if (date.isBefore(today)) {
            throw new ApiException(
                "No se pueden crear bloqueos para fechas pasadas",
                HttpStatus.BAD_REQUEST);
        }
        
        logger.debug("Fecha validada correctamente - Fecha: {}, Hoy: {}", date, today);
    }

    /**
     * Valida las reglas de isFullDay y timeRange:
     * - Si isFullDay = true, no debe tener timeRange
     * - Si isFullDay = false, debe tener timeRange
     */
    private void validateIsFullDayAndTimeRange(ManualBlockRequest request) {
        Boolean isFullDay = request.getIsFullDay();
        TimeRangeRequest timeRange = request.getTimeRange();

        if (Boolean.TRUE.equals(isFullDay)) {
            // Si es día completo, no debe tener rango horario
            if (timeRange != null) {
                throw new ApiException(
                    "Si el bloqueo es para día completo (isFullDay = true), no debe proporcionar un rango horario",
                    HttpStatus.BAD_REQUEST);
            }
        } else {
            // Si no es día completo, debe tener rango horario
            if (timeRange == null) {
                throw new ApiException(
                    "Si el bloqueo es para un rango horario (isFullDay = false), debe proporcionar un rango horario",
                    HttpStatus.BAD_REQUEST);
            }
        }
    }

    /**
     * Valida que el rango horario sea válido.
     */
    private void validateTimeRange(TimeRangeRequest timeRangeRequest) {
        if (timeRangeRequest == null) {
            return;
        }

        TimeRange timeRange = mapper.toTimeRange(timeRangeRequest);
        
        if (!timeRange.isValid()) {
            throw new ApiException(
                String.format("El rango horario '%s - %s' no es válido. El horario de inicio debe ser anterior al horario de fin.",
                    timeRange.getStart(), timeRange.getEnd()),
                HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Detecta turnos existentes que se ven afectados por el bloqueo.
     * 
     * Busca turnos en estados activos (CREATED, CONFIRMED) que:
     * - Estén en la fecha del bloqueo
     * - Y estén en el rango horario bloqueado (o todo el día si isFullDay = true)
     * 
     * @param request Request del bloqueo
     * @return Lista de turnos afectados
     */
    private List<ManualBlockResponse.AffectedAppointmentInfo> detectAffectedAppointments(ManualBlockRequest request) {
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = 
            List.of(
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED
            );
        
        // Buscar todos los turnos activos en la fecha del bloqueo
        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = 
            appointmentRepository.findByDateAndStateIn(request.getDate(), activeStates);
        
        if (appointments.isEmpty()) {
            logger.debug("No hay turnos activos en la fecha: {}", request.getDate());
            return new ArrayList<>();
        }
        
        // Si es bloqueo día completo, todos los turnos están afectados
        if (Boolean.TRUE.equals(request.getIsFullDay())) {
            return appointments.stream()
                .map(a -> new ManualBlockResponse.AffectedAppointmentInfo(
                    a.getId(),
                    a.getUserId(),
                    a.getAppointmentDate(),
                    a.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")),
                    a.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm"))
                ))
                .collect(Collectors.toList());
        }
        
        // Si es bloqueo parcial, verificar que el turno esté en el rango horario bloqueado
        if (request.getTimeRange() == null) {
            logger.warn("Bloqueo parcial sin rango horario - Fecha: {}", request.getDate());
            return new ArrayList<>();
        }
        
        try {
            LocalTime blockStart = LocalTime.parse(request.getTimeRange().getStart(), DateTimeFormatter.ofPattern("HH:mm"));
            LocalTime blockEnd = LocalTime.parse(request.getTimeRange().getEnd(), DateTimeFormatter.ofPattern("HH:mm"));
            
            return appointments.stream()
                .filter(a -> {
                    LocalTime appointmentStart = a.getStartTime();
                    // Verificar si el turno se superpone con el bloqueo
                    // Un turno está afectado si su hora de inicio está dentro del rango bloqueado
                    return !appointmentStart.isBefore(blockStart) && appointmentStart.isBefore(blockEnd);
                })
                .map(a -> new ManualBlockResponse.AffectedAppointmentInfo(
                    a.getId(),
                    a.getUserId(),
                    a.getAppointmentDate(),
                    a.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")),
                    a.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm"))
                ))
                .collect(Collectors.toList());
        } catch (Exception e) {
            logger.error("Error al parsear rango horario del bloqueo: {}", e.getMessage(), e);
            return new ArrayList<>();
        }
    }

    /**
     * Convierte ManualBlockRequest a entidad ManualBlock.
     */
    private ManualBlock toEntity(ManualBlockRequest request, Long userId) {
        TimeRange timeRange = null;
        if (!request.getIsFullDay() && request.getTimeRange() != null) {
            timeRange = mapper.toTimeRange(request.getTimeRange());
        }

        return new ManualBlock(
            request.getDate(),
            request.getIsFullDay(),
            timeRange,
            request.getReason(),
            request.getAffectsExistingAppointments(),
            userId
        );
    }

    /**
     * Convierte entidad ManualBlock a ManualBlockResponse.
     * Null-safe para evitar NPE con datos legacy o inconsistentes.
     */
    private ManualBlockResponse toResponse(ManualBlock block, List<ManualBlockResponse.AffectedAppointmentInfo> affectedAppointments) {
        if (block == null) {
            return null;
        }
        ManualBlockResponse response = new ManualBlockResponse();
        response.setId(block.getId());
        response.setBlockDate(block.getBlockDate());
        response.setIsFullDay(block.getIsFullDay() != null ? block.getIsFullDay() : true);
        response.setReason(block.getReason() != null ? block.getReason() : "");
        response.setActive(block.getActive() != null ? block.getActive() : true);
        response.setAffectsExistingAppointments(block.getAffectsExistingAppointments() != null ? block.getAffectsExistingAppointments() : false);
        response.setCreatedByUserId(block.getCreatedByUserId());
        response.setCreatedAt(block.getCreatedAt());
        response.setUpdatedAt(block.getUpdatedAt());
        response.setAffectedAppointments(affectedAppointments != null ? affectedAppointments : List.of());

        // Convertir timeRange solo si existe y es bloqueo parcial (evita embeddable con columnas null)
        if (Boolean.FALSE.equals(block.getIsFullDay()) && block.getTimeRange() != null) {
            response.setTimeRange(mapper.toTimeRangeResponse(block.getTimeRange()));
        }

        return response;
    }
}

