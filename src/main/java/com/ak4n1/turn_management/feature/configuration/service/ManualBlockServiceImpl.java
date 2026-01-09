package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.dto.request.ManualBlockRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.TimeRangeRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.ManualBlockResponse;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
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

    public ManualBlockServiceImpl(ManualBlockRepository repository,
                                 CalendarConfigurationMapper mapper,
                                 AppointmentRepository appointmentRepository,
                                 WebSocketNotificationService webSocketNotificationService) {
        this.repository = repository;
        this.mapper = mapper;
        this.appointmentRepository = appointmentRepository;
        this.webSocketNotificationService = webSocketNotificationService;
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

        // 9. Notificar a usuarios afectados y admins
        try {
            // Notificar a admins
            webSocketNotificationService.sendNotificationToAdmins(
                NotificationType.BLOCK_CREATED,
                "Bloqueo Operativo Creado",
                String.format("Se ha creado un bloqueo operativo para el %s (%s). Turnos afectados: %d", 
                    saved.getBlockDate(), 
                    saved.getIsFullDay() ? "día completo" : "rango horario",
                    affectedAppointments.size()),
                RelatedEntityType.MANUAL_BLOCK,
                saved.getId()
            );

            // Notificar a usuarios con turnos afectados
            if (!affectedAppointments.isEmpty()) {
                List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = 
                    List.of(
                        com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                        com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED
                    );
                
                List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> affectedAppointmentsList = 
                    appointmentRepository.findByDateAndStateIn(saved.getBlockDate(), activeStates);
                
                // Filtrar solo los que están en el rango bloqueado (si es bloqueo parcial)
                if (!saved.getIsFullDay() && saved.getTimeRange() != null) {
                    try {
                        java.time.LocalTime blockStart = java.time.LocalTime.parse(saved.getTimeRange().getStart(), 
                            java.time.format.DateTimeFormatter.ofPattern("HH:mm"));
                        java.time.LocalTime blockEnd = java.time.LocalTime.parse(saved.getTimeRange().getEnd(), 
                            java.time.format.DateTimeFormatter.ofPattern("HH:mm"));
                        
                        affectedAppointmentsList = affectedAppointmentsList.stream()
                            .filter(a -> {
                                java.time.LocalTime appointmentStart = a.getStartTime();
                                return !appointmentStart.isBefore(blockStart) && appointmentStart.isBefore(blockEnd);
                            })
                            .collect(java.util.stream.Collectors.toList());
                    } catch (Exception e) {
                        logger.warn("Error al filtrar turnos por rango horario: {}", e.getMessage());
                    }
                }
                
                for (var appointment : affectedAppointmentsList) {
                    webSocketNotificationService.sendNotificationToUser(
                        appointment.getUserId(),
                        NotificationType.BLOCK_CREATED,
                        "Bloqueo Operativo - Turno Afectado",
                        String.format("Se ha creado un bloqueo operativo para el %s que afecta tu turno del %s a las %s. Por favor, contacta con el administrador.",
                            saved.getBlockDate(),
                            appointment.getAppointmentDate(),
                            appointment.getStartTime()),
                        RelatedEntityType.APPOINTMENT,
                        appointment.getId(),
                        appointment.getId()
                    );
                }
            }
        } catch (Exception e) {
            logger.error("Error al enviar notificaciones WebSocket de bloqueo: {}", e.getMessage(), e);
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
     */
    private ManualBlockResponse toResponse(ManualBlock block, List<ManualBlockResponse.AffectedAppointmentInfo> affectedAppointments) {
        ManualBlockResponse response = new ManualBlockResponse();
        response.setId(block.getId());
        response.setBlockDate(block.getBlockDate());
        response.setIsFullDay(block.getIsFullDay());
        response.setReason(block.getReason());
        response.setActive(block.getActive());
        response.setAffectsExistingAppointments(block.getAffectsExistingAppointments());
        response.setCreatedByUserId(block.getCreatedByUserId());
        response.setCreatedAt(block.getCreatedAt());
        response.setUpdatedAt(block.getUpdatedAt());
        response.setAffectedAppointments(affectedAppointments);

        // Convertir timeRange si existe
        if (block.getTimeRange() != null) {
            response.setTimeRange(mapper.toTimeRangeResponse(block.getTimeRange()));
        }

        return response;
    }
}

