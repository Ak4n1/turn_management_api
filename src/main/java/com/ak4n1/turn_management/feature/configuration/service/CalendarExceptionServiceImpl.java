package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.TimeRangeRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarExceptionResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
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
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de excepciones de calendario.
 * 
 * Responsabilidades:
 * - Validar excepciones
 * - Calcular impacto (turnos afectados)
 * - Asegurar que no haya duplicados
 * - Mantener integridad de datos
 */
@Service
public class CalendarExceptionServiceImpl implements CalendarExceptionService {

    private static final Logger logger = LoggerFactory.getLogger(CalendarExceptionServiceImpl.class);
    
    /**
     * Zona horaria del sistema: GMT-3 (Argentina)
     */
    private static final ZoneId ZONE_ID = ZoneId.of("America/Argentina/Buenos_Aires");

    private final CalendarExceptionRepository repository;
    private final CalendarConfigurationMapper mapper;
    private final AppointmentRepository appointmentRepository;
    private final WebSocketNotificationService webSocketNotificationService;

    public CalendarExceptionServiceImpl(CalendarExceptionRepository repository,
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
    public CalendarExceptionResponse createException(CalendarExceptionRequest request, Long userId) {
        logger.info("Creando excepción de calendario - Usuario: {}, Fecha: {}, IsOpen: {}", 
            userId, request.getDate(), request.getIsOpen());

        // 1. Validar request básico
        validateCalendarExceptionRequest(request);

        // 2. Validar que la fecha no sea pasada (GMT-3)
        validateDateNotInPast(request.getDate());

        // 3. Validar que no exista excepción duplicada
        validateNoDuplicate(request.getDate());

        // 4. Validar reglas de isOpen y timeRanges
        validateIsOpenAndTimeRanges(request);

        // 5. Validar horarios si están presentes
        if (request.getIsOpen() && request.getTimeRanges() != null && !request.getTimeRanges().isEmpty()) {
            validateTimeRanges(request.getTimeRanges());
        }

        // 6. Convertir request a entidad
        CalendarException exception = toEntity(request, userId);

        // 7. Guardar
        CalendarException saved = repository.save(exception);
        logger.info("Excepción de calendario creada exitosamente - ID: {}, Fecha: {}", 
            saved.getId(), saved.getExceptionDate());

        // 8. Calcular impacto (turnos afectados)
        Integer affectedAppointmentsCount = calculateImpact(saved.getExceptionDate());

        // 9. Notificar a usuarios afectados y admins
        try {
            // Notificar a admins
            webSocketNotificationService.sendNotificationToAdmins(
                NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                "Excepción de Calendario Creada",
                String.format("Se ha creado una excepción para el %s (%s). Turnos afectados: %d", 
                    saved.getExceptionDate(), 
                    saved.getIsOpen() ? "abierto" : "cerrado",
                    affectedAppointmentsCount),
                RelatedEntityType.CALENDAR_EXCEPTION,
                saved.getId()
            );

            // Notificar a usuarios con turnos afectados
            if (affectedAppointmentsCount > 0) {
                List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = 
                    List.of(
                        com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                        com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED
                    );
                
                List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> affectedAppointments = 
                    appointmentRepository.findByDateAndStateIn(saved.getExceptionDate(), activeStates);
                
                for (var appointment : affectedAppointments) {
                    webSocketNotificationService.sendNotificationToUser(
                        appointment.getUserId(),
                        NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                        "Cambio en Calendario",
                        String.format("Se ha creado una excepción para el %s que afecta tu turno del %s a las %s. Por favor, revisa los detalles.",
                            saved.getExceptionDate(),
                            appointment.getAppointmentDate(),
                            appointment.getStartTime()),
                        RelatedEntityType.APPOINTMENT,
                        appointment.getId(),
                        appointment.getId()
                    );
                }
            }
        } catch (Exception e) {
            logger.error("Error al enviar notificaciones WebSocket de excepción: {}", e.getMessage(), e);
        }

        // 10. Enviar actualización de disponibilidad en tiempo real para la fecha afectada - Tiempo Real
        try {
            webSocketNotificationService.broadcastAvailabilityUpdate(saved.getExceptionDate());
        } catch (Exception e) {
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {}. Error: {}", 
                saved.getExceptionDate(), e.getMessage(), e);
        }

        // 11. Convertir a response
        return toResponse(saved, affectedAppointmentsCount);
    }

    /**
     * Valida que el request tenga la estructura correcta.
     */
    private void validateCalendarExceptionRequest(CalendarExceptionRequest request) {
        if (request == null) {
            throw new ApiException("La excepción no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        if (request.getDate() == null) {
            throw new ApiException("La fecha es obligatoria", HttpStatus.BAD_REQUEST);
        }

        if (request.getIsOpen() == null) {
            throw new ApiException("El campo isOpen es obligatorio", HttpStatus.BAD_REQUEST);
        }

        if (request.getReason() == null || request.getReason().trim().isEmpty()) {
            throw new ApiException("El motivo es obligatorio", HttpStatus.BAD_REQUEST);
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
                "No se pueden crear excepciones para fechas pasadas",
                HttpStatus.BAD_REQUEST);
        }
        
        logger.debug("Fecha validada correctamente - Fecha: {}, Hoy: {}", date, today);
    }

    /**
     * Valida que no exista una excepción duplicada para la misma fecha.
     */
    private void validateNoDuplicate(LocalDate date) {
        if (repository.existsByExceptionDate(date)) {
            throw new ApiException(
                "Ya existe una excepción para esta fecha",
                HttpStatus.CONFLICT);
        }
        
        logger.debug("No existe excepción duplicada para la fecha: {}", date);
    }

    /**
     * Valida las reglas de isOpen y timeRanges:
     * - Si isOpen = true, debe tener timeRanges
     * - Si isOpen = false, no debe tener timeRanges
     */
    private void validateIsOpenAndTimeRanges(CalendarExceptionRequest request) {
        Boolean isOpen = request.getIsOpen();
        List<TimeRangeRequest> timeRanges = request.getTimeRanges();

        if (Boolean.TRUE.equals(isOpen)) {
            // Si está abierto, debe tener al menos un rango horario
            if (timeRanges == null || timeRanges.isEmpty()) {
                throw new ApiException(
                    "Si el día está abierto (isOpen = true), debe proporcionar al menos un rango horario",
                    HttpStatus.BAD_REQUEST);
            }
        } else {
            // Si está cerrado, no debe tener rangos horarios
            if (timeRanges != null && !timeRanges.isEmpty()) {
                throw new ApiException(
                    "Si el día está cerrado (isOpen = false), no debe proporcionar rangos horarios",
                    HttpStatus.BAD_REQUEST);
            }
        }
    }

    /**
     * Valida que los rangos horarios sean válidos:
     * - Formato correcto (HH:mm)
     * - Start < End
     * - No superposiciones
     */
    private void validateTimeRanges(List<TimeRangeRequest> timeRangeRequests) {
        if (timeRangeRequests == null || timeRangeRequests.isEmpty()) {
            return;
        }

        // Convertir a entidades TimeRange para validar
        List<TimeRange> timeRanges = timeRangeRequests.stream()
            .map(mapper::toTimeRange)
            .collect(Collectors.toList());

        // Validar que todos los rangos sean válidos
        for (TimeRange timeRange : timeRanges) {
            if (!timeRange.isValid()) {
                throw new ApiException(
                    String.format("El rango horario '%s - %s' no es válido. El horario de inicio debe ser anterior al horario de fin.",
                        timeRange.getStart(), timeRange.getEnd()),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Validar que no haya superposiciones
        for (int i = 0; i < timeRanges.size(); i++) {
            for (int j = i + 1; j < timeRanges.size(); j++) {
                if (timeRanges.get(i).overlaps(timeRanges.get(j))) {
                    throw new ApiException(
                        String.format("Los rangos horarios no pueden superponerse. Los rangos '%s - %s' y '%s - %s' se superponen.",
                            timeRanges.get(i).getStart(), timeRanges.get(i).getEnd(),
                            timeRanges.get(j).getStart(), timeRanges.get(j).getEnd()),
                        HttpStatus.BAD_REQUEST);
                }
            }
        }

        logger.debug("{} rangos horarios validados correctamente", timeRanges.size());
    }

    /**
     * Calcula el impacto de la excepción (cantidad de turnos afectados).
     * 
     * Busca turnos en estados activos (CREATED, CONFIRMED) en la fecha de la excepción.
     * 
     * @param exceptionDate Fecha de la excepción
     * @return Cantidad de turnos afectados
     */
    private Integer calculateImpact(LocalDate exceptionDate) {
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = 
            List.of(
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED
            );
        
        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = 
            appointmentRepository.findByDateAndStateIn(exceptionDate, activeStates);
        
        int count = appointments.size();
        logger.debug("Calculando impacto para fecha: {} - Turnos afectados: {}", exceptionDate, count);
        return count;
    }

    /**
     * Convierte CalendarExceptionRequest a entidad CalendarException.
     */
    private CalendarException toEntity(CalendarExceptionRequest request, Long userId) {
        CalendarException exception = new CalendarException(
            request.getDate(),
            request.getIsOpen(),
            request.getReason(),
            userId
        );

        // Convertir timeRanges si existen
        if (request.getIsOpen() && request.getTimeRanges() != null && !request.getTimeRanges().isEmpty()) {
            List<TimeRange> timeRanges = request.getTimeRanges().stream()
                .map(mapper::toTimeRange)
                .collect(Collectors.toList());
            exception.setTimeRanges(timeRanges);
        } else {
            exception.setTimeRanges(new ArrayList<>());
        }

        return exception;
    }

    /**
     * Convierte entidad CalendarException a CalendarExceptionResponse.
     */
    private CalendarExceptionResponse toResponse(CalendarException exception, Integer affectedAppointmentsCount) {
        CalendarExceptionResponse response = new CalendarExceptionResponse();
        response.setId(exception.getId());
        response.setExceptionDate(exception.getExceptionDate());
        response.setIsOpen(exception.getIsOpen());
        response.setReason(exception.getReason());
        response.setActive(exception.getActive());
        response.setCreatedByUserId(exception.getCreatedByUserId());
        response.setCreatedAt(exception.getCreatedAt());
        response.setUpdatedAt(exception.getUpdatedAt());
        response.setAffectedAppointmentsCount(affectedAppointmentsCount);

        // Convertir timeRanges
        if (exception.getTimeRanges() != null && !exception.getTimeRanges().isEmpty()) {
            List<TimeRangeResponse> timeRangeResponses = exception.getTimeRanges().stream()
                .map(mapper::toTimeRangeResponse)
                .collect(Collectors.toList());
            response.setTimeRanges(timeRangeResponses);
        } else {
            response.setTimeRanges(new ArrayList<>());
        }

        return response;
    }
}

