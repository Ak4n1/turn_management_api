package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarExceptionResponse;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.configuration.usecase.CreateCalendarExceptionUseCase;
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
 * Delega la creación de excepciones a CreateCalendarExceptionUseCase.
 */
@Service
public class CalendarExceptionServiceImpl implements CalendarExceptionService {

    private static final Logger logger = LoggerFactory.getLogger(CalendarExceptionServiceImpl.class);

    private final CalendarExceptionRepository repository;
    private final ManualBlockRepository manualBlockRepository;
    private final CalendarConfigurationMapper mapper;
    private final AppointmentRepository appointmentRepository;
    private final CreateCalendarExceptionUseCase createCalendarExceptionUseCase;

    public CalendarExceptionServiceImpl(CalendarExceptionRepository repository,
                                       ManualBlockRepository manualBlockRepository,
                                       CalendarConfigurationMapper mapper,
                                       AppointmentRepository appointmentRepository,
                                       CreateCalendarExceptionUseCase createCalendarExceptionUseCase) {
        this.repository = repository;
        this.manualBlockRepository = manualBlockRepository;
        this.mapper = mapper;
        this.appointmentRepository = appointmentRepository;
        this.createCalendarExceptionUseCase = createCalendarExceptionUseCase;
    }

    @Override
    @Transactional
    public CalendarExceptionResponse createException(CalendarExceptionRequest request, Long userId) {
        return createCalendarExceptionUseCase.execute(request, userId);
    }

    @Override
    public List<CalendarExceptionResponse> getAllActiveExceptions() {
        logger.info("Obteniendo todas las excepciones activas");
        
        List<CalendarException> exceptions = repository.findByActiveTrue();
        
        return exceptions.stream()
            .map(exception -> {
                // Calcular impacto (turnos afectados)
                Integer affectedAppointmentsCount = calculateImpact(exception.getExceptionDate());
                return mapper.toExceptionResponse(exception, affectedAppointmentsCount);
            })
            .sorted((a, b) -> a.getExceptionDate().compareTo(b.getExceptionDate()))
            .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public CalendarExceptionResponse updateException(Long id, CalendarExceptionRequest request, Long userId) {
        CalendarException exception = repository.findById(id)
                .orElseThrow(() -> new ApiException("Excepción no encontrada", HttpStatus.NOT_FOUND));

        if (!Boolean.TRUE.equals(exception.getActive())) {
            throw new ApiException("No se puede editar una excepción eliminada", HttpStatus.BAD_REQUEST);
        }

        validateUpdateRequest(request, id);

        exception.setExceptionDate(request.getDate());
        exception.setIsOpen(request.getIsOpen());
        exception.setReason(request.getReason());
        if (Boolean.TRUE.equals(request.getIsOpen()) && request.getTimeRanges() != null && !request.getTimeRanges().isEmpty()) {
            List<TimeRange> ranges = request.getTimeRanges().stream()
                    .map(mapper::toTimeRange)
                    .collect(Collectors.toList());
            exception.setTimeRanges(ranges);
        } else {
            exception.setTimeRanges(new ArrayList<>());
        }

        CalendarException saved = repository.save(exception);
        logger.info("Excepción actualizada - ID: {}, Fecha: {}", saved.getId(), saved.getExceptionDate());
        Integer affectedCount = calculateImpact(saved.getExceptionDate());
        return mapper.toExceptionResponse(saved, affectedCount);
    }

    @Override
    @Transactional
    public void deactivateException(Long id) {
        CalendarException exception = repository.findById(id)
                .orElseThrow(() -> new ApiException("Excepción no encontrada", HttpStatus.NOT_FOUND));
        repository.delete(exception);
        logger.info("Excepción eliminada - ID: {}", id);
    }

    private void validateUpdateRequest(CalendarExceptionRequest request, Long exceptionId) {
        if (request == null || request.getDate() == null || request.getIsOpen() == null) {
            throw new ApiException("Fecha e isOpen son obligatorios", HttpStatus.BAD_REQUEST);
        }
        if (request.getReason() == null || request.getReason().trim().isEmpty()) {
            throw new ApiException("El motivo es obligatorio", HttpStatus.BAD_REQUEST);
        }
        LocalDate today = ZonedDateTime.now(ZoneId.of("America/Argentina/Buenos_Aires")).toLocalDate();
        if (request.getDate().isBefore(today)) {
            throw new ApiException("No se pueden actualizar excepciones a fechas pasadas", HttpStatus.BAD_REQUEST);
        }
        if (repository.existsByExceptionDateAndIdNot(request.getDate(), exceptionId)) {
            throw new ApiException("Ya existe otra excepción para esta fecha", HttpStatus.CONFLICT);
        }
        if (!manualBlockRepository.findActiveBlocksByDate(request.getDate()).isEmpty()) {
            throw new ApiException("No se puede crear o actualizar una excepción en una fecha que ya tiene un bloqueo operativo (prioridad máxima). Elimine o cambie el bloqueo si desea usar una excepción.", HttpStatus.CONFLICT);
        }
        if (Boolean.TRUE.equals(request.getIsOpen())) {
            if (request.getTimeRanges() == null || request.getTimeRanges().isEmpty()) {
                throw new ApiException("Si el día está abierto, debe tener al menos un rango horario", HttpStatus.BAD_REQUEST);
            }
            List<TimeRange> ranges = request.getTimeRanges().stream().map(mapper::toTimeRange).collect(Collectors.toList());
            for (TimeRange r : ranges) {
                if (!r.isValid()) {
                    throw new ApiException("Rango horario inválido: " + r.getStart() + " - " + r.getEnd(), HttpStatus.BAD_REQUEST);
                }
            }
            for (int i = 0; i < ranges.size(); i++) {
                for (int j = i + 1; j < ranges.size(); j++) {
                    if (ranges.get(i).overlaps(ranges.get(j))) {
                        throw new ApiException("Los rangos horarios no pueden superponerse", HttpStatus.BAD_REQUEST);
                    }
                }
            }
        } else if (request.getTimeRanges() != null && !request.getTimeRanges().isEmpty()) {
            throw new ApiException("Si el día está cerrado, no debe tener rangos horarios", HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Calcula el impacto de la excepción (cantidad de turnos afectados).
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
        
        return appointmentRepository.findByDateAndStateIn(exceptionDate, activeStates).size();
    }
}
