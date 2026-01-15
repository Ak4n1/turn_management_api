package com.ak4n1.turn_management.feature.configuration.service.slots;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedDayResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.feature.configuration.service.evaluation.DayEvaluationService;
import com.ak4n1.turn_management.feature.configuration.util.DateUtils;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de generación de slots disponibles.
 */
@Service
public class SlotGenerationServiceImpl implements SlotGenerationService {

    private static final Logger logger = LoggerFactory.getLogger(SlotGenerationServiceImpl.class);

    private final CalendarConfigurationRepository repository;
    private final CalendarExceptionRepository exceptionRepository;
    private final ManualBlockRepository manualBlockRepository;
    private final AppointmentRepository appointmentRepository;
    private final DayEvaluationService dayEvaluationService;

    public SlotGenerationServiceImpl(CalendarConfigurationRepository repository,
                                    CalendarExceptionRepository exceptionRepository,
                                    ManualBlockRepository manualBlockRepository,
                                    AppointmentRepository appointmentRepository,
                                    DayEvaluationService dayEvaluationService) {
        this.repository = repository;
        this.exceptionRepository = exceptionRepository;
        this.manualBlockRepository = manualBlockRepository;
        this.appointmentRepository = appointmentRepository;
        this.dayEvaluationService = dayEvaluationService;
    }

    @Override
    @Transactional(readOnly = true)
    public SlotsResponse getAvailableSlots(LocalDate date) {
        logger.info("Solicitud de slots disponibles para fecha: {}", date);

        // 1. Validar que la fecha no sea pasada
        LocalDate today = DateUtils.getTodayGMT3();
        if (date.isBefore(today)) {
            throw new ApiException(
                    "No se pueden generar slots para fechas pasadas. Fecha solicitada: " + date,
                    HttpStatus.BAD_REQUEST);
        }

        // 2. Obtener configuración activa para duración y evaluar día directamente
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. El sistema no está configurado.",
                        HttpStatus.SERVICE_UNAVAILABLE));

        Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
        if (appointmentDurationMinutes == null) {
            throw new ApiException(
                    "No hay duración de turnos configurada. Debe configurar la duración antes de generar slots.",
                    HttpStatus.BAD_REQUEST);
        }

        // 3. Obtener excepciones y bloqueos para esta fecha
        List<CalendarException> exceptions = exceptionRepository
                .findByActiveTrueAndExceptionDateBetween(date, date);
        List<ManualBlock> blocks = manualBlockRepository
                .findByActiveTrueAndBlockDateBetween(date, date);

        // 4. Evaluar el día aplicando precedencia para obtener rangos horarios
        ConsolidatedDayResponse dayInfo = dayEvaluationService.evaluateDay(date, activeConfig, exceptions, blocks);

        // 5. Si el día está cerrado, retornar respuesta con información de turnos existentes
        if ("CLOSED".equals(dayInfo.getState())) {
            Integer existingAppointmentsCount = dayEvaluationService.countExistingAppointments(date);

            SlotsResponse response = new SlotsResponse();
            response.setDate(date);
            response.setSlots(new ArrayList<>());
            response.setTotalSlots(0);
            response.setAvailableSlots(0);

            if (existingAppointmentsCount > 0) {
                // Día cerrado CON turnos existentes
                response.setHasExistingAppointments(true);
                response.setExistingAppointmentsCount(existingAppointmentsCount);
                response.setMessage(
                        String.format(
                                "Este día está cerrado según la configuración actual, pero tiene %d turno(s) existente(s) " +
                                        "creado(s) con una configuración anterior. No se pueden crear nuevos turnos para este día. " +
                                        "Los turnos existentes seguirán siendo válidos y pueden ser atendidos.",
                                existingAppointmentsCount));
            } else {
                // Día cerrado SIN turnos existentes
                response.setHasExistingAppointments(false);
                response.setExistingAppointmentsCount(0);
                response.setMessage(
                        String.format(
                                "La fecha %s no tiene horarios disponibles para generar slots. %s",
                                date,
                                dayInfo.getRuleDescription()));
            }

            logger.info("Slots solicitados para día cerrado - Fecha: {}, Turnos existentes: {}",
                    date, existingAppointmentsCount);

            return response;
        }

        // 6. Si hay bloqueo parcial, necesitamos obtener los rangos horarios base antes del bloqueo
        List<TimeRangeResponse> timeRangesToUse = dayInfo.getTimeRanges();

        // Si hay bloqueo parcial y no hay rangos en dayInfo, obtener rangos base
        // (porque los bloqueos no calculan rangos disponibles automáticamente parciales)
        if ((timeRangesToUse == null || timeRangesToUse.isEmpty()) &&
                "PARTIAL".equals(dayInfo.getState()) &&
                !blocks.isEmpty()) {
            // Obtener rangos horarios base antes de aplicar bloqueo
            // Primero verificar si hay excepción
            List<CalendarException> exceptionsForDate = exceptions.stream()
                    .filter(e -> e.getExceptionDate().equals(date))
                    .collect(Collectors.toList());

            if (!exceptionsForDate.isEmpty() && Boolean.TRUE.equals(exceptionsForDate.get(0).getIsOpen())) {
                // Si hay excepción abierta, usar sus rangos
                timeRangesToUse = exceptionsForDate.get(0).getTimeRanges().stream()
                        .map(tr -> new TimeRangeResponse(tr.getStart(), tr.getEnd()))
                        .collect(Collectors.toList());
            } else {
                // Usar rangos base (sin excepciones ni bloqueos)
                ConsolidatedDayResponse baseDayInfo = dayEvaluationService.evaluateDay(date, activeConfig, new ArrayList<>(), new ArrayList<>());
                if (baseDayInfo.getTimeRanges() != null && !baseDayInfo.getTimeRanges().isEmpty()) {
                    timeRangesToUse = baseDayInfo.getTimeRanges();
                }
            }
        }

        // 7. Verificar que haya rangos horarios disponibles
        if (timeRangesToUse == null || timeRangesToUse.isEmpty()) {
            throw new ApiException(
                    String.format("La fecha %s no tiene horarios disponibles para generar slots. %s",
                            date, dayInfo.getRuleDescription()),
                    HttpStatus.BAD_REQUEST);
        }

        // 8. Generar slots desde los rangos horarios disponibles
        List<SlotResponse> allSlots = new ArrayList<>();
        for (TimeRangeResponse timeRange : timeRangesToUse) {
            List<SlotResponse> slotsFromRange = generateSlotsFromRange(
                    timeRange, appointmentDurationMinutes, blocks);
            allSlots.addAll(slotsFromRange);
        }

        // 9. Ordenar slots por hora de inicio
        allSlots.sort(Comparator.comparing(SlotResponse::getStart));

        // 10. Excluir slots ocupados por turnos existentes
        List<SlotResponse> availableSlots = excludeOccupiedSlots(allSlots, date);
        int availableCount = (int) availableSlots.stream()
                .filter(SlotResponse::getAvailable)
                .count();

        logger.info("Slots generados - Fecha: {}, Total: {}, Disponibles: {}",
                date, allSlots.size(), availableCount);

        // 11. Construir respuesta
        SlotsResponse response = new SlotsResponse();
        response.setDate(date);
        response.setSlots(availableSlots);
        response.setTotalSlots(allSlots.size());
        response.setAvailableSlots(availableCount);

        // 12. Si el día es PARTIAL, verificar si hay turnos existentes afectados por el bloqueo
        if ("PARTIAL".equals(dayInfo.getState()) && !blocks.isEmpty()) {
            Integer existingAppointmentsCount = dayEvaluationService.countExistingAppointments(date);

            if (existingAppointmentsCount > 0) {
                // Obtener rango horario del bloqueo para verificar si afecta turnos
                ManualBlock block = blocks.get(0);

                if (block.getTimeRange() != null) {
                    LocalTime blockStart = LocalTime.parse(block.getTimeRange().getStart(),
                            DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime blockEnd = LocalTime.parse(block.getTimeRange().getEnd(),
                            DateTimeFormatter.ofPattern("HH:mm"));

                    // Contar turnos que intersectan con el bloqueo
                    List<AppointmentState> activeStates = Arrays.asList(
                            AppointmentState.CREATED,
                            AppointmentState.CONFIRMED);

                    long affectedAppointments = appointmentRepository
                            .findByDateAndStateIn(date, activeStates)
                            .stream()
                            .filter(app -> {
                                LocalTime appStart = app.getStartTime();
                                LocalTime appEnd = app.getEndTime();
                                // Verificar si el turno intersecta con el bloqueo
                                return (appStart.isBefore(blockEnd) && appEnd.isAfter(blockStart));
                            })
                            .count();

                    if (affectedAppointments > 0) {
                        response.setHasExistingAppointments(true);
                        response.setExistingAppointmentsCount((int) affectedAppointments);
                        response.setMessage(
                                String.format(
                                        "Este día tiene un bloqueo parcial (%s-%s) y %d turno(s) existente(s) que podrían verse afectados.",
                                        blockStart.format(DateTimeFormatter.ofPattern("HH:mm")),
                                        blockEnd.format(DateTimeFormatter.ofPattern("HH:mm")),
                                        affectedAppointments));
                        return response;
                    }
                }
            }
        }

        return response;
    }

    @Override
    public List<SlotResponse> generateSlotsFromRange(TimeRangeResponse timeRange,
                                                     Integer appointmentDurationMinutes,
                                                     List<ManualBlock> blocks) {
        List<SlotResponse> slots = new ArrayList<>();

        try {
            LocalTime startTime = LocalTime.parse(timeRange.getStart(), DateTimeFormatter.ofPattern("HH:mm"));
            LocalTime endTime = LocalTime.parse(timeRange.getEnd(), DateTimeFormatter.ofPattern("HH:mm"));

            LocalTime currentStart = startTime;
            while (currentStart.plusMinutes(appointmentDurationMinutes).isBefore(endTime) ||
                    currentStart.plusMinutes(appointmentDurationMinutes).equals(endTime)) {

                LocalTime slotEnd = currentStart.plusMinutes(appointmentDurationMinutes);

                // Verificar si el slot está bloqueado
                boolean isBlocked = isSlotBlocked(currentStart, slotEnd, blocks);

                SlotResponse slot = new SlotResponse(
                        currentStart.format(DateTimeFormatter.ofPattern("HH:mm")),
                        slotEnd.format(DateTimeFormatter.ofPattern("HH:mm")),
                        !isBlocked);

                slots.add(slot);

                // Avanzar al siguiente slot
                currentStart = currentStart.plusMinutes(appointmentDurationMinutes);
            }
        } catch (Exception e) {
            logger.warn("Error al generar slots desde rango {}: {}", timeRange, e.getMessage());
        }

        return slots;
    }

    @Override
    public List<SlotResponse> excludeOccupiedSlots(List<SlotResponse> slots, LocalDate date) {
        List<AppointmentState> activeStates = List.of(
                AppointmentState.CREATED,
                AppointmentState.CONFIRMED);

        // Buscar todos los turnos activos para esta fecha
        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findByDateAndStateIn(date, activeStates);

        // Crear un set de horas de inicio ocupadas para búsqueda rápida
        java.util.Set<String> occupiedStartTimes = appointments.stream()
                .map(a -> a.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")))
                .collect(Collectors.toSet());

        // Marcar slots ocupados
        return slots.stream()
                .map(slot -> {
                    if (occupiedStartTimes.contains(slot.getStart())) {
                        // Slot ocupado
                        return new SlotResponse(slot.getStart(), slot.getEnd(), false);
                    }
                    // Slot disponible
                    return slot;
                })
                .collect(Collectors.toList());
    }

    @Override
    public boolean isSlotBlocked(LocalTime slotStart, LocalTime slotEnd, List<ManualBlock> blocks) {
        if (blocks == null || blocks.isEmpty()) {
            return false;
        }

        for (ManualBlock block : blocks) {
            // Si es bloqueo día completo, todos los slots están bloqueados
            if (Boolean.TRUE.equals(block.getIsFullDay())) {
                return true;
            }

            // Si es bloqueo parcial, verificar si el slot se superpone con el bloqueo
            if (block.getTimeRange() != null) {
                try {
                    LocalTime blockStart = LocalTime.parse(block.getTimeRange().getStart(),
                            DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime blockEnd = LocalTime.parse(block.getTimeRange().getEnd(),
                            DateTimeFormatter.ofPattern("HH:mm"));

                    // Verificar superposición: slot se superpone si slotStart < blockEnd && slotEnd > blockStart
                    if (slotStart.isBefore(blockEnd) && slotEnd.isAfter(blockStart)) {
                        return true;
                    }
                } catch (Exception e) {
                    logger.warn("Error al verificar bloqueo: {}", e.getMessage());
                }
            }
        }

        return false;
    }
}
