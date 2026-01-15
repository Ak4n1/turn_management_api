package com.ak4n1.turn_management.feature.configuration.service.evaluation;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedDayResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.util.DateUtils;
import com.ak4n1.turn_management.feature.configuration.util.DayNameUtils;
import org.springframework.stereotype.Service;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de evaluación de días.
 * Evalúa la disponibilidad de días aplicando reglas de precedencia.
 */
@Service
public class DayEvaluationServiceImpl implements DayEvaluationService {

    private final AppointmentRepository appointmentRepository;
    private final CalendarConfigurationMapper mapper;

    public DayEvaluationServiceImpl(AppointmentRepository appointmentRepository,
                                   CalendarConfigurationMapper mapper) {
        this.appointmentRepository = appointmentRepository;
        this.mapper = mapper;
    }

    @Override
    public ConsolidatedDayResponse evaluateDay(LocalDate date, CalendarConfiguration config,
                                              List<CalendarException> exceptions,
                                              List<ManualBlock> blocks) {
        // 0. Si la fecha es pasada, siempre retornar CLOSED (días pasados no son relevantes para gestión futura)
        LocalDate today = DateUtils.getTodayGMT3();
        if (date.isBefore(today)) {
            ConsolidatedDayResponse pastDay = new ConsolidatedDayResponse();
            pastDay.setDate(date);
            pastDay.setState("CLOSED");
            pastDay.setRuleType("BASE");
            pastDay.setRuleDescription("Día pasado - Cerrado");
            pastDay.setTimeRanges(new ArrayList<>());
            pastDay.setHasExistingAppointments(false);
            pastDay.setAppointmentsCount(0);
            return pastDay;
        }

        // 1. Verificar bloqueos (prioridad máxima)
        Optional<ManualBlock> blockForDate = blocks.stream()
                .filter(b -> b.getBlockDate().equals(date))
                .findFirst();

        if (blockForDate.isPresent()) {
            return evaluateDayWithBlock(date, blockForDate.get());
        }

        // 2. Verificar excepciones
        Optional<CalendarException> exceptionForDate = exceptions.stream()
                .filter(e -> e.getExceptionDate().equals(date))
                .findFirst();

        if (exceptionForDate.isPresent()) {
            return evaluateDayWithException(date, exceptionForDate.get());
        }

        // 3. Usar configuración base
        return evaluateDayWithBase(date, config);
    }

    /**
     * Evalúa un día que tiene un bloqueo operativo.
     */
    private ConsolidatedDayResponse evaluateDayWithBlock(LocalDate date, ManualBlock block) {
        ConsolidatedDayResponse day = new ConsolidatedDayResponse();
        day.setDate(date);
        day.setRuleType("BLOCK");

        if (block.getIsFullDay()) {
            // Bloqueo día completo
            day.setState("CLOSED");
            day.setRuleDescription(String.format("%s - Bloqueo operativo (día completo)", block.getReason()));
            day.setTimeRanges(new ArrayList<>());
        } else {
            // Bloqueo rango horario
            day.setState("PARTIAL");
            day.setRuleDescription(String.format("%s - Bloqueo operativo (rango horario)", block.getReason()));
            // Por ahora no calculamos los rangos disponibles después de restar el bloqueo
            // En el futuro se podría calcular qué rangos quedan disponibles
            day.setTimeRanges(new ArrayList<>());
        }

        Boolean hasAppointments = hasExistingAppointments(date);
        Integer appointmentsCount = countExistingAppointments(date);
        day.setHasExistingAppointments(hasAppointments);
        day.setAppointmentsCount(appointmentsCount);
        return day;
    }

    /**
     * Evalúa un día que tiene una excepción.
     */
    private ConsolidatedDayResponse evaluateDayWithException(LocalDate date, CalendarException exception) {
        ConsolidatedDayResponse day = new ConsolidatedDayResponse();
        day.setDate(date);
        day.setRuleType("EXCEPTION");

        if (!exception.getIsOpen()) {
            // Excepción cerrada
            day.setState("CLOSED");
            day.setRuleDescription(String.format("%s - Excepción", exception.getReason()));
            day.setTimeRanges(new ArrayList<>());
        } else {
            // Excepción abierta
            List<TimeRangeResponse> timeRanges = exception.getTimeRanges().stream()
                    .filter(tr -> tr != null)
                    .map(mapper::toTimeRangeResponse)
                    .filter(tr -> tr != null)
                    .collect(Collectors.toList());

            day.setTimeRanges(timeRanges);
            day.setState(timeRanges.isEmpty() ? "CLOSED" : "OPEN");
            day.setRuleDescription(String.format("%s - Excepción", exception.getReason()));
        }

        Boolean hasAppointments = hasExistingAppointments(date);
        Integer appointmentsCount = countExistingAppointments(date);
        day.setHasExistingAppointments(hasAppointments);
        day.setAppointmentsCount(appointmentsCount);
        return day;
    }

    /**
     * Evalúa un día usando la configuración base.
     */
    private ConsolidatedDayResponse evaluateDayWithBase(LocalDate date, CalendarConfiguration activeConfig) {
        ConsolidatedDayResponse day = new ConsolidatedDayResponse();
        day.setDate(date);
        day.setRuleType("BASE");

        WeeklyConfig weeklyConfig = activeConfig.getWeeklyConfig();
        if (weeklyConfig == null) {
            // No hay configuración semanal (caso edge)
            day.setState("CLOSED");
            day.setRuleDescription("Sin configuración - Día cerrado");
            day.setTimeRanges(new ArrayList<>());
            Boolean hasAppointments = hasExistingAppointments(date);
            Integer appointmentsCount = countExistingAppointments(date);
            day.setHasExistingAppointments(hasAppointments);
            day.setAppointmentsCount(appointmentsCount);
            return day;
        }

        // Obtener día de la semana (1=Lunes, 7=Domingo)
        DayOfWeek dayOfWeek = date.getDayOfWeek();
        int dayOfWeekNumber = dayOfWeek.getValue(); // Java DayOfWeek: 1=Lunes, 7=Domingo

        Boolean isDayOpen = weeklyConfig.isDayOpen(dayOfWeekNumber);

        if (isDayOpen == null || !isDayOpen) {
            // Día cerrado en configuración base
            day.setState("CLOSED");
            String dayName = DayNameUtils.getDayName(dayOfWeekNumber);
            day.setRuleDescription(String.format("%s - Cerrado",
                    DayNameUtils.capitalizeFirst(dayName)));
            day.setTimeRanges(new ArrayList<>());
        } else {
            // Día abierto en configuración base - buscar horarios
            List<DailyHours> dailyHoursList = activeConfig.getDailyHours();
            Optional<DailyHours> dailyHoursForDay = dailyHoursList != null ? dailyHoursList.stream()
                    .filter(dh -> dh.getDayOfWeek().equals(dayOfWeekNumber))
                    .findFirst() : Optional.empty();

            // Verificar si hay turnos existentes ANTES de determinar el estado
            // Esto es importante para días que están abiertos pero no tienen horarios
            // configurados
            Boolean hasAppointments = hasExistingAppointments(date);
            Integer appointmentsCount = countExistingAppointments(date);

            if (dailyHoursForDay.isPresent() &&
                    dailyHoursForDay.get().getTimeRanges() != null &&
                    !dailyHoursForDay.get().getTimeRanges().isEmpty()) {

                // Día abierto con horarios
                List<TimeRangeResponse> timeRanges = dailyHoursForDay.get().getTimeRanges().stream()
                        .filter(tr -> tr != null)
                        .map(mapper::toTimeRangeResponse)
                        .filter(tr -> tr != null)
                        .collect(Collectors.toList());

                day.setTimeRanges(timeRanges);
                day.setState(timeRanges.isEmpty() ? "CLOSED" : "OPEN");
                String dayName = DayNameUtils.getDayName(dayOfWeekNumber);
                day.setRuleDescription(String.format("%s - Abierto",
                        DayNameUtils.capitalizeFirst(dayName)));
            } else {
                // Día abierto en configuración semanal pero sin horarios diarios configurados
                // Si tiene turnos existentes, mantenerlo como OPEN (estaba abierto cuando se
                // crearon los turnos)
                // Si no tiene turnos, marcarlo como CLOSED (no se pueden crear nuevos turnos
                // sin horarios)
                if (Boolean.TRUE.equals(hasAppointments) && appointmentsCount != null && appointmentsCount > 0) {
                    // Tiene turnos existentes - mantener como OPEN para indicar que el día está
                    // operativo
                    day.setState("OPEN");
                    String dayName = DayNameUtils.getDayName(dayOfWeekNumber);
                    day.setRuleDescription(
                            String.format("%s - Abierto (sin horarios configurados, pero con turnos existentes)",
                                    DayNameUtils.capitalizeFirst(dayName)));
                    day.setTimeRanges(new ArrayList<>());
                } else {
                    // Sin turnos y sin horarios - cerrado
                    day.setState("CLOSED");
                    String dayName = DayNameUtils.getDayName(dayOfWeekNumber);
                    day.setRuleDescription(String.format("%s - Cerrado",
                            DayNameUtils.capitalizeFirst(dayName)));
                    day.setTimeRanges(new ArrayList<>());
                }
            }

            day.setHasExistingAppointments(hasAppointments);
            day.setAppointmentsCount(appointmentsCount);
        }

        // Si el día está cerrado, también calcular turnos existentes
        if ("CLOSED".equals(day.getState())) {
            Boolean hasAppointments = hasExistingAppointments(date);
            Integer appointmentsCount = countExistingAppointments(date);
            day.setHasExistingAppointments(hasAppointments);
            day.setAppointmentsCount(appointmentsCount);
        }

        return day;
    }

    @Override
    public Boolean hasExistingAppointments(LocalDate date) {
        // Si la fecha es pasada, no hay turnos "existentes" relevantes para el calendario
        LocalDate today = DateUtils.getTodayGMT3();
        if (date.isBefore(today)) {
            return false;
        }

        List<AppointmentState> confirmedStates = List.of(AppointmentState.CONFIRMED);

        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findByDateAndStateIn(date, confirmedStates);

        return !appointments.isEmpty();
    }

    @Override
    public Integer countExistingAppointments(LocalDate date) {
        // Si la fecha es pasada, no hay turnos "existentes" relevantes para el calendario
        LocalDate today = DateUtils.getTodayGMT3();
        if (date.isBefore(today)) {
            return 0;
        }

        // Contar solo turnos CONFIRMED para el calendario general
        List<AppointmentState> confirmedStates = List.of(AppointmentState.CONFIRMED);

        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findByDateAndStateIn(date, confirmedStates);

        return appointments.size();
    }
}
