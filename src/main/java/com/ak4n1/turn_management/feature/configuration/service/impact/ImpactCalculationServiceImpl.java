package com.ak4n1.turn_management.feature.configuration.service.impact;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.ManualBlockRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.PreviewImpactRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.WeeklyConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.AffectedAppointmentInfo;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedDayResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.PreviewImpactResponse;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.feature.configuration.service.evaluation.DayEvaluationService;
import com.ak4n1.turn_management.feature.configuration.util.DayNameUtils;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class ImpactCalculationServiceImpl implements ImpactCalculationService {

    private static final Logger logger = LoggerFactory.getLogger(ImpactCalculationServiceImpl.class);

    private final CalendarConfigurationRepository configurationRepository;
    private final CalendarExceptionRepository exceptionRepository;
    private final ManualBlockRepository manualBlockRepository;
    private final AppointmentRepository appointmentRepository;
    private final UserService userService;
    private final DayEvaluationService dayEvaluationService;

    public ImpactCalculationServiceImpl(
            CalendarConfigurationRepository configurationRepository,
            CalendarExceptionRepository exceptionRepository,
            ManualBlockRepository manualBlockRepository,
            AppointmentRepository appointmentRepository,
            UserService userService,
            DayEvaluationService dayEvaluationService) {
        this.configurationRepository = configurationRepository;
        this.exceptionRepository = exceptionRepository;
        this.manualBlockRepository = manualBlockRepository;
        this.appointmentRepository = appointmentRepository;
        this.userService = userService;
        this.dayEvaluationService = dayEvaluationService;
    }

    @Override
    @Transactional(readOnly = true)
    public PreviewImpactResponse previewImpact(PreviewImpactRequest request) {
        logger.info("Solicitud de previsualización de impacto - Tipo: {}", request.getChangeType());

        // 1. Validar request y obtener rango de fechas
        LocalDate startDate = request.getStartDate();
        LocalDate endDate = request.getEndDate();
        LocalDate today = LocalDate.now();

        // Si no se especifica rango, usar próximos 90 días
        if (startDate == null) {
            startDate = today;
        }
        if (endDate == null) {
            endDate = today.plusDays(90);
        }

        // Validar rango
        if (startDate.isBefore(today)) {
            startDate = today; // No evaluar fechas pasadas
        }
        if (startDate.isAfter(endDate)) {
            throw new ApiException(
                    "La fecha de inicio no puede ser posterior a la fecha de fin",
                    HttpStatus.BAD_REQUEST);
        }

        long daysBetween = java.time.temporal.ChronoUnit.DAYS.between(startDate, endDate) + 1;
        if (daysBetween > 90) {
            endDate = startDate.plusDays(89); // Máximo 90 días
        }

        // 2. Obtener configuración actual
        CalendarConfiguration activeConfig = configurationRepository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. Debe crear primero una configuración semanal base.",
                        HttpStatus.BAD_REQUEST));

        // 3. Obtener excepciones y bloqueos actuales
        List<CalendarException> currentExceptions = exceptionRepository
                .findByActiveTrueAndExceptionDateBetween(startDate, endDate);
        List<ManualBlock> currentBlocks = manualBlockRepository
                .findByActiveTrueAndBlockDateBetween(startDate, endDate);

        // 4. Simular el cambio propuesto y calcular impacto
        ImpactCalculationResult result = calculateImpact(
                request, activeConfig, currentExceptions, currentBlocks, startDate, endDate);

        logger.info("Impacto calculado - Días afectados: {}, Slots perdidos: {}, Turnos afectados: {}",
                result.getAffectedDays(), result.getSlotsLost(), result.getExistingAppointmentsAffected());

        // 5. Construir respuesta
        PreviewImpactResponse response = new PreviewImpactResponse();
        response.setAffectedDays(result.getAffectedDays());
        response.setSlotsLost(result.getSlotsLost());
        response.setExistingAppointmentsAffected(result.getExistingAppointmentsAffected());
        response.setAppointments(result.getAppointments());
        response.setChangeDescription(result.getChangeDescription());

        return response;
    }

    /**
     * Calcula el impacto de los cambios propuestos.
     */
    private ImpactCalculationResult calculateImpact(
            PreviewImpactRequest request,
            CalendarConfiguration currentConfig,
            List<CalendarException> currentExceptions,
            List<ManualBlock> currentBlocks,
            LocalDate startDate,
            LocalDate endDate) {

        String changeType = request.getChangeType();

        return switch (changeType) {
            case "WEEKLY_CONFIG" -> calculateWeeklyConfigImpact(request, currentConfig, currentExceptions,
                    currentBlocks, startDate, endDate);
            case "DAILY_HOURS" -> calculateDailyHoursImpact(request, currentConfig, currentExceptions,
                    currentBlocks, startDate, endDate);
            case "APPOINTMENT_DURATION" -> calculateAppointmentDurationImpact(request, currentConfig, currentExceptions,
                    currentBlocks, startDate, endDate);
            case "EXCEPTION" -> calculateExceptionImpact(request, currentConfig, currentExceptions,
                    currentBlocks, startDate, endDate);
            case "BLOCK" -> calculateBlockImpact(request, currentConfig, currentExceptions,
                    currentBlocks, startDate, endDate);
            default -> throw new ApiException(
                    "Tipo de cambio no válido: " + changeType,
                    HttpStatus.BAD_REQUEST);
        };
    }

    /**
     * Calcula el impacto de un cambio en la configuración semanal.
     */
    private ImpactCalculationResult calculateWeeklyConfigImpact(
            PreviewImpactRequest request,
            CalendarConfiguration currentConfig,
            List<CalendarException> currentExceptions,
            List<ManualBlock> currentBlocks,
            LocalDate startDate,
            LocalDate endDate) {

        if (request.getWeeklyConfig() == null) {
            throw new ApiException("Configuración semanal propuesta es requerida", HttpStatus.BAD_REQUEST);
        }

        WeeklyConfigRequest proposedWeekly = request.getWeeklyConfig();
        WeeklyConfig currentWeekly = currentConfig.getWeeklyConfig();

        int affectedDays = 0;
        int slotsLost = 0;
        List<AffectedAppointmentInfo> affectedAppointments = new ArrayList<>();
        StringBuilder description = new StringBuilder("Cambio en configuración semanal: ");

        // Comparar cada día de la semana en el rango especificado
        LocalDate currentDateLoop = startDate;
        List<String> affectedDayNames = new ArrayList<>();
        List<LocalDate> daysBeingClosed = new ArrayList<>(); // Días que pasan de abierto a cerrado
        List<LocalDate> daysAlreadyClosedWithAppointments = new ArrayList<>(); // Días ya cerrados con turnos activos

        while (!currentDateLoop.isAfter(endDate)) {
            final LocalDate currentDate = currentDateLoop;
            // Verificar si hay bloqueo o excepción (tienen prioridad)
            boolean hasBlock = currentBlocks.stream()
                    .anyMatch(b -> b.getBlockDate().equals(currentDate));
            if (hasBlock) {
                currentDateLoop = currentDateLoop.plusDays(1);
                continue; // Los bloqueos tienen prioridad máxima, no se ven afectados
            }

            boolean hasException = currentExceptions.stream()
                    .anyMatch(e -> e.getExceptionDate().equals(currentDate));
            if (hasException) {
                currentDateLoop = currentDateLoop.plusDays(1);
                continue; // Las excepciones tienen prioridad, no se ven afectadas
            }

            // Evaluar día usando configuración actual vs propuesta
            DayOfWeek dayOfWeek = currentDate.getDayOfWeek();
            int dayOfWeekNumber = dayOfWeek.getValue();

            Boolean currentIsOpen = currentWeekly != null ? currentWeekly.isDayOpen(dayOfWeekNumber) : null;
            Boolean proposedIsOpen = getDayOpenFromRequest(proposedWeekly, dayOfWeekNumber);

            // Si cambia el estado (abierto/cerrado), es un día afectado
            if (!java.util.Objects.equals(currentIsOpen, proposedIsOpen)) {
                affectedDays++;

                // Si el día pasa de abierto a cerrado, perder slots y agregar a días que se
                // cierran
                if (Boolean.TRUE.equals(currentIsOpen) && Boolean.FALSE.equals(proposedIsOpen)) {
                    // Agregar este día a la lista de días que se están cerrando
                    daysBeingClosed.add(currentDate);

                    // Calcular slots actuales para este día
                    List<DailyHours> currentDailyHours = currentConfig.getDailyHours();
                    Optional<DailyHours> dailyHoursForDay = currentDailyHours.stream()
                            .filter(dh -> dh.getDayOfWeek().equals(dayOfWeekNumber))
                            .findFirst();

                    if (dailyHoursForDay.isPresent()) {
                        Integer currentDuration = currentConfig.getAppointmentDurationMinutes();
                        if (currentDuration != null) {
                            int currentSlots = countSlotsForDay(
                                    dailyHoursForDay.get().getTimeRanges(),
                                    currentDuration);
                            slotsLost += currentSlots;
                        }
                    }
                } else if (Boolean.FALSE.equals(currentIsOpen) && Boolean.TRUE.equals(proposedIsOpen)) {
                    // Si el día pasa de cerrado a abierto, no hay slots perdidos
                    // Pero sigue siendo un día afectado
                }

                // Agregar nombre del día a la lista para la descripción
                String dayName = DayNameUtils.getDayName(dayOfWeekNumber);
                affectedDayNames.add(DayNameUtils.capitalizeFirst(dayName));
            } else if (Boolean.FALSE.equals(proposedIsOpen) && Boolean.FALSE.equals(currentIsOpen)) {
                // CASO ESPECIAL: El día ya está cerrado y se propone mantenerlo cerrado
                // Verificar si hay turnos activos en este día que deberían ser considerados
                // Esto maneja el caso donde se cerró un día, no se cancelaron los turnos,
                // y luego se intenta cerrar el día nuevamente
                daysAlreadyClosedWithAppointments.add(currentDate);
            }

            currentDateLoop = currentDateLoop.plusDays(1);
        }

        // Construir descripción
        if (affectedDays > 0) {
            if (affectedDayNames.size() <= 3) {
                description.append(String.join(", ", affectedDayNames));
            } else {
                description.append(affectedDayNames.get(0))
                        .append(", ")
                        .append(affectedDayNames.get(1))
                        .append(" y otros ")
                        .append(affectedDays - 2)
                        .append(" días");
            }
        } else {
            description.append("No se detectaron cambios en el rango especificado");
        }

        // Calcular turnos afectados en días que se están cerrando
        // También incluir días que ya están cerrados pero tienen turnos activos
        // (caso: se cerró un día, no se cancelaron turnos, y se intenta cerrar
        // nuevamente)
        List<LocalDate> allDaysToCheck = new ArrayList<>(daysBeingClosed);

        // Verificar días ya cerrados que tienen turnos activos
        if (!daysAlreadyClosedWithAppointments.isEmpty()) {
            List<AffectedAppointmentInfo> appointmentsInClosedDays = calculateAffectedAppointmentsForDates(
                    daysAlreadyClosedWithAppointments);
            // Solo agregar días que realmente tienen turnos activos
            Set<LocalDate> datesWithAppointments = appointmentsInClosedDays.stream()
                    .map(AffectedAppointmentInfo::getDate)
                    .collect(Collectors.toSet());

            for (LocalDate date : daysAlreadyClosedWithAppointments) {
                if (datesWithAppointments.contains(date) && !allDaysToCheck.contains(date)) {
                    allDaysToCheck.add(date);
                }
            }
        }

        if (!allDaysToCheck.isEmpty()) {
            affectedAppointments = calculateAffectedAppointmentsForDates(allDaysToCheck);
        } else {
            // Si no hay días que se cierren o días cerrados con turnos, no hay turnos
            // afectados
            affectedAppointments = new ArrayList<>();
        }

        return new ImpactCalculationResult(
                affectedDays,
                slotsLost,
                affectedAppointments.size(),
                affectedAppointments,
                description.toString());
    }

    /**
     * Calcula el impacto de un cambio en horarios diarios.
     */
    private ImpactCalculationResult calculateDailyHoursImpact(
            PreviewImpactRequest request,
            CalendarConfiguration currentConfig,
            List<CalendarException> currentExceptions,
            List<ManualBlock> currentBlocks,
            LocalDate startDate,
            LocalDate endDate) {

        if (request.getDailyHours() == null) {
            throw new ApiException("Horarios diarios propuestos son requeridos", HttpStatus.BAD_REQUEST);
        }

        // Por simplicidad, calculamos impacto comparando slots totales
        // En una implementación más completa, compararíamos día por día
        int affectedDays = 0;
        int slotsLost = 0;
        List<AffectedAppointmentInfo> affectedAppointments = new ArrayList<>();
        String changeDescription = "Cambio en horarios diarios";

        // Obtener duración actual de turnos
        Integer currentDuration = currentConfig.getAppointmentDurationMinutes();

        // Calcular slots actuales
        int currentTotalSlots = 0;
        LocalDate currentDateLoop = startDate;
        while (!currentDateLoop.isAfter(endDate)) {
            final LocalDate date = currentDateLoop;
            // Ignorar bloqueos y excepciones
            boolean hasBlock = currentBlocks.stream()
                    .anyMatch(b -> b.getBlockDate().equals(date));
            boolean hasException = currentExceptions.stream()
                    .anyMatch(e -> e.getExceptionDate().equals(date));

            if (!hasBlock && !hasException) {
                DayOfWeek dayOfWeek = date.getDayOfWeek();
                int dayOfWeekNumber = dayOfWeek.getValue();

                WeeklyConfig weeklyConfig = currentConfig.getWeeklyConfig();
                if (weeklyConfig != null && Boolean.TRUE.equals(weeklyConfig.isDayOpen(dayOfWeekNumber))) {
                    List<DailyHours> currentDailyHours = currentConfig.getDailyHours();
                    Optional<DailyHours> dailyHoursForDay = currentDailyHours.stream()
                            .filter(dh -> dh.getDayOfWeek().equals(dayOfWeekNumber))
                            .findFirst();

                    if (dailyHoursForDay.isPresent()) {
                        int slots = countSlotsForDay(
                                dailyHoursForDay.get().getTimeRanges(),
                                currentDuration);
                        currentTotalSlots += slots;
                    }
                }
            }
            currentDateLoop = currentDateLoop.plusDays(1);
        }

        // Para horarios propuestos, necesitaríamos simular la nueva configuración
        // Por ahora, asumimos que el cambio podría afectar todos los días abiertos
        affectedDays = (int) java.time.temporal.ChronoUnit.DAYS.between(startDate, endDate) + 1;
        slotsLost = currentTotalSlots; // Estimación conservadora

        affectedAppointments = calculateAffectedAppointments(startDate, endDate);

        return new ImpactCalculationResult(
                affectedDays,
                slotsLost,
                affectedAppointments.size(),
                affectedAppointments,
                changeDescription);
    }

    /**
     * Calcula el impacto de un cambio en la duración de turnos.
     */
    private ImpactCalculationResult calculateAppointmentDurationImpact(
            PreviewImpactRequest request,
            CalendarConfiguration currentConfig,
            List<CalendarException> currentExceptions,
            List<ManualBlock> currentBlocks,
            LocalDate startDate,
            LocalDate endDate) {

        if (request.getAppointmentDurationMinutes() == null) {
            throw new ApiException("Duración de turnos propuesta es requerida", HttpStatus.BAD_REQUEST);
        }

        Integer proposedDuration = request.getAppointmentDurationMinutes();
        Integer currentDuration = currentConfig.getAppointmentDurationMinutes();

        if (currentDuration == null) {
            throw new ApiException("No hay duración configurada actualmente", HttpStatus.BAD_REQUEST);
        }

        int affectedDays = 0;
        int slotsLost = 0;
        List<AffectedAppointmentInfo> affectedAppointments = new ArrayList<>();

        // Comparar slots actuales vs propuestos
        LocalDate currentDateLoop = startDate;
        while (!currentDateLoop.isAfter(endDate)) {
            final LocalDate date = currentDateLoop;
            boolean hasBlock = currentBlocks.stream()
                    .anyMatch(b -> b.getBlockDate().equals(date));
            boolean hasException = currentExceptions.stream()
                    .anyMatch(e -> e.getExceptionDate().equals(date));

            if (!hasBlock && !hasException) {
                DayOfWeek dayOfWeek = date.getDayOfWeek();
                int dayOfWeekNumber = dayOfWeek.getValue();

                WeeklyConfig weeklyConfig = currentConfig.getWeeklyConfig();
                if (weeklyConfig != null && Boolean.TRUE.equals(weeklyConfig.isDayOpen(dayOfWeekNumber))) {
                    List<DailyHours> currentDailyHours = currentConfig.getDailyHours();
                    Optional<DailyHours> dailyHoursForDay = currentDailyHours.stream()
                            .filter(dh -> dh.getDayOfWeek().equals(dayOfWeekNumber))
                            .findFirst();

                    if (dailyHoursForDay.isPresent()) {
                        List<TimeRange> timeRanges = dailyHoursForDay.get().getTimeRanges();
                        int currentSlots = countSlotsForDay(timeRanges, currentDuration);
                        int proposedSlots = countSlotsForDay(timeRanges, proposedDuration);

                        if (currentSlots != proposedSlots) {
                            affectedDays++;
                            if (proposedSlots < currentSlots) {
                                slotsLost += (currentSlots - proposedSlots);
                            }
                        }
                    }
                }
            }
            currentDateLoop = currentDateLoop.plusDays(1);
        }

        affectedAppointments = calculateAffectedAppointments(startDate, endDate);

        String changeDescription = String.format(
                "Cambio en duración de turnos: %d minutos → %d minutos",
                currentDuration, proposedDuration);

        return new ImpactCalculationResult(
                affectedDays,
                slotsLost,
                affectedAppointments.size(),
                affectedAppointments,
                changeDescription);
    }

    /**
     * Calcula el impacto de una nueva excepción.
     */
    private ImpactCalculationResult calculateExceptionImpact(
            PreviewImpactRequest request,
            CalendarConfiguration currentConfig,
            List<CalendarException> currentExceptions,
            List<ManualBlock> currentBlocks,
            LocalDate startDate,
            LocalDate endDate) {

        if (request.getException() == null) {
            throw new ApiException("Excepción propuesta es requerida", HttpStatus.BAD_REQUEST);
        }

        CalendarExceptionRequest proposedException = request.getException();
        LocalDate exceptionDate = proposedException.getDate();

        // Solo calcular impacto si la fecha está en el rango
        if (exceptionDate.isBefore(startDate) || exceptionDate.isAfter(endDate)) {
            return new ImpactCalculationResult(0, 0, 0, new ArrayList<>(),
                    "Excepción fuera del rango evaluado");
        }

        int affectedDays = 0;
        int slotsLost = 0;
        List<AffectedAppointmentInfo> affectedAppointments = new ArrayList<>();

        // Si la fecha ya tiene un bloqueo, la excepción no tiene efecto
        boolean hasBlock = currentBlocks.stream()
                .anyMatch(b -> b.getBlockDate().equals(exceptionDate));
        if (hasBlock) {
            return new ImpactCalculationResult(0, 0, 0, new ArrayList<>(),
                    "La fecha ya tiene un bloqueo operativo (prioridad máxima)");
        }

        // Verificar estado actual del día
        ConsolidatedDayResponse currentDay = dayEvaluationService.evaluateDay(
                exceptionDate, currentConfig, currentExceptions, currentBlocks);

        // Calcular slots actuales si el día está abierto
        if ("OPEN".equals(currentDay.getState())) {
            int currentSlots = 0;
            Integer currentDuration = currentConfig.getAppointmentDurationMinutes();
            if (currentDay.getTimeRanges() != null && currentDuration != null) {
                List<TimeRange> currentRanges = currentDay.getTimeRanges().stream()
                        .map(tr -> new TimeRange(tr.getStart(), tr.getEnd()))
                        .collect(Collectors.toList());
                currentSlots = countSlotsForDay(currentRanges, currentDuration);
            }

            // Si la excepción cierra el día, se pierden todos los slots
            if (Boolean.FALSE.equals(proposedException.getIsOpen())) {
                affectedDays = 1;
                slotsLost = currentSlots;
            } else if (Boolean.TRUE.equals(proposedException.getIsOpen())) {
                // Si la excepción abre con diferentes horarios, calcular diferencia
                affectedDays = 1;
                // Por simplicidad, asumimos que se pierden algunos slots si cambian los rangos
                slotsLost = currentSlots / 2; // Estimación
            }
        } else if ("CLOSED".equals(currentDay.getState()) &&
                Boolean.TRUE.equals(proposedException.getIsOpen())) {
            // Si el día está cerrado y la excepción lo abre, no hay slots perdidos
            affectedDays = 1;
            slotsLost = 0;
        }

        affectedAppointments = calculateAffectedAppointments(exceptionDate, exceptionDate);

        String changeDescription = String.format(
                "Nueva excepción: %s (%s)",
                exceptionDate,
                proposedException.getIsOpen() ? "abierto" : "cerrado");

        return new ImpactCalculationResult(
                affectedDays,
                slotsLost,
                affectedAppointments.size(),
                affectedAppointments,
                changeDescription);
    }

    /**
     * Calcula el impacto de un nuevo bloqueo.
     */
    private ImpactCalculationResult calculateBlockImpact(
            PreviewImpactRequest request,
            CalendarConfiguration currentConfig,
            List<CalendarException> currentExceptions,
            List<ManualBlock> currentBlocks,
            LocalDate startDate,
            LocalDate endDate) {

        if (request.getBlock() == null) {
            throw new ApiException("Bloqueo propuesto es requerido", HttpStatus.BAD_REQUEST);
        }

        ManualBlockRequest proposedBlock = request.getBlock();
        LocalDate blockDate = proposedBlock.getDate();

        // Solo calcular impacto si la fecha está en el rango
        if (blockDate.isBefore(startDate) || blockDate.isAfter(endDate)) {
            return new ImpactCalculationResult(0, 0, 0, new ArrayList<>(),
                    "Bloqueo fuera del rango evaluado");
        }

        int affectedDays = 0;
        int slotsLost = 0;
        List<AffectedAppointmentInfo> affectedAppointments = new ArrayList<>();

        // Verificar estado actual del día (sin incluir el bloqueo propuesto)
        ConsolidatedDayResponse currentDay = dayEvaluationService.evaluateDay(
                blockDate, currentConfig, currentExceptions, currentBlocks);

        // Un bloqueo siempre afecta el día (prioridad máxima)
        affectedDays = 1;

        // Si el bloqueo es día completo, perder todos los slots actuales
        if (Boolean.TRUE.equals(proposedBlock.getIsFullDay())) {
            // Si el día está abierto o parcial, calcular slots perdidos
            if ("OPEN".equals(currentDay.getState()) || "PARTIAL".equals(currentDay.getState())) {
                Integer currentDuration = currentConfig.getAppointmentDurationMinutes();
                if (currentDay.getTimeRanges() != null && !currentDay.getTimeRanges().isEmpty()
                        && currentDuration != null) {
                    List<TimeRange> currentRanges = currentDay.getTimeRanges().stream()
                            .map(tr -> new TimeRange(tr.getStart(), tr.getEnd()))
                            .collect(Collectors.toList());
                    slotsLost = countSlotsForDay(currentRanges, currentDuration);
                }
            }
        } else {
            // Bloqueo parcial - calcular slots afectados en el rango
            if (proposedBlock.getTimeRange() != null) {
                TimeRange blockRange = new TimeRange(
                        proposedBlock.getTimeRange().getStart(),
                        proposedBlock.getTimeRange().getEnd());
                Integer durationMinutes = blockRange.getDurationMinutes();
                if (durationMinutes != null) {
                    Integer currentDuration = currentConfig.getAppointmentDurationMinutes();
                    if (currentDuration != null && durationMinutes >= currentDuration) {
                        slotsLost = durationMinutes / currentDuration;
                    }
                }
            }
        }

        affectedAppointments = calculateAffectedAppointments(blockDate, blockDate);

        String changeDescription = String.format(
                "Nuevo bloqueo operativo: %s (%s)",
                blockDate,
                proposedBlock.getIsFullDay() ? "día completo" : "rango horario");

        return new ImpactCalculationResult(
                affectedDays,
                slotsLost,
                affectedAppointments.size(),
                affectedAppointments,
                changeDescription);
    }

    /**
     * Cuenta los slots disponibles para un día dado sus rangos horarios y duración
     * de turnos.
     */
    private int countSlotsForDay(List<TimeRange> timeRanges, Integer appointmentDurationMinutes) {
        if (timeRanges == null || timeRanges.isEmpty() || appointmentDurationMinutes == null) {
            return 0;
        }

        int totalSlots = 0;
        for (TimeRange range : timeRanges) {
            if (range.isValid()) {
                Integer durationMinutes = range.getDurationMinutes();
                if (durationMinutes != null && durationMinutes >= appointmentDurationMinutes) {
                    totalSlots += durationMinutes / appointmentDurationMinutes;
                }
            }
        }
        return totalSlots;
    }

    /**
     * Obtiene si un día está abierto desde WeeklyConfigRequest.
     */
    private Boolean getDayOpenFromRequest(WeeklyConfigRequest request, int dayOfWeek) {
        return switch (dayOfWeek) {
            case 1 -> request.getMonday();
            case 2 -> request.getTuesday();
            case 3 -> request.getWednesday();
            case 4 -> request.getThursday();
            case 5 -> request.getFriday();
            case 6 -> request.getSaturday();
            case 7 -> request.getSunday();
            default -> false;
        };
    }

    @Override
    public List<AffectedAppointmentInfo> calculateAffectedAppointments(LocalDate startDate, LocalDate endDate) {
        List<AppointmentState> activeStates = List.of(
                AppointmentState.CREATED,
                AppointmentState.CONFIRMED);

        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findAllByAppointmentDateBetween(startDate, endDate);

        // Filtrar solo los que están en estados activos
        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> activeAppointments = appointments
                .stream()
                .filter(a -> activeStates.contains(a.getState()))
                .collect(Collectors.toList());

        // Convertir a DTOs con información de usuario
        return activeAppointments.stream()
                .map(a -> {
                    try {
                        // Obtener información del usuario
                        User user = userService.findById(a.getUserId())
                                .orElse(null);

                        if (user != null) {
                            return new AffectedAppointmentInfo(
                                    a.getId(),
                                    a.getAppointmentDate(),
                                    a.getStartTime(),
                                    user.getId(),
                                    user.getFirstName(),
                                    user.getLastName(),
                                    user.getEmail());
                        } else {
                            // Si no se encuentra el usuario, retornar sin información de usuario
                            return new AffectedAppointmentInfo(
                                    a.getId(),
                                    a.getAppointmentDate(),
                                    a.getStartTime());
                        }
                    } catch (Exception e) {
                        logger.warn("Error al obtener usuario para turno {}: {}", a.getId(), e.getMessage());
                        // Si hay error, retornar sin información de usuario
                        return new AffectedAppointmentInfo(
                                a.getId(),
                                a.getAppointmentDate(),
                                a.getStartTime());
                    }
                })
                .collect(Collectors.toList());
    }

    @Override
    public List<AffectedAppointmentInfo> calculateAffectedAppointmentsForDates(List<LocalDate> dates) {
        if (dates == null || dates.isEmpty()) {
            return new ArrayList<>();
        }

        List<AppointmentState> activeStates = List.of(
                AppointmentState.CREATED,
                AppointmentState.CONFIRMED);

        // Convertir lista de fechas a un Set para búsqueda eficiente
        Set<LocalDate> datesSet = new HashSet<>(dates);

        // Buscar todos los turnos en el rango de fechas (desde la más antigua hasta la
        // más reciente)
        LocalDate minDate = dates.stream().min(LocalDate::compareTo)
                .orElseThrow(() -> new ApiException("Lista de fechas vacía", HttpStatus.INTERNAL_SERVER_ERROR));
        LocalDate maxDate = dates.stream().max(LocalDate::compareTo)
                .orElseThrow(() -> new ApiException("Lista de fechas vacía", HttpStatus.INTERNAL_SERVER_ERROR));

        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findAllByAppointmentDateBetween(minDate, maxDate);

        // Filtrar solo los que están en estados activos Y en las fechas específicas que
        // se están cerrando
        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> activeAppointments = appointments
                .stream()
                .filter(a -> activeStates.contains(a.getState()))
                .filter(a -> datesSet.contains(a.getAppointmentDate()))
                .collect(Collectors.toList());

        // Convertir a DTOs con información de usuario
        return activeAppointments.stream()
                .map(a -> {
                    try {
                        // Obtener información del usuario
                        User user = userService.findById(a.getUserId())
                                .orElse(null);

                        if (user != null) {
                            return new AffectedAppointmentInfo(
                                    a.getId(),
                                    a.getAppointmentDate(),
                                    a.getStartTime(),
                                    user.getId(),
                                    user.getFirstName(),
                                    user.getLastName(),
                                    user.getEmail());
                        } else {
                            // Si no se encuentra el usuario, retornar sin información de usuario
                            return new AffectedAppointmentInfo(
                                    a.getId(),
                                    a.getAppointmentDate(),
                                    a.getStartTime());
                        }
                    } catch (Exception e) {
                        logger.warn("Error al obtener usuario para turno {}: {}", a.getId(), e.getMessage());
                        // Si hay error, retornar sin información de usuario
                        return new AffectedAppointmentInfo(
                                a.getId(),
                                a.getAppointmentDate(),
                                a.getStartTime());
                    }
                })
                .collect(Collectors.toList());
    }
}
