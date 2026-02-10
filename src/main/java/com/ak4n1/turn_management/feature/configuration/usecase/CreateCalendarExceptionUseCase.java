package com.ak4n1.turn_management.feature.configuration.usecase;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarExceptionResponse;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.feature.configuration.service.cancellation.AppointmentCancellationService;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.feature.notification.service.EmailService;
import com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService;
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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * UseCase for creating calendar exceptions.
 * Encapsulates the logic for validating, saving, and handling impact of
 * exceptions.
 */
@Service
public class CreateCalendarExceptionUseCase {

    private static final Logger logger = LoggerFactory.getLogger(CreateCalendarExceptionUseCase.class);
    private static final ZoneId ZONE_ID = ZoneId.of("America/Argentina/Buenos_Aires");

    private final CalendarExceptionRepository repository;
    private final ManualBlockRepository manualBlockRepository;
    private final CalendarConfigurationMapper mapper;
    private final AppointmentRepository appointmentRepository;
    private final WebSocketNotificationService webSocketNotificationService;
    private final AppointmentCancellationService appointmentCancellationService;
    private final EmailService emailService;
    private final UserRepository userRepository;

    public CreateCalendarExceptionUseCase(CalendarExceptionRepository repository,
            ManualBlockRepository manualBlockRepository,
            CalendarConfigurationMapper mapper,
            AppointmentRepository appointmentRepository,
            WebSocketNotificationService webSocketNotificationService,
            AppointmentCancellationService appointmentCancellationService,
            EmailService emailService,
            UserRepository userRepository) {
        this.repository = repository;
        this.manualBlockRepository = manualBlockRepository;
        this.mapper = mapper;
        this.appointmentRepository = appointmentRepository;
        this.webSocketNotificationService = webSocketNotificationService;
        this.appointmentCancellationService = appointmentCancellationService;
        this.emailService = emailService;
        this.userRepository = userRepository;
    }

    @Transactional
    public CalendarExceptionResponse execute(CalendarExceptionRequest request, Long userId) {
        logger.info("Executing CreateCalendarExceptionUseCase - User: {}, Date: {}, IsOpen: {}",
                userId, request.getDate(), request.getIsOpen());

        // 1. Validation Logic
        validateRequest(request);

        // 2. Map to Entity
        CalendarException exception = toEntity(request, userId);

        // 3. Save Exception
        CalendarException saved = repository.save(exception);
        logger.info("Calendar exception saved successfully - ID: {}, Date: {}",
                saved.getId(), saved.getExceptionDate());

        // 4. Handle Affected Appointments
        // Logic change: If opening a day, assume no appointments can exist
        List<Appointment> affectedAppointments = new ArrayList<>();
        if (Boolean.FALSE.equals(saved.getIsOpen())) {
            affectedAppointments = getAffectedAppointmentsOnClosure(saved.getExceptionDate());
        } else {
            logger.info("Opening day via exception - Skipping affected appointments check for date: {}",
                    saved.getExceptionDate());
        }

        Integer affectedAppointmentsCount = affectedAppointments.size();

        // 5. Process Auto-Cancellation if requested
        if (affectedAppointmentsCount > 0) {
            handleCancellationsAndNotifications(saved, affectedAppointments, request, userId);
        }

        // 6. WebSocket Notifications
        notifyAdmins(saved, affectedAppointmentsCount);
        notifyAvailabilityUpdate(saved.getExceptionDate());

        // 7. Map to Response
        return mapper.toExceptionResponse(saved, affectedAppointmentsCount);
    }

    private void validateRequest(CalendarExceptionRequest request) {
        if (request == null)
            throw new ApiException("Request cannot be null", HttpStatus.BAD_REQUEST);
        if (request.getDate() == null)
            throw new ApiException("Date is required", HttpStatus.BAD_REQUEST);
        if (request.getIsOpen() == null)
            throw new ApiException("isOpen field is required", HttpStatus.BAD_REQUEST);
        if (request.getReason() == null || request.getReason().trim().isEmpty()) {
            throw new ApiException("Reason is required", HttpStatus.BAD_REQUEST);
        }

        // Past date validation
        LocalDate today = ZonedDateTime.now(ZONE_ID).toLocalDate();
        if (request.getDate().isBefore(today)) {
            throw new ApiException("Cannot create exceptions for past dates", HttpStatus.BAD_REQUEST);
        }

        // Duplicate validation
        if (repository.existsByExceptionDate(request.getDate())) {
            throw new ApiException("An exception already exists for this date", HttpStatus.CONFLICT);
        }

        // No exception on a date that already has a block (block has priority)
        if (!manualBlockRepository.findActiveBlocksByDate(request.getDate()).isEmpty()) {
            throw new ApiException("No se puede crear una excepción en una fecha que ya tiene un bloqueo operativo (prioridad máxima). Elimine o cambie el bloqueo si desea usar una excepción.", HttpStatus.CONFLICT);
        }

        // Logical validation for isOpen and ranges
        if (Boolean.TRUE.equals(request.getIsOpen())) {
            if (request.getTimeRanges() == null || request.getTimeRanges().isEmpty()) {
                throw new ApiException("If isOpen is true, at least one time range must be provided",
                        HttpStatus.BAD_REQUEST);
            }
            validateTimeRanges(request.getTimeRanges());
        } else {
            if (request.getTimeRanges() != null && !request.getTimeRanges().isEmpty()) {
                throw new ApiException("If isOpen is false, time ranges should not be provided",
                        HttpStatus.BAD_REQUEST);
            }
        }
    }

    private void validateTimeRanges(
            List<? extends com.ak4n1.turn_management.feature.configuration.dto.request.TimeRangeRequest> timeRangeRequests) {
        List<TimeRange> timeRanges = timeRangeRequests.stream()
                .map(mapper::toTimeRange)
                .collect(Collectors.toList());

        for (TimeRange range : timeRanges) {
            if (!range.isValid()) {
                throw new ApiException(String.format("Invalid range: %s - %s", range.getStart(), range.getEnd()),
                        HttpStatus.BAD_REQUEST);
            }
        }

        for (int i = 0; i < timeRanges.size(); i++) {
            for (int j = i + 1; j < timeRanges.size(); j++) {
                if (timeRanges.get(i).overlaps(timeRanges.get(j))) {
                    throw new ApiException(String.format("Overlapping ranges: %s-%s and %s-%s",
                            timeRanges.get(i).getStart(), timeRanges.get(i).getEnd(),
                            timeRanges.get(j).getStart(), timeRanges.get(j).getEnd()), HttpStatus.BAD_REQUEST);
                }
            }
        }
    }

    private List<Appointment> getAffectedAppointmentsOnClosure(LocalDate date) {
        List<AppointmentState> activeStates = List.of(AppointmentState.CREATED, AppointmentState.CONFIRMED);
        return appointmentRepository.findByDateAndStateIn(date, activeStates);
    }

    private void handleCancellationsAndNotifications(CalendarException saved, List<Appointment> affected,
            CalendarExceptionRequest request, Long userId) {
        Boolean autoCancel = Boolean.TRUE.equals(request.getAutoCancelAffectedAppointments());
        List<Long> idsToCancel = request.getAppointmentIdsToCancel();
        String reason = request.getCancellationReason();
        if (reason == null || reason.trim().isEmpty()) {
            reason = "Día cerrado según excepción de calendario (" + saved.getReason() + ")";
        }

        Set<Long> cancelledIds = new HashSet<>();
        List<Appointment> toCancel = new ArrayList<>();

        if (autoCancel) {
            toCancel = affected;
        } else if (idsToCancel != null && !idsToCancel.isEmpty()) {
            Set<Long> filter = new HashSet<>(idsToCancel);
            toCancel = affected.stream().filter(a -> filter.contains(a.getId())).collect(Collectors.toList());
        }

        if (!toCancel.isEmpty()) {
            cancelledIds = toCancel.stream().map(Appointment::getId).collect(Collectors.toSet());
            appointmentCancellationService.cancelAffectedAppointmentsByDayClosure(toCancel, userId, reason);
            sendAggregatedEmails(toCancel, reason);
        }

        // Notify via WebSocket those who weren't cancelled
        for (Appointment appointment : affected) {
            if (!cancelledIds.contains(appointment.getId())) {
                webSocketNotificationService.sendNotificationToUser(
                        appointment.getUserId(),
                        NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                        "Cambio en Calendario",
                        String.format("Se ha creado una excepción para el %s que afecta tu turno. Por favor, revisa.",
                                saved.getExceptionDate()),
                        RelatedEntityType.APPOINTMENT,
                        appointment.getId(),
                        appointment.getId());
            }
        }
    }

    private void sendAggregatedEmails(List<Appointment> appointments, String reason) {
        Map<Long, List<UserAffectedAppointmentInfo>> grouped = appointments.stream()
                .collect(Collectors.groupingBy(Appointment::getUserId,
                        Collectors
                                .mapping(
                                        a -> new UserAffectedAppointmentInfo(a.getId(), a.getAppointmentDate(),
                                                a.getStartTime().toString(), a.getEndTime().toString()),
                                        Collectors.toList())));

        grouped.forEach((userId, affectedApts) -> {
            userRepository.findById(userId).ifPresent(user -> {
                emailService.sendMassCancellationEmail(user, affectedApts, reason);

                String message = affectedApts.size() == 1
                        ? "Tu turno ha sido cancelado. Motivo: " + reason
                        : String.format("Tus %d turnos han sido cancelados. Motivo: %s", affectedApts.size(), reason);

                webSocketNotificationService.sendNotificationToUser(
                        user.getId(),
                        NotificationType.APPOINTMENT_CANCELLED_BY_ADMIN,
                        "Turnos Cancelados",
                        message,
                        RelatedEntityType.APPOINTMENT,
                        affectedApts.get(0).getAppointmentId(),
                        affectedApts.get(0).getAppointmentId());
            });
        });
    }

    private void notifyAdmins(CalendarException saved, int affectedCount) {
        webSocketNotificationService.sendNotificationToAdmins(
                NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                "Excepción de Calendario Creada",
                String.format("Excepción para %s (%s). Afectados: %d",
                        saved.getExceptionDate(), saved.getIsOpen() ? "abierto" : "cerrado", affectedCount),
                RelatedEntityType.CALENDAR_EXCEPTION,
                saved.getId());
    }

    private void notifyAvailabilityUpdate(LocalDate date) {
        webSocketNotificationService.broadcastAvailabilityUpdate(date);
    }

    private CalendarException toEntity(CalendarExceptionRequest request, Long userId) {
        CalendarException exception = new CalendarException(
                request.getDate(), request.getIsOpen(), request.getReason(), userId);

        if (Boolean.TRUE.equals(request.getIsOpen()) && request.getTimeRanges() != null) {
            exception.setTimeRanges(
                    request.getTimeRanges().stream().map(mapper::toTimeRange).collect(Collectors.toList()));
        } else {
            exception.setTimeRanges(new ArrayList<>());
        }
        return exception;
    }

}
