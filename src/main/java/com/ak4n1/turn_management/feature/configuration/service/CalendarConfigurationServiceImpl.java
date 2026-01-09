package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.request.AppointmentDurationRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.ManualBlockRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.PreviewImpactRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.WeeklyConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.AffectedAppointmentInfo;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityRangeResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarConfigurationResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedCalendarResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedDayResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.DayAvailabilityResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.PreviewImpactResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationHistoryResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationVersionResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentHistoryRepository;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.feature.configuration.mapper.CalendarConfigurationMapper;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de configuración de calendario.
 * 
 * Responsabilidades:
 * - Validar configuraciones
 * - Gestionar versionado
 * - Asegurar que solo una configuración esté activa
 * - Mantener integridad de datos
 */
@Service
public class CalendarConfigurationServiceImpl implements CalendarConfigurationService {

    private static final Logger logger = LoggerFactory.getLogger(CalendarConfigurationServiceImpl.class);

    private final CalendarConfigurationRepository repository;
    private final CalendarExceptionRepository exceptionRepository;
    private final ManualBlockRepository manualBlockRepository;
    private final CalendarConfigurationMapper mapper;
    private final AppointmentRepository appointmentRepository;
    private final AppointmentHistoryRepository appointmentHistoryRepository;
    private final UserService userService;
    private final UserRepository userRepository;
    private final WebSocketNotificationService webSocketNotificationService;
    private final com.ak4n1.turn_management.feature.notification.service.EmailService emailService;

    public CalendarConfigurationServiceImpl(CalendarConfigurationRepository repository,
            CalendarExceptionRepository exceptionRepository,
            ManualBlockRepository manualBlockRepository,
            CalendarConfigurationMapper mapper,
            AppointmentRepository appointmentRepository,
            AppointmentHistoryRepository appointmentHistoryRepository,
            UserService userService,
            UserRepository userRepository,
            WebSocketNotificationService webSocketNotificationService,
            com.ak4n1.turn_management.feature.notification.service.EmailService emailService) {
        this.repository = repository;
        this.exceptionRepository = exceptionRepository;
        this.manualBlockRepository = manualBlockRepository;
        this.mapper = mapper;
        this.appointmentRepository = appointmentRepository;
        this.appointmentHistoryRepository = appointmentHistoryRepository;
        this.userService = userService;
        this.userRepository = userRepository;
        this.webSocketNotificationService = webSocketNotificationService;
        this.emailService = emailService;
    }

    @Override
    @Transactional
    public CalendarConfigurationResponse createWeeklyConfig(WeeklyConfigRequest request, Long userId) {
        logger.info("Creando nueva configuración semanal - Usuario: {}", userId);

        // 1. Validar request
        validateWeeklyConfigRequest(request);

        // 2. Convertir request a entidad embebida
        WeeklyConfig weeklyConfig = mapper.toWeeklyConfig(request);

        // 3. Validar lógica de negocio
        validateWeeklyConfig(weeklyConfig);

        // 4. Calcular nueva versión
        Integer newVersion = calculateNextVersion();

        // 5. Desactivar configuración anterior (si existe)
        deactivatePreviousConfiguration();

        // 6. Crear nueva configuración
        CalendarConfiguration newConfiguration = new CalendarConfiguration(
                weeklyConfig,
                userId,
                request.getNotes());
        newConfiguration.setVersion(newVersion);
        newConfiguration.setActive(true);

        // 7. Guardar
        CalendarConfiguration saved = repository.save(newConfiguration);
        logger.info("Configuración semanal creada exitosamente - ID: {}, Versión: {}", saved.getId(),
                saved.getVersion());

        // 8. Notificar a admins sobre el cambio de configuración
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                    NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                    "Configuración de Calendario Actualizada",
                    String.format(
                            "Se ha actualizado la configuración semanal base (Versión %d). Revisa el impacto en turnos existentes.",
                            saved.getVersion()),
                    RelatedEntityType.CALENDAR_CONFIGURATION,
                    saved.getId());
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de cambio de configuración: {}", e.getMessage(), e);
        }

        // 9. Enviar actualización general de disponibilidad en tiempo real - Tiempo
        // Real
        try {
            java.util.Map<String, Object> additionalData = new java.util.HashMap<>();
            additionalData.put("configurationVersion", saved.getVersion());
            webSocketNotificationService.broadcastGeneralAvailabilityUpdate(
                    String.format(
                            "La configuración del calendario ha cambiado (Versión %d). Por favor, actualiza la vista de disponibilidad.",
                            saved.getVersion()),
                    additionalData);
        } catch (Exception e) {
            logger.error("Error al enviar actualización general de disponibilidad WebSocket: {}", e.getMessage(), e);
        }

        // 10. Procesar turnos afectados usando IDs del previewImpact (si se
        // proporcionan)
        try {
            java.util.List<Long> appointmentIdsToCancel = request.getAppointmentIdsToCancel();
            Boolean autoCancel = Boolean.TRUE.equals(request.getAutoCancelAffectedAppointments());
            String cancellationReason = request.getCancellationReason();

            if (cancellationReason == null || cancellationReason.trim().isEmpty()) {
                cancellationReason = "Día cerrado según nueva configuración";
            }

            // Si no hay IDs de turnos, no hay nada que procesar
            if (appointmentIdsToCancel == null || appointmentIdsToCancel.isEmpty()) {
                logger.debug("No hay turnos afectados para procesar");
                return mapper.toResponse(saved);
            }

            logger.info("Procesando {} turno(s) afectado(s) - AutoCancel: {}", appointmentIdsToCancel.size(),
                    autoCancel);

            // Obtener todos los turnos afectados de una vez (batch query)
            List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> affectedAppointments = appointmentRepository
                    .findAllById(appointmentIdsToCancel)
                    .stream()
                    .filter(a -> a
                            .getState() == com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED
                            ||
                            a.getState() == com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED)
                    .collect(java.util.stream.Collectors.toList());

            if (affectedAppointments.isEmpty()) {
                logger.debug("No se encontraron turnos activos para los IDs proporcionados");
                return mapper.toResponse(saved);
            }

            // Agrupar turnos por usuario
            java.util.Map<Long, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo>> userAffectedAppointments = new java.util.HashMap<>();
            java.util.List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointmentsToCancel = new java.util.ArrayList<>();

            for (com.ak4n1.turn_management.feature.appointment.domain.Appointment appointment : affectedAppointments) {
                Long affectedUserId = appointment.getUserId();
                LocalDate appointmentDate = appointment.getAppointmentDate();

                userAffectedAppointments.computeIfAbsent(affectedUserId, k -> new java.util.ArrayList<>())
                        .add(new com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo(
                                appointment.getId(),
                                appointmentDate,
                                appointment.getStartTime().toString(),
                                appointment.getEndTime().toString()));

                if (autoCancel) {
                    appointmentsToCancel.add(appointment);
                }
            }

            // Cancelar turnos si está activado
            if (autoCancel && !appointmentsToCancel.isEmpty()) {
                cancelAffectedAppointmentsByDayClosure(appointmentsToCancel, userId, cancellationReason);
            }

            // Obtener todos los usuarios de una vez (batch query)
            java.util.Set<Long> affectedUserIds = userAffectedAppointments.keySet();
            java.util.Map<Long, User> usersMap = new java.util.HashMap<>();
            if (!affectedUserIds.isEmpty()) {
                List<User> affectedUsers = userRepository.findAllById(affectedUserIds);
                for (User user : affectedUsers) {
                    usersMap.put(user.getId(), user);
                }
            }

            // Enviar notificaciones agrupadas por usuario
            for (java.util.Map.Entry<Long, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo>> entry : userAffectedAppointments
                    .entrySet()) {
                try {
                    Long affectedUserId = entry.getKey();
                    java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> appointments = entry
                            .getValue();

                    User affectedUser = usersMap.get(affectedUserId);
                    if (affectedUser == null) {
                        logger.warn("Usuario no encontrado para userId={}", affectedUserId);
                        continue;
                    }

                    if (autoCancel) {
                        emailService.sendMassCancellationEmail(affectedUser, appointments, cancellationReason);

                        String message = appointments.size() == 1
                                ? String.format(
                                        "Tu turno para el %s a las %s ha sido cancelado. Motivo: %s. Por favor, vuelva a solicitar un turno en un día hábil.",
                                        appointments.get(0).getDate(), appointments.get(0).getStartTime(),
                                        cancellationReason)
                                : String.format(
                                        "Tus %d turno(s) han sido cancelados. Motivo: %s. Por favor, vuelva a solicitar turnos en días hábiles.",
                                        appointments.size(), cancellationReason);

                        webSocketNotificationService.sendNotificationToUser(
                                affectedUser.getId(),
                                NotificationType.APPOINTMENT_CANCELLED_BY_ADMIN,
                                appointments.size() == 1 ? "Turno Cancelado" : "Turnos Cancelados",
                                message,
                                RelatedEntityType.APPOINTMENT,
                                appointments.get(0).getAppointmentId(),
                                appointments.get(0).getAppointmentId());

                        logger.info("Cancelación enviada a usuario {} - {} turno(s) cancelado(s)", affectedUser.getId(),
                                appointments.size());
                    } else {
                        emailService.sendDaysClosedNotificationEmail(affectedUser, appointments);

                        String message = appointments.size() == 1
                                ? String.format(
                                        "El día %s ha sido cerrado según la nueva configuración, pero tu turno a las %s seguirá siendo válido. Si necesitas reprogramarlo, contacta al administrador.",
                                        appointments.get(0).getDate(), appointments.get(0).getStartTime())
                                : String.format(
                                        "Se han cerrado %d días según la nueva configuración, pero tus %d turno(s) siguen siendo válidos. Si necesitas reprogramarlos, contacta al administrador.",
                                        appointments.stream().map(a -> a.getDate())
                                                .collect(java.util.stream.Collectors.toSet()).size(),
                                        appointments.size());

                        webSocketNotificationService.sendNotificationToUser(
                                affectedUser.getId(),
                                NotificationType.DAY_CLOSED_WITH_APPOINTMENT,
                                appointments.size() == 1 ? "Día Cerrado - Turno Afectado"
                                        : "Días Cerrados - Turnos Afectados",
                                message,
                                RelatedEntityType.APPOINTMENT,
                                appointments.get(0).getAppointmentId(),
                                appointments.get(0).getAppointmentId());

                        logger.info("Notificación enviada a usuario {} - {} turno(s) afectado(s)", affectedUser.getId(),
                                appointments.size());
                    }
                } catch (Exception e) {
                    logger.error("Error al enviar notificación - Usuario ID: {}, Error: {}", entry.getKey(),
                            e.getMessage(), e);
                }
            }
        } catch (Exception e) {
            logger.error("Error al procesar turnos afectados: {}", e.getMessage(), e);
        }

        // 11. Convertir a response
        return mapper.toResponse(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public CalendarConfigurationResponse getActiveConfiguration() {
        Optional<CalendarConfiguration> active = repository.findByActiveTrue();
        return active.map(mapper::toResponse).orElse(null);
    }

    /**
     * Valida que el request tenga todos los campos requeridos.
     * Las validaciones de @NotNull en el DTO ya se ejecutan antes,
     * pero esta validación adicional asegura que los valores no sean null.
     */
    private void validateWeeklyConfigRequest(WeeklyConfigRequest request) {
        if (request == null) {
            throw new ApiException("La configuración no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        // Validar que todos los días estén presentes (no null)
        if (request.getMonday() == null || request.getTuesday() == null ||
                request.getWednesday() == null || request.getThursday() == null ||
                request.getFriday() == null || request.getSaturday() == null ||
                request.getSunday() == null) {
            throw new ApiException("Todos los días de la semana deben estar definidos (true o false)",
                    HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida la lógica de negocio de la configuración semanal.
     * 
     * Reglas:
     * - Al menos un día debe estar abierto (no tiene sentido tener todos los días
     * cerrados)
     * - No hay solapamientos lógicos (esto se verifica en el futuro cuando haya
     * horarios)
     */
    private void validateWeeklyConfig(WeeklyConfig weeklyConfig) {
        if (weeklyConfig == null) {
            throw new ApiException("La configuración semanal no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        // Validar que al menos un día esté abierto
        int openDays = weeklyConfig.countOpenDays();
        if (openDays == 0) {
            throw new ApiException(
                    "Debe haber al menos un día abierto en la semana. No se puede configurar todos los días como cerrados.",
                    HttpStatus.BAD_REQUEST);
        }

        logger.debug("Configuración semanal validada - Días abiertos: {}", openDays);
    }

    /**
     * Calcula la siguiente versión.
     * Si no hay configuraciones previas, empieza en 1.
     */
    private Integer calculateNextVersion() {
        Optional<Integer> maxVersion = repository.findMaxVersion();
        return maxVersion.map(v -> v + 1).orElse(1);
    }

    /**
     * Desactiva la configuración anterior (si existe).
     * Solo debe haber una configuración activa a la vez.
     */
    private void deactivatePreviousConfiguration() {
        Optional<CalendarConfiguration> activeConfig = repository.findByActiveTrue();
        if (activeConfig.isPresent()) {
            CalendarConfiguration previous = activeConfig.get();
            previous.setActive(false);
            repository.save(previous);
            logger.info("Configuración anterior desactivada - Versión: {}", previous.getVersion());
        }
    }

    @Override
    @Transactional
    public CalendarConfigurationResponse configureDailyHours(DailyHoursConfigRequest request, Long userId) {
        logger.info("Configurando horarios diarios - Usuario: {}", userId);

        // 1. Validar request
        validateDailyHoursConfigRequest(request);

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. Debe crear primero una configuración semanal base.",
                        HttpStatus.BAD_REQUEST));

        // 3. Convertir request a entidades DailyHours
        List<DailyHours> dailyHoursList = mapper.toDailyHoursList(request);

        // 4. Validar que solo días abiertos tengan horarios
        validateOnlyOpenDaysHaveHours(dailyHoursList, activeConfig.getWeeklyConfig());

        // 5. Validar que no haya superposiciones
        validateNoOverlaps(dailyHoursList);

        // 6. Validar formato y lógica de rangos
        validateTimeRanges(dailyHoursList);

        // 7. Calcular nueva versión
        Integer newVersion = calculateNextVersion();

        // 8. Desactivar configuración anterior
        deactivatePreviousConfiguration();

        // 9. Crear nueva configuración (copiando la configuración semanal base)
        CalendarConfiguration newConfiguration = new CalendarConfiguration(
                activeConfig.getWeeklyConfig(),
                userId,
                request.getNotes() != null ? request.getNotes() : "Configuración de horarios diarios");
        newConfiguration.setVersion(newVersion);
        newConfiguration.setActive(true);

        // 10. Asociar horarios diarios a la nueva configuración
        for (DailyHours dailyHours : dailyHoursList) {
            dailyHours.setCalendarConfiguration(newConfiguration);
        }
        newConfiguration.setDailyHours(dailyHoursList);

        // 11. Guardar
        CalendarConfiguration saved = repository.save(newConfiguration);
        logger.info("Horarios diarios configurados exitosamente - ID: {}, Versión: {}", saved.getId(),
                saved.getVersion());

        // 12. Notificar a admins sobre el cambio de configuración
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                    NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                    "Horarios Diarios Actualizados",
                    String.format(
                            "Se han actualizado los horarios diarios (Versión %d). Revisa el impacto en turnos existentes.",
                            saved.getVersion()),
                    RelatedEntityType.CALENDAR_CONFIGURATION,
                    saved.getId());
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de cambio de configuración: {}", e.getMessage(), e);
        }

        // 13. Enviar actualización general de disponibilidad en tiempo real - Tiempo
        // Real
        try {
            java.util.Map<String, Object> additionalData = new java.util.HashMap<>();
            additionalData.put("configurationVersion", saved.getVersion());
            webSocketNotificationService.broadcastGeneralAvailabilityUpdate(
                    String.format(
                            "Los horarios diarios han cambiado (Versión %d). Por favor, actualiza la vista de disponibilidad.",
                            saved.getVersion()),
                    additionalData);
        } catch (Exception e) {
            logger.error("Error al enviar actualización general de disponibilidad WebSocket: {}", e.getMessage(), e);
        }

        // 14. Convertir a response
        return mapper.toResponse(saved);
    }

    /**
     * Valida que el request tenga la estructura correcta.
     */
    private void validateDailyHoursConfigRequest(DailyHoursConfigRequest request) {
        if (request == null) {
            throw new ApiException("La configuración de horarios no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        if (request.getDailyHours() == null || request.getDailyHours().isEmpty()) {
            throw new ApiException(
                    "Debe proporcionar al menos un día con horarios",
                    HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida que solo días abiertos tengan horarios configurados.
     * 
     * @param dailyHoursList Lista de horarios diarios
     * @param weeklyConfig   Configuración semanal que define qué días están
     *                       abiertos
     */
    private void validateOnlyOpenDaysHaveHours(List<DailyHours> dailyHoursList, WeeklyConfig weeklyConfig) {
        if (weeklyConfig == null) {
            throw new ApiException(
                    "La configuración semanal no está definida",
                    HttpStatus.BAD_REQUEST);
        }

        for (DailyHours dailyHours : dailyHoursList) {
            Integer dayOfWeek = dailyHours.getDayOfWeek();
            Boolean isDayOpen = weeklyConfig.isDayOpen(dayOfWeek);

            if (isDayOpen == null || !isDayOpen) {
                String dayName = getDayName(dayOfWeek);
                throw new ApiException(
                        String.format(
                                "No se pueden definir horarios para días cerrados. El día '%s' está cerrado según la configuración semanal.",
                                dayName),
                        HttpStatus.BAD_REQUEST);
            }
        }
    }

    /**
     * Valida que no haya superposiciones de rangos horarios en el mismo día.
     * 
     * @param dailyHoursList Lista de horarios diarios
     */
    private void validateNoOverlaps(List<DailyHours> dailyHoursList) {
        for (DailyHours dailyHours : dailyHoursList) {
            if (!dailyHours.hasNoOverlaps()) {
                String dayName = getDayName(dailyHours.getDayOfWeek());
                throw new ApiException(
                        String.format("Los rangos horarios no pueden superponerse. Hay superposiciones en el día '%s'.",
                                dayName),
                        HttpStatus.BAD_REQUEST);
            }
        }
    }

    /**
     * Valida que todos los rangos horarios sean válidos (start < end y formato
     * correcto).
     * 
     * @param dailyHoursList Lista de horarios diarios
     */
    private void validateTimeRanges(List<DailyHours> dailyHoursList) {
        for (DailyHours dailyHours : dailyHoursList) {
            if (!dailyHours.allRangesValid()) {
                String dayName = getDayName(dailyHours.getDayOfWeek());
                throw new ApiException(
                        String.format(
                                "Los rangos horarios del día '%s' no son válidos. El horario de inicio debe ser anterior al horario de fin.",
                                dayName),
                        HttpStatus.BAD_REQUEST);
            }
        }
    }

    @Override
    @Transactional
    public CalendarConfigurationResponse configureAppointmentDuration(AppointmentDurationRequest request, Long userId) {
        logger.info("Configurando duración de turnos - Usuario: {}, Duración: {} minutos", userId,
                request.getDurationMinutes());

        // 1. Validar request
        validateAppointmentDurationRequest(request);

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. Debe crear primero una configuración semanal base.",
                        HttpStatus.BAD_REQUEST));

        // 3. Validar que existan horarios configurados
        if (activeConfig.getDailyHours() == null || activeConfig.getDailyHours().isEmpty()) {
            throw new ApiException(
                    "No hay horarios configurados. Debe configurar los horarios diarios antes de definir la duración de los turnos.",
                    HttpStatus.BAD_REQUEST);
        }

        // 4. Validar que la duración sea divisible por 15
        if (request.getDurationMinutes() % 15 != 0) {
            throw new ApiException(
                    "La duración debe ser divisible por 15 minutos. Ejemplos válidos: 15, 30, 45, 60, 90, 120, etc.",
                    HttpStatus.BAD_REQUEST);
        }

        // 5. Validar compatibilidad con rangos horarios existentes
        validateDurationCompatibility(request.getDurationMinutes(), activeConfig.getDailyHours());

        // 6. Calcular nueva versión
        Integer newVersion = calculateNextVersion();

        // 7. Desactivar configuración anterior
        deactivatePreviousConfiguration();

        // 8. Crear nueva configuración (copiando todo de la anterior)
        CalendarConfiguration newConfiguration = new CalendarConfiguration(
                activeConfig.getWeeklyConfig(),
                userId,
                request.getNotes() != null ? request.getNotes()
                        : "Configuración de duración de turnos: " + request.getDurationMinutes() + " minutos");
        newConfiguration.setVersion(newVersion);
        newConfiguration.setActive(true);
        newConfiguration.setAppointmentDurationMinutes(request.getDurationMinutes());

        // 9. Copiar horarios diarios de la configuración anterior
        List<DailyHours> copiedDailyHours = new ArrayList<>();
        for (DailyHours originalDailyHours : activeConfig.getDailyHours()) {
            DailyHours copiedDailyHoursEntity = new DailyHours(originalDailyHours.getDayOfWeek());
            copiedDailyHoursEntity.setTimeRanges(new ArrayList<>(originalDailyHours.getTimeRanges()));
            copiedDailyHoursEntity.setCalendarConfiguration(newConfiguration);
            copiedDailyHours.add(copiedDailyHoursEntity);
        }
        newConfiguration.setDailyHours(copiedDailyHours);

        // 10. Guardar
        CalendarConfiguration saved = repository.save(newConfiguration);
        logger.info("Duración de turnos configurada exitosamente - ID: {}, Versión: {}, Duración: {} minutos",
                saved.getId(), saved.getVersion(), saved.getAppointmentDurationMinutes());

        // 11. Notificar a admins sobre el cambio de configuración
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                    NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                    "Duración de Turnos Actualizada",
                    String.format(
                            "Se ha actualizado la duración de turnos a %d minutos (Versión %d). Revisa el impacto en turnos existentes.",
                            saved.getAppointmentDurationMinutes(), saved.getVersion()),
                    RelatedEntityType.CALENDAR_CONFIGURATION,
                    saved.getId());
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de cambio de configuración: {}", e.getMessage(), e);
        }

        // 12. Enviar actualización general de disponibilidad en tiempo real - Tiempo
        // Real
        try {
            java.util.Map<String, Object> additionalData = new java.util.HashMap<>();
            additionalData.put("configurationVersion", saved.getVersion());
            additionalData.put("durationMinutes", saved.getAppointmentDurationMinutes());
            webSocketNotificationService.broadcastGeneralAvailabilityUpdate(
                    String.format(
                            "La duración de turnos ha cambiado a %d minutos (Versión %d). Por favor, actualiza la vista de disponibilidad.",
                            saved.getAppointmentDurationMinutes(), saved.getVersion()),
                    additionalData);
        } catch (Exception e) {
            logger.error("Error al enviar actualización general de disponibilidad WebSocket: {}", e.getMessage(), e);
        }

        // 13. Convertir a response
        return mapper.toResponse(saved);
    }

    /**
     * Valida que el request tenga la estructura correcta.
     */
    private void validateAppointmentDurationRequest(AppointmentDurationRequest request) {
        if (request == null) {
            throw new ApiException("La configuración de duración no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        if (request.getDurationMinutes() == null) {
            throw new ApiException("La duración del turno es obligatoria", HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida que la duración sea compatible con todos los rangos horarios
     * existentes.
     * La duración debe dividir cada rango horario (no debe haber residuo).
     * 
     * @param durationMinutes Duración propuesta en minutos
     * @param dailyHoursList  Lista de horarios diarios configurados
     * @throws ApiException si la duración no es compatible con algún rango
     */
    private void validateDurationCompatibility(Integer durationMinutes, List<DailyHours> dailyHoursList) {
        if (dailyHoursList == null || dailyHoursList.isEmpty()) {
            return; // No hay horarios para validar
        }

        for (DailyHours dailyHours : dailyHoursList) {
            if (dailyHours.getTimeRanges() == null || dailyHours.getTimeRanges().isEmpty()) {
                continue;
            }

            for (TimeRange timeRange : dailyHours.getTimeRanges()) {
                Integer rangeDurationMinutes = timeRange.getDurationMinutes();

                if (rangeDurationMinutes == null) {
                    // Rango inválido, debería haberse validado antes, pero lo ignoramos aquí
                    continue;
                }

                // Validar que la duración divida el rango horario
                if (rangeDurationMinutes % durationMinutes != 0) {
                    String dayName = getDayName(dailyHours.getDayOfWeek());
                    throw new ApiException(
                            String.format(
                                    "La duración de %d minutos no es compatible con los horarios configurados. " +
                                            "El rango del %s (%s - %s) tiene una duración de %d minutos, " +
                                            "que no es divisible por %d minutos. " +
                                            "Ejemplo: para un rango de %d minutos, las duraciones válidas son divisores de %d (ej: %d, %d, %d minutos).",
                                    durationMinutes,
                                    dayName,
                                    timeRange.getStart(),
                                    timeRange.getEnd(),
                                    rangeDurationMinutes,
                                    durationMinutes,
                                    rangeDurationMinutes,
                                    rangeDurationMinutes,
                                    getDivisorsExample(rangeDurationMinutes)),
                            HttpStatus.BAD_REQUEST);
                }
            }
        }

        logger.debug("Duración {} minutos validada correctamente contra todos los rangos horarios", durationMinutes);
    }

    /**
     * Obtiene ejemplos de divisores para mensajes de error más útiles.
     * 
     * @param number Número del cual obtener divisores comunes
     * @return String con ejemplos de divisores válidos
     */
    private String getDivisorsExample(Integer number) {
        // Buscar divisores comunes en el rango 15-240
        java.util.List<Integer> commonDivisors = new java.util.ArrayList<>();
        for (int i = 15; i <= Math.min(number, 240); i += 15) {
            if (number % i == 0) {
                commonDivisors.add(i);
            }
        }

        if (commonDivisors.isEmpty()) {
            return "ninguno (ajuste los horarios o la duración)";
        }

        // Mostrar máximo 3 ejemplos
        int maxExamples = Math.min(3, commonDivisors.size());
        return commonDivisors.subList(0, maxExamples).stream()
                .map(String::valueOf)
                .collect(java.util.stream.Collectors.joining(", "));
    }

    @Override
    @Transactional(readOnly = true)
    public ConsolidatedCalendarResponse getConsolidatedCalendar(LocalDate startDate, LocalDate endDate) {
        logger.info("Solicitud de calendario consolidado - Rango: {} a {}", startDate, endDate);

        // 1. Validar rango de fechas
        validateDateRange(startDate, endDate);

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. Debe crear primero una configuración semanal base.",
                        HttpStatus.BAD_REQUEST));

        // 3. Obtener excepciones y bloqueos del rango (para optimizar consultas)
        List<CalendarException> exceptionsInRange = exceptionRepository
                .findByActiveTrueAndExceptionDateBetween(startDate, endDate);
        List<ManualBlock> blocksInRange = manualBlockRepository
                .findByActiveTrueAndBlockDateBetween(startDate, endDate);

        // 4. Generar días del calendario
        List<ConsolidatedDayResponse> days = new ArrayList<>();
        LocalDate currentDate = startDate;
        while (!currentDate.isAfter(endDate)) {
            ConsolidatedDayResponse day = evaluateDay(currentDate, activeConfig, exceptionsInRange, blocksInRange);
            days.add(day);
            currentDate = currentDate.plusDays(1);
        }

        logger.info("Calendario consolidado generado - {} días evaluados", days.size());

        return new ConsolidatedCalendarResponse(days);
    }

    /**
     * Valida el rango de fechas.
     */
    private void validateDateRange(LocalDate startDate, LocalDate endDate) {
        if (startDate == null || endDate == null) {
            throw new ApiException("Las fechas de inicio y fin son obligatorias", HttpStatus.BAD_REQUEST);
        }

        if (startDate.isAfter(endDate)) {
            throw new ApiException(
                    "La fecha de inicio no puede ser posterior a la fecha de fin",
                    HttpStatus.BAD_REQUEST);
        }

        long daysBetween = java.time.temporal.ChronoUnit.DAYS.between(startDate, endDate) + 1;
        if (daysBetween > 90) {
            throw new ApiException(
                    "El rango de fechas no puede exceder 90 días. Rango solicitado: " + daysBetween + " días",
                    HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Evalúa un día específico aplicando el orden de precedencia:
     * 1. Bloqueos operativos (prioridad máxima)
     * 2. Excepciones por fecha
     * 3. Configuración base del calendario semanal
     * 
     * @param date              Fecha a evaluar
     * @param activeConfig      Configuración activa
     * @param exceptionsInRange Excepciones en el rango consultado (para optimizar)
     * @param blocksInRange     Bloqueos en el rango consultado (para optimizar)
     * @return Información consolidada del día
     */
    private ConsolidatedDayResponse evaluateDay(LocalDate date, CalendarConfiguration activeConfig,
            List<CalendarException> exceptionsInRange,
            List<ManualBlock> blocksInRange) {

        // 0. Si la fecha es pasada, siempre retornar CLOSED (días pasados no son relevantes para gestión futura)
        LocalDate today = getTodayGMT3();
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
        Optional<ManualBlock> blockForDate = blocksInRange.stream()
                .filter(b -> b.getBlockDate().equals(date))
                .findFirst();

        if (blockForDate.isPresent()) {
            return evaluateDayWithBlock(date, blockForDate.get());
        }

        // 2. Verificar excepciones
        Optional<CalendarException> exceptionForDate = exceptionsInRange.stream()
                .filter(e -> e.getExceptionDate().equals(date))
                .findFirst();

        if (exceptionForDate.isPresent()) {
            return evaluateDayWithException(date, exceptionForDate.get());
        }

        // 3. Usar configuración base
        return evaluateDayWithBase(date, activeConfig);
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

        Boolean hasAppointments = calculateHasExistingAppointments(date);
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

        Boolean hasAppointments = calculateHasExistingAppointments(date);
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
            Boolean hasAppointments = calculateHasExistingAppointments(date);
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
            String dayName = getDayName(dayOfWeekNumber);
            day.setRuleDescription(String.format("%s - Cerrado",
                    capitalizeFirst(dayName)));
            day.setTimeRanges(new ArrayList<>());
        } else {
            // Día abierto en configuración base - buscar horarios
            List<DailyHours> dailyHoursList = activeConfig.getDailyHours();
            Optional<DailyHours> dailyHoursForDay = dailyHoursList.stream()
                    .filter(dh -> dh.getDayOfWeek().equals(dayOfWeekNumber))
                    .findFirst();

            // Verificar si hay turnos existentes ANTES de determinar el estado
            // Esto es importante para días que están abiertos pero no tienen horarios
            // configurados
            Boolean hasAppointments = calculateHasExistingAppointments(date);
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
                String dayName = getDayName(dayOfWeekNumber);
                day.setRuleDescription(String.format("%s - Abierto",
                        capitalizeFirst(dayName)));
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
                    String dayName = getDayName(dayOfWeekNumber);
                    day.setRuleDescription(
                            String.format("%s - Abierto (sin horarios configurados, pero con turnos existentes)",
                                    capitalizeFirst(dayName)));
                    day.setTimeRanges(new ArrayList<>());
                } else {
                    // Sin turnos y sin horarios - cerrado
                    day.setState("CLOSED");
                    String dayName = getDayName(dayOfWeekNumber);
                    day.setRuleDescription(String.format("%s - Cerrado",
                            capitalizeFirst(dayName)));
                    day.setTimeRanges(new ArrayList<>());
                }
            }

            day.setHasExistingAppointments(hasAppointments);
            day.setAppointmentsCount(appointmentsCount);
        }

        // Si el día está cerrado, también calcular turnos existentes
        if ("CLOSED".equals(day.getState())) {
            Boolean hasAppointments = calculateHasExistingAppointments(date);
            Integer appointmentsCount = countExistingAppointments(date);
            day.setHasExistingAppointments(hasAppointments);
            day.setAppointmentsCount(appointmentsCount);
        }

        return day;
    }

    /**
     * Calcula si hay turnos confirmados para una fecha.
     * 
     * Solo verifica turnos en estado CONFIRMED para el calendario general.
     * El admin necesita ver solo turnos confirmados para gestionar el calendario.
     * 
     * IMPORTANTE: Solo considera turnos futuros (fecha >= hoy) para el calendario general.
     * Los turnos pasados no afectan el estado visual del día.
     * 
     * @param date Fecha a verificar
     * @return true si hay turnos confirmados futuros, false en caso contrario
     */
    private Boolean calculateHasExistingAppointments(LocalDate date) {
        // Si la fecha es pasada, no hay turnos "existentes" relevantes para el calendario
        LocalDate today = getTodayGMT3();
        if (date.isBefore(today)) {
            return false;
        }
        
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> confirmedStates = List
                .of(com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED);

        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findByDateAndStateIn(date, confirmedStates);

        return !appointments.isEmpty();
    }

    /**
     * Cuenta los turnos confirmados para una fecha.
     * 
     * Solo cuenta turnos en estado CONFIRMED para el calendario general.
     * El admin necesita ver solo turnos confirmados para gestionar el calendario.
     * 
     * IMPORTANTE: Solo considera turnos futuros (fecha >= hoy) para el calendario general.
     * Los turnos pasados no afectan el estado visual del día.
     * 
     * @param date Fecha a verificar
     * @return Cantidad de turnos confirmados futuros para esa fecha
     */
    private Integer countExistingAppointments(LocalDate date) {
        // Si la fecha es pasada, no hay turnos "existentes" relevantes para el calendario
        LocalDate today = getTodayGMT3();
        if (date.isBefore(today)) {
            return 0;
        }
        
        // Contar solo turnos CONFIRMED para el calendario general
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> confirmedStates = List
                .of(com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED);

        List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments = appointmentRepository
                .findByDateAndStateIn(date, confirmedStates);

        return appointments.size();
    }

    /**
     * Cancela masivamente los turnos afectados por el cierre de días.
     * Cambia el estado a CANCELLED_BY_ADMIN y registra en historial.
     * 
     * @param appointments Lista de turnos a cancelar
     * @param adminUserId  ID del administrador que realiza la cancelación
     * @param reason       Razón de la cancelación
     */
    private void cancelAffectedAppointmentsByDayClosure(
            java.util.List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments,
            Long adminUserId,
            String reason) {
        logger.info("Iniciando cancelación masiva de {} turno(s) afectados por cierre de días - Admin: {}, Razón: {}",
                appointments.size(), adminUserId, reason);

        // Filtrar turnos válidos para cancelar
        java.util.List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointmentsToCancel = new java.util.ArrayList<>();
        java.util.List<AppointmentHistory> historiesToSave = new java.util.ArrayList<>();

        for (com.ak4n1.turn_management.feature.appointment.domain.Appointment appointment : appointments) {
            try {
                // Validar que el turno no esté ya cancelado o completado
                if (appointment.getState() == AppointmentState.CANCELLED ||
                        appointment.getState() == AppointmentState.CANCELLED_BY_ADMIN ||
                        appointment.getState() == AppointmentState.COMPLETED ||
                        appointment.getState() == AppointmentState.EXPIRED) {
                    logger.debug("Turno ID {} omitido - Estado actual: {}", appointment.getId(),
                            appointment.getState());
                    continue;
                }

                // Guardar estado anterior
                AppointmentState previousState = appointment.getState();

                // Cancelar el turno por admin
                appointment.cancelByAdmin();
                appointmentsToCancel.add(appointment);

                // Preparar historial para batch save
                AppointmentHistory history = new AppointmentHistory(
                        appointment.getId(),
                        adminUserId,
                        previousState,
                        AppointmentState.CANCELLED_BY_ADMIN,
                        "CANCELLED_BY_ADMIN",
                        reason,
                        null // clientIp no disponible en este contexto
                );
                historiesToSave.add(history);

                logger.debug(
                        "Turno preparado para cancelación masiva - ID: {}, Usuario: {}, Estado anterior: {}, Razón: {}",
                        appointment.getId(), appointment.getUserId(), previousState, reason);

            } catch (Exception e) {
                // NO lanzar excepción - un error en un turno no debe bloquear la cancelación de
                // los demás
                logger.error("Error al preparar turno para cancelación masiva - Turno ID: {}, Error: {}",
                        appointment.getId(), e.getMessage(), e);
            }
        }

        // Batch save de todos los turnos cancelados (MUCHO MÁS RÁPIDO)
        if (!appointmentsToCancel.isEmpty()) {
            try {
                appointmentRepository.saveAll(appointmentsToCancel);
                logger.info("Batch save de {} turno(s) cancelado(s) completado", appointmentsToCancel.size());
            } catch (Exception e) {
                logger.error("Error en batch save de turnos cancelados: {}", e.getMessage(), e);
                throw e; // Re-lanzar porque esto es crítico
            }
        }

        // Batch save de todos los historiales (MUCHO MÁS RÁPIDO)
        if (!historiesToSave.isEmpty()) {
            try {
                appointmentHistoryRepository.saveAll(historiesToSave);
                logger.info("Batch save de {} historial(es) de cancelación completado", historiesToSave.size());
            } catch (Exception e) {
                logger.error("Error en batch save de historiales: {}", e.getMessage(), e);
                // No re-lanzar porque los turnos ya están cancelados, solo falta el historial
            }
        }

        logger.info("Cancelación masiva completada - {} turno(s) cancelado(s) en batch", appointmentsToCancel.size());
    }

    /**
     * Capitaliza la primera letra de un string.
     */
    private String capitalizeFirst(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
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
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
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
     * Resultado del cálculo de impacto.
     */
    private static class ImpactCalculationResult {
        private Integer affectedDays;
        private Integer slotsLost;
        private Integer existingAppointmentsAffected;
        private List<AffectedAppointmentInfo> appointments;
        private String changeDescription;

        public ImpactCalculationResult(Integer affectedDays, Integer slotsLost,
                Integer existingAppointmentsAffected,
                List<AffectedAppointmentInfo> appointments,
                String changeDescription) {
            this.affectedDays = affectedDays;
            this.slotsLost = slotsLost;
            this.existingAppointmentsAffected = existingAppointmentsAffected;
            this.appointments = appointments;
            this.changeDescription = changeDescription;
        }

        // Getters
        public Integer getAffectedDays() {
            return affectedDays;
        }

        public Integer getSlotsLost() {
            return slotsLost;
        }

        public Integer getExistingAppointmentsAffected() {
            return existingAppointmentsAffected;
        }

        public List<AffectedAppointmentInfo> getAppointments() {
            return appointments;
        }

        public String getChangeDescription() {
            return changeDescription;
        }
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
                String dayName = getDayName(dayOfWeekNumber);
                affectedDayNames.add(capitalizeFirst(dayName));
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
            java.util.Set<LocalDate> datesWithAppointments = appointmentsInClosedDays.stream()
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
        ConsolidatedDayResponse currentDay = evaluateDay(
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
        ConsolidatedDayResponse currentDay = evaluateDay(
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

    /**
     * Calcula los turnos afectados por un cambio en la configuración del
     * calendario.
     * 
     * Busca turnos en estados activos (CREATED, CONFIRMED) en el rango de fechas
     * especificado.
     */
    private List<AffectedAppointmentInfo> calculateAffectedAppointments(LocalDate startDate, LocalDate endDate) {
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = List.of(
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED);

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

    /**
     * NUEVO: Calcula turnos afectados para días específicos (útil cuando se cierran
     * días específicos).
     * 
     * @param dates Lista de fechas específicas donde buscar turnos afectados
     * @return Lista de turnos afectados solo en esas fechas
     */
    private List<AffectedAppointmentInfo> calculateAffectedAppointmentsForDates(List<LocalDate> dates) {
        if (dates == null || dates.isEmpty()) {
            return new ArrayList<>();
        }

        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = List.of(
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED);

        // Convertir lista de fechas a un Set para búsqueda eficiente
        java.util.Set<LocalDate> datesSet = new java.util.HashSet<>(dates);

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

    @Override
    @Transactional(readOnly = true)
    public AvailabilityResponse checkAvailability(LocalDate date) {
        logger.info("Evaluación de disponibilidad solicitada para fecha: {}", date);

        // 1. Validar que la fecha no sea pasada (usando GMT-3)
        LocalDate today = getTodayGMT3();
        if (date.isBefore(today)) {
            throw new ApiException(
                    "No se puede evaluar disponibilidad para fechas pasadas. Fecha solicitada: " + date,
                    HttpStatus.BAD_REQUEST);
        }

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. El sistema no está configurado.",
                        HttpStatus.SERVICE_UNAVAILABLE));

        // 3. Obtener excepciones y bloqueos para esta fecha
        List<CalendarException> exceptions = exceptionRepository
                .findByActiveTrueAndExceptionDateBetween(date, date);
        List<ManualBlock> blocks = manualBlockRepository
                .findByActiveTrueAndBlockDateBetween(date, date);

        // 4. Evaluar el día aplicando precedencia
        ConsolidatedDayResponse dayInfo = evaluateDay(date, activeConfig, exceptions, blocks);

        // 5. Construir respuesta base
        AvailabilityResponse response = new AvailabilityResponse();
        response.setDate(date);
        response.setRuleApplied(dayInfo.getRuleType());
        response.setIsAvailable("OPEN".equals(dayInfo.getState()));
        response.setTimeRanges(dayInfo.getTimeRanges());
        response.setDescription(dayInfo.getRuleDescription());

        // 6. NUEVO: Si el día está cerrado, verificar si tiene turnos existentes
        if ("CLOSED".equals(dayInfo.getState()) || "PARTIAL".equals(dayInfo.getState())) {
            Integer existingAppointmentsCount = countExistingAppointments(date);

            if (existingAppointmentsCount > 0) {
                response.setHasExistingAppointments(true);
                response.setExistingAppointmentsCount(existingAppointmentsCount);

                // Mensaje específico según el tipo de regla
                String ruleTypeDescription = getRuleTypeDescription(dayInfo.getRuleType(), dayInfo.getState());
                response.setMessage(
                        String.format(
                                "Este día está %s según la configuración actual, pero tiene %d turno(s) existente(s) " +
                                        "creado(s) con una configuración anterior. No se pueden crear nuevos turnos para este día. "
                                        +
                                        "Los turnos existentes seguirán siendo válidos y pueden ser atendidos.",
                                ruleTypeDescription,
                                existingAppointmentsCount));
            } else {
                // Día cerrado sin turnos
                response.setHasExistingAppointments(false);
                response.setExistingAppointmentsCount(0);
                response.setMessage(
                        String.format(
                                "Este día está %s según la configuración actual. No se pueden crear nuevos turnos.",
                                getRuleTypeDescription(dayInfo.getRuleType(), dayInfo.getState())));
            }
        } else {
            // Día abierto
            response.setHasExistingAppointments(false);
            response.setExistingAppointmentsCount(0);
            response.setMessage(null); // No es necesario mensaje para días abiertos
        }

        logger.info("Disponibilidad evaluada - Fecha: {}, Disponible: {}, Regla: {}, Turnos existentes: {}",
                date, response.getIsAvailable(), response.getRuleApplied(),
                response.getExistingAppointmentsCount() != null ? response.getExistingAppointmentsCount() : 0);

        return response;
    }

    /**
     * NUEVO: Helper method para obtener descripción del tipo de regla y estado.
     */
    private String getRuleTypeDescription(String ruleType, String state) {
        if ("BLOCK".equals(ruleType)) {
            return "PARTIAL".equals(state) ? "parcialmente bloqueado" : "bloqueado";
        } else if ("EXCEPTION".equals(ruleType)) {
            return "cerrado por excepción";
        } else {
            return "cerrado";
        }
    }

    @Override
    @Transactional(readOnly = true)
    public SlotsResponse getAvailableSlots(LocalDate date) {
        logger.info("Solicitud de slots disponibles para fecha: {}", date);

        // 1. Validar que la fecha no sea pasada
        LocalDate today = getTodayGMT3();
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
        ConsolidatedDayResponse dayInfo = evaluateDay(date, activeConfig, exceptions, blocks);

        // 5. NUEVO: Si el día está cerrado, retornar respuesta con información de
        // turnos existentes
        if ("CLOSED".equals(dayInfo.getState())) {
            Integer existingAppointmentsCount = countExistingAppointments(date);

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
                                "Este día está cerrado según la configuración actual, pero tiene %d turno(s) existente(s) "
                                        +
                                        "creado(s) con una configuración anterior. No se pueden crear nuevos turnos para este día. "
                                        +
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

        // 6. Si hay bloqueo parcial, necesitamos obtener los rangos horarios base antes
        // del bloqueo
        List<TimeRangeResponse> timeRangesToUse = dayInfo.getTimeRanges();

        // Si hay bloqueo parcial y no hay rangos en dayInfo, obtener rangos base
        // (porque evaluateDayWithBlock no calcula rangos disponibles para bloqueos
        // parciales)
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
                // Usar rangos base
                ConsolidatedDayResponse baseDayInfo = evaluateDayWithBase(date, activeConfig);
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

        // 12. NUEVO: Si el día es PARTIAL, verificar si hay turnos existentes afectados
        // por el bloqueo
        if ("PARTIAL".equals(dayInfo.getState()) && !blocks.isEmpty()) {
            Integer existingAppointmentsCount = countExistingAppointments(date);

            if (existingAppointmentsCount > 0) {
                // Obtener rango horario del bloqueo para verificar si afecta turnos
                ManualBlock block = blocks.get(0);

                if (block.getTimeRange() != null) {
                    LocalTime blockStart = LocalTime.parse(block.getTimeRange().getStart(),
                            DateTimeFormatter.ofPattern("HH:mm"));
                    LocalTime blockEnd = LocalTime.parse(block.getTimeRange().getEnd(),
                            DateTimeFormatter.ofPattern("HH:mm"));

                    // Contar turnos que intersectan con el bloqueo
                    List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = Arrays
                            .asList(
                                    com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                                    com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED);

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

    /**
     * Genera slots desde un rango horario según la duración configurada.
     * Excluye slots que están bloqueados.
     */
    private List<SlotResponse> generateSlotsFromRange(
            TimeRangeResponse timeRange,
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

    /**
     * Verifica si un slot está bloqueado por algún bloqueo operativo.
     */
    private boolean isSlotBlocked(LocalTime slotStart, LocalTime slotEnd, List<ManualBlock> blocks) {
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

                    // Verificar superposición: slot se superpone si slotStart < blockEnd && slotEnd
                    // > blockStart
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

    /**
     * Excluye slots ocupados por turnos existentes.
     * 
     * Marca como no disponibles los slots que tienen turnos en estados activos
     * (CREATED, CONFIRMED).
     */
    private List<SlotResponse> excludeOccupiedSlots(List<SlotResponse> slots, LocalDate date) {
        List<com.ak4n1.turn_management.feature.appointment.domain.AppointmentState> activeStates = List.of(
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CREATED,
                com.ak4n1.turn_management.feature.appointment.domain.AppointmentState.CONFIRMED);

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
    @Transactional(readOnly = true)
    public AvailabilityRangeResponse getAvailabilityRange(LocalDate startDate, LocalDate endDate) {
        logger.info("Solicitud de disponibilidad por rango recibida - Inicio: {}, Fin: {}", startDate, endDate);

        LocalDate today = getTodayGMT3();

        // 1. Validar fechas
        if (startDate == null) {
            startDate = today;
        }
        if (endDate == null) {
            endDate = today.plusDays(89); // 90 días incluyendo hoy
        }

        // Ajustar fechas pasadas
        if (startDate.isBefore(today)) {
            startDate = today;
        }

        // Validar que startDate <= endDate
        if (startDate.isAfter(endDate)) {
            throw new ApiException(
                    "La fecha de inicio no puede ser posterior a la fecha de fin",
                    HttpStatus.BAD_REQUEST);
        }

        // Validar rango máximo: 90 días (antes de procesar)
        long daysBetween = java.time.temporal.ChronoUnit.DAYS.between(startDate, endDate) + 1;
        if (daysBetween > 90) {
            throw new ApiException(
                    String.format("El rango de fechas no puede exceder 90 días. Rango solicitado: %d días",
                            daysBetween),
                    HttpStatus.BAD_REQUEST);
        }

        // 2. Verificar que exista configuración activa con duración
        CalendarConfiguration activeConfig = repository.findByActiveTrue()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. El sistema no está configurado.",
                        HttpStatus.SERVICE_UNAVAILABLE));

        Integer appointmentDurationMinutes = activeConfig.getAppointmentDurationMinutes();
        if (appointmentDurationMinutes == null) {
            throw new ApiException(
                    "No hay duración de turnos configurada. Debe configurar la duración antes de consultar disponibilidad.",
                    HttpStatus.BAD_REQUEST);
        }

        // 3. Obtener excepciones y bloqueos para el rango
        List<CalendarException> exceptions = exceptionRepository
                .findByActiveTrueAndExceptionDateBetween(startDate, endDate);
        List<ManualBlock> blocks = manualBlockRepository
                .findByActiveTrueAndBlockDateBetween(startDate, endDate);

        // 4. Evaluar cada día en el rango
        List<DayAvailabilityResponse> days = new ArrayList<>();
        LocalDate currentDateLoop = startDate;

        while (!currentDateLoop.isAfter(endDate)) {
            final LocalDate currentDate = currentDateLoop; // Variable final para uso en lambdas
            try {
                // Evaluar disponibilidad del día
                ConsolidatedDayResponse dayInfo = evaluateDay(currentDate, activeConfig, exceptions, blocks);

                // Si el día tiene horarios, generar slots y calcular estadísticas
                Integer totalSlots = 0;
                Integer availableSlots = 0;
                String status = "CLOSED";

                // Obtener bloqueos para este día
                List<ManualBlock> blocksForDate = blocks.stream()
                        .filter(b -> b.getBlockDate().equals(currentDate))
                        .collect(Collectors.toList());

                // Obtener rangos horarios disponibles
                List<TimeRangeResponse> timeRangesToUse = dayInfo.getTimeRanges();

                // Si hay bloqueo parcial o si timeRanges está vacío pero el estado no es
                // CLOSED,
                // obtener rangos base antes del bloqueo
                boolean hasPartialBlock = !blocksForDate.isEmpty() &&
                        blocksForDate.stream().anyMatch(b -> !Boolean.TRUE.equals(b.getIsFullDay()));

                if ((timeRangesToUse == null || timeRangesToUse.isEmpty()) &&
                        ("PARTIAL".equals(dayInfo.getState()) || hasPartialBlock)) {
                    // Obtener rangos horarios base antes de aplicar bloqueo
                    List<CalendarException> exceptionsForDate = exceptions.stream()
                            .filter(e -> e.getExceptionDate().equals(currentDate))
                            .collect(Collectors.toList());

                    if (!exceptionsForDate.isEmpty() && Boolean.TRUE.equals(exceptionsForDate.get(0).getIsOpen())) {
                        // Si hay excepción abierta, usar sus rangos
                        timeRangesToUse = exceptionsForDate.get(0).getTimeRanges().stream()
                                .map(tr -> new TimeRangeResponse(tr.getStart(), tr.getEnd()))
                                .collect(Collectors.toList());
                    } else {
                        // Usar rangos base
                        ConsolidatedDayResponse baseDayInfo = evaluateDayWithBase(currentDate, activeConfig);
                        if (baseDayInfo.getTimeRanges() != null && !baseDayInfo.getTimeRanges().isEmpty()) {
                            timeRangesToUse = baseDayInfo.getTimeRanges();
                        }
                    }
                }

                if (timeRangesToUse != null && !timeRangesToUse.isEmpty()) {
                    List<SlotResponse> allSlots = new ArrayList<>();

                    for (TimeRangeResponse timeRange : timeRangesToUse) {
                        List<SlotResponse> slotsFromRange = generateSlotsFromRange(
                                timeRange, appointmentDurationMinutes, blocksForDate);
                        allSlots.addAll(slotsFromRange);
                    }

                    totalSlots = allSlots.size();
                    availableSlots = (int) allSlots.stream()
                            .filter(SlotResponse::getAvailable)
                            .count();

                    // Determinar estado
                    if (totalSlots == 0) {
                        status = "CLOSED";
                    } else if (availableSlots == 0) {
                        status = "CLOSED";
                    } else if (availableSlots.equals(totalSlots)) {
                        status = "FULL";
                    } else {
                        status = "PARTIAL";
                    }
                }

                DayAvailabilityResponse dayResponse = new DayAvailabilityResponse(
                        currentDate, status, availableSlots, totalSlots);
                days.add(dayResponse);

            } catch (Exception e) {
                // Si hay error al evaluar un día, lo marcamos como cerrado
                logger.warn("Error al evaluar disponibilidad para fecha {}: {}", currentDate, e.getMessage());
                DayAvailabilityResponse dayResponse = new DayAvailabilityResponse(
                        currentDate, "CLOSED", 0, 0);
                days.add(dayResponse);
            }

            currentDateLoop = currentDateLoop.plusDays(1);
        }

        logger.info("Disponibilidad por rango calculada - Rango: {} a {}, Días evaluados: {}",
                startDate, endDate, days.size());

        // 5. Construir respuesta
        AvailabilityRangeResponse response = new AvailabilityRangeResponse();
        response.setStartDate(startDate);
        response.setEndDate(endDate);
        response.setDays(days);

        return response;
    }

    /**
     * Obtiene la fecha actual en zona horaria GMT-3 (Argentina).
     * 
     * @return Fecha actual en GMT-3
     */
    private LocalDate getTodayGMT3() {
        ZoneId gmtMinus3 = ZoneId.of("America/Argentina/Buenos_Aires");
        return ZonedDateTime.now(gmtMinus3).toLocalDate();
    }

    /**
     * Obtiene el nombre del día en español para mensajes de error.
     * 
     * @param dayOfWeek 1=Lunes, 2=Martes, ..., 7=Domingo
     * @return Nombre del día
     */
    private String getDayName(Integer dayOfWeek) {
        return switch (dayOfWeek) {
            case 1 -> "lunes";
            case 2 -> "martes";
            case 3 -> "miércoles";
            case 4 -> "jueves";
            case 5 -> "viernes";
            case 6 -> "sábado";
            case 7 -> "domingo";
            default -> "día " + dayOfWeek;
        };
    }

    /**
     * Obtiene el historial completo de cambios en la configuración del calendario.
     * 
     * Implementa US-T018.1:
     * - Solo accesible por admin
     * - Muestra todas las versiones de configuración
     * - Incluye quién hizo cada cambio y cuándo
     * - Detecta qué cambió en cada versión
     * - Cuenta turnos asociados a cada versión
     * - Versiones ordenadas por fecha descendente
     */
    @Override
    @Transactional(readOnly = true)
    public ConfigurationHistoryResponse getConfigurationHistory() {
        logger.info("Consultando historial de configuraciones del calendario");

        // 1. Obtener todas las configuraciones ordenadas por versión descendente
        List<CalendarConfiguration> configurations = repository.findAllByOrderByVersionDesc();

        logger.info("Configuraciones encontradas: {}", configurations.size());

        // 2. Mapear a DTOs con información adicional
        List<ConfigurationVersionResponse> versions = new ArrayList<>();
        CalendarConfiguration previousConfig = null;

        for (CalendarConfiguration config : configurations) {
            ConfigurationVersionResponse versionResponse = new ConfigurationVersionResponse();
            versionResponse.setVersionId(config.getId());
            versionResponse.setVersion(config.getVersion());
            versionResponse.setCreatedAt(config.getCreatedAt().toString());
            versionResponse.setCreatedByUserId(config.getCreatedByUserId());
            versionResponse.setNotes(config.getNotes());
            versionResponse.setActive(Boolean.TRUE.equals(config.getActive()));

            // Obtener email del usuario que creó la configuración
            Optional<User> user = userService.findById(config.getCreatedByUserId());
            if (user.isPresent()) {
                versionResponse.setCreatedByEmail(user.get().getEmail());
            }

            // Contar turnos asociados a esta versión
            long appointmentsCount = appointmentRepository.countByCalendarConfigVersion(config.getVersion());
            versionResponse.setAppointmentsCount((int) appointmentsCount);

            // Detectar cambios comparando con la versión anterior
            List<String> changes = detectChanges(config, previousConfig);
            versionResponse.setChanges(changes);

            versions.add(versionResponse);
            previousConfig = config;
        }

        logger.info("Historial generado - Total versiones: {}", versions.size());

        return new ConfigurationHistoryResponse(versions, versions.size());
    }

    /**
     * Detecta los cambios entre dos versiones de configuración.
     */
    private List<String> detectChanges(CalendarConfiguration current, CalendarConfiguration previous) {
        List<String> changes = new ArrayList<>();

        if (previous == null) {
            // Primera versión
            changes.add("Configuración inicial creada");
            if (current.getAppointmentDurationMinutes() != null) {
                changes.add(String.format("Duración de turno: %d minutos", current.getAppointmentDurationMinutes()));
            }
            if (current.getWeeklyConfig() != null) {
                WeeklyConfig weekly = current.getWeeklyConfig();
                List<String> daysOpen = new ArrayList<>();
                if (Boolean.TRUE.equals(weekly.getMonday()))
                    daysOpen.add("lunes");
                if (Boolean.TRUE.equals(weekly.getTuesday()))
                    daysOpen.add("martes");
                if (Boolean.TRUE.equals(weekly.getWednesday()))
                    daysOpen.add("miércoles");
                if (Boolean.TRUE.equals(weekly.getThursday()))
                    daysOpen.add("jueves");
                if (Boolean.TRUE.equals(weekly.getFriday()))
                    daysOpen.add("viernes");
                if (Boolean.TRUE.equals(weekly.getSaturday()))
                    daysOpen.add("sábado");
                if (Boolean.TRUE.equals(weekly.getSunday()))
                    daysOpen.add("domingo");
                if (!daysOpen.isEmpty()) {
                    changes.add(String.format("Días abiertos: %s", String.join(", ", daysOpen)));
                }
            }
            return changes;
        }

        // Comparar duración de turnos
        Integer currentDuration = current.getAppointmentDurationMinutes();
        Integer previousDuration = previous.getAppointmentDurationMinutes();
        if (currentDuration != null && previousDuration != null && !currentDuration.equals(previousDuration)) {
            changes.add(
                    String.format("Duración de turno cambiada de %d a %d minutos", previousDuration, currentDuration));
        } else if (currentDuration != null && previousDuration == null) {
            changes.add(String.format("Duración de turno configurada: %d minutos", currentDuration));
        } else if (currentDuration == null && previousDuration != null) {
            changes.add("Duración de turno eliminada");
        }

        // Comparar configuración semanal
        WeeklyConfig currentWeekly = current.getWeeklyConfig();
        WeeklyConfig previousWeekly = previous.getWeeklyConfig();
        if (currentWeekly != null && previousWeekly != null) {
            if (!java.util.Objects.equals(currentWeekly.getMonday(), previousWeekly.getMonday())) {
                changes.add(String.format("Lunes: %s",
                        Boolean.TRUE.equals(currentWeekly.getMonday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getTuesday(), previousWeekly.getTuesday())) {
                changes.add(String.format("Martes: %s",
                        Boolean.TRUE.equals(currentWeekly.getTuesday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getWednesday(), previousWeekly.getWednesday())) {
                changes.add(String.format("Miércoles: %s",
                        Boolean.TRUE.equals(currentWeekly.getWednesday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getThursday(), previousWeekly.getThursday())) {
                changes.add(String.format("Jueves: %s",
                        Boolean.TRUE.equals(currentWeekly.getThursday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getFriday(), previousWeekly.getFriday())) {
                changes.add(String.format("Viernes: %s",
                        Boolean.TRUE.equals(currentWeekly.getFriday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getSaturday(), previousWeekly.getSaturday())) {
                changes.add(String.format("Sábado: %s",
                        Boolean.TRUE.equals(currentWeekly.getSaturday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getSunday(), previousWeekly.getSunday())) {
                changes.add(String.format("Domingo: %s",
                        Boolean.TRUE.equals(currentWeekly.getSunday()) ? "abierto" : "cerrado"));
            }
        }

        // Comparar horarios diarios (simplificado - solo detecta si hay cambios en
        // cantidad)
        int currentDailyHoursCount = current.getDailyHours() != null ? current.getDailyHours().size() : 0;
        int previousDailyHoursCount = previous.getDailyHours() != null ? previous.getDailyHours().size() : 0;
        if (currentDailyHoursCount != previousDailyHoursCount) {
            changes.add(String.format("Horarios diarios: %d configurados (anterior: %d)", currentDailyHoursCount,
                    previousDailyHoursCount));
        }

        // Si no hay cambios detectados, agregar mensaje genérico
        if (changes.isEmpty()) {
            changes.add("Configuración actualizada (cambios menores)");
        }

        return changes;
    }
}
