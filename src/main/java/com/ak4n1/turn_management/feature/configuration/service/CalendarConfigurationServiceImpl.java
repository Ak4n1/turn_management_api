package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.request.AppointmentDurationRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.FullConfigRequest;
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
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
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
import com.ak4n1.turn_management.feature.configuration.util.DateUtils;
import com.ak4n1.turn_management.feature.configuration.util.DayNameUtils;
import com.ak4n1.turn_management.feature.configuration.service.validation.WeeklyConfigValidator;
import com.ak4n1.turn_management.feature.configuration.service.validation.DailyHoursValidator;
import com.ak4n1.turn_management.feature.configuration.service.validation.AppointmentDurationValidator;
import com.ak4n1.turn_management.feature.configuration.service.cancellation.AppointmentCancellationService;
import com.ak4n1.turn_management.feature.configuration.service.configuration.ConfigurationManagementService;
import com.ak4n1.turn_management.feature.configuration.service.configuration.ConfigurationVersionService;
import com.ak4n1.turn_management.feature.configuration.service.evaluation.DayEvaluationService;
import com.ak4n1.turn_management.feature.configuration.service.history.ConfigurationHistoryService;
import com.ak4n1.turn_management.feature.configuration.service.impact.ImpactCalculationService;
import com.ak4n1.turn_management.feature.configuration.service.slots.SlotGenerationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de configuración de calendario.
 *
 * Este servicio actúa como orquestador principal, delegando responsabilidades específicas
 * a servicios especializados siguiendo el principio de responsabilidad única (SRP).
 *
 * Responsabilidades principales:
 * - Orquestar validaciones y operaciones de configuración
 * - Gestionar notificaciones y comunicaciones
 * - Coordinar cancelaciones de turnos afectados
 * - Delegar lógica especializada a servicios específicos
 *
 * Servicios especializados utilizados:
 * - ConfigurationManagementService: CRUD y versionado de configuraciones
 * - ConfigurationVersionService: Gestión de versiones
 * - ConfigurationHistoryService: Historial de cambios
 * - ImpactCalculationService: Cálculo de impacto de cambios
 * - AppointmentCancellationService: Cancelación de turnos
 * - DayEvaluationService: Evaluación de días
 * - SlotGenerationService: Generación de slots disponibles
 * - Validadores: Validación específica por tipo de configuración
 */
@Service
public class CalendarConfigurationServiceImpl implements CalendarConfigurationService {

    private static final Logger logger = LoggerFactory.getLogger(CalendarConfigurationServiceImpl.class);

    private final CalendarConfigurationRepository repository;
    private final CalendarExceptionRepository exceptionRepository;
    private final ManualBlockRepository manualBlockRepository;
    private final CalendarConfigurationMapper mapper;
    private final AppointmentRepository appointmentRepository;
    private final UserService userService;
    private final UserRepository userRepository;
    private final WebSocketNotificationService webSocketNotificationService;
    private final com.ak4n1.turn_management.feature.notification.service.EmailService emailService;
    private final WeeklyConfigValidator weeklyConfigValidator;
    private final DailyHoursValidator dailyHoursValidator;
    private final AppointmentDurationValidator appointmentDurationValidator;
    private final DayEvaluationService dayEvaluationService;
    private final SlotGenerationService slotGenerationService;
    private final ImpactCalculationService impactCalculationService;
    private final AppointmentCancellationService appointmentCancellationService;
    private final ConfigurationManagementService configurationManagementService;
    private final ConfigurationVersionService versionService;
    private final ConfigurationHistoryService configurationHistoryService;

    public CalendarConfigurationServiceImpl(CalendarConfigurationRepository repository,
            CalendarExceptionRepository exceptionRepository,
            ManualBlockRepository manualBlockRepository,
            CalendarConfigurationMapper mapper,
            AppointmentRepository appointmentRepository,
            UserService userService,
            UserRepository userRepository,
            WebSocketNotificationService webSocketNotificationService,
            com.ak4n1.turn_management.feature.notification.service.EmailService emailService,
            WeeklyConfigValidator weeklyConfigValidator,
            DailyHoursValidator dailyHoursValidator,
            AppointmentDurationValidator appointmentDurationValidator,
            DayEvaluationService dayEvaluationService,
            SlotGenerationService slotGenerationService,
            ImpactCalculationService impactCalculationService,
            AppointmentCancellationService appointmentCancellationService,
            ConfigurationManagementService configurationManagementService,
            ConfigurationVersionService versionService,
            ConfigurationHistoryService configurationHistoryService) {
        this.repository = repository;
        this.exceptionRepository = exceptionRepository;
        this.manualBlockRepository = manualBlockRepository;
        this.mapper = mapper;
        this.appointmentRepository = appointmentRepository;
        this.userService = userService;
        this.userRepository = userRepository;
        this.webSocketNotificationService = webSocketNotificationService;
        this.emailService = emailService;
        this.weeklyConfigValidator = weeklyConfigValidator;
        this.dailyHoursValidator = dailyHoursValidator;
        this.appointmentDurationValidator = appointmentDurationValidator;
        this.dayEvaluationService = dayEvaluationService;
        this.slotGenerationService = slotGenerationService;
        this.impactCalculationService = impactCalculationService;
        this.appointmentCancellationService = appointmentCancellationService;
        this.configurationManagementService = configurationManagementService;
        this.versionService = versionService;
        this.configurationHistoryService = configurationHistoryService;
    }

    @Override
    @Transactional
    public CalendarConfigurationResponse createWeeklyConfig(WeeklyConfigRequest request, Long userId) {
        logger.info("Creando nueva configuración semanal - Usuario: {}", userId);

        // 1. Validar request
        weeklyConfigValidator.validate(request);

        // 2. Convertir request a entidad embebida
        WeeklyConfig weeklyConfig = mapper.toWeeklyConfig(request);

        // 3. Validar lógica de negocio
        weeklyConfigValidator.validateDomain(weeklyConfig);

        // 4-7. Crear nueva configuración (maneja versión y desactivación internamente)
        CalendarConfiguration saved = configurationManagementService.createWeeklyConfiguration(
                weeklyConfig,
                userId,
                request.getNotes());
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
                appointmentCancellationService.cancelAffectedAppointmentsByDayClosure(appointmentsToCancel, userId, cancellationReason);
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
        Optional<CalendarConfiguration> active = configurationManagementService.getActiveConfiguration();
        return active.map(mapper::toResponse).orElse(null);
    }

    @Override
    @Transactional
    public CalendarConfigurationResponse configureDailyHours(DailyHoursConfigRequest request, Long userId) {
        logger.info("Configurando horarios diarios - Usuario: {}", userId);

        // 1. Validar request
        dailyHoursValidator.validate(request);

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = configurationManagementService.getActiveConfiguration()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. Debe crear primero una configuración semanal base.",
                        HttpStatus.BAD_REQUEST));

        // 3. Convertir request a entidades DailyHours
        List<DailyHours> dailyHoursList = mapper.toDailyHoursList(request);

        // 4. Validar que solo días abiertos tengan horarios
        dailyHoursValidator.validateOnlyOpenDaysHaveHours(dailyHoursList, activeConfig.getWeeklyConfig());

        // 5. Validar que no haya superposiciones
        dailyHoursValidator.validateNoOverlaps(dailyHoursList);

        // 6. Validar formato y lógica de rangos
        dailyHoursValidator.validateTimeRanges(dailyHoursList);

        // 7-11. Actualizar horarios diarios (maneja versión y desactivación internamente)
        CalendarConfiguration saved = configurationManagementService.updateDailyHours(
                activeConfig,
                dailyHoursList,
                userId,
                request.getNotes() != null ? request.getNotes() : "Configuración de horarios diarios");
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

    @Override
    @Transactional
    public CalendarConfigurationResponse configureAppointmentDuration(AppointmentDurationRequest request, Long userId) {
        logger.info("Configurando duración de turnos - Usuario: {}, Duración: {} minutos", userId,
                request.getDurationMinutes());

        // 1. Validar request
        appointmentDurationValidator.validate(request);

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = configurationManagementService.getActiveConfiguration()
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
        appointmentDurationValidator.validateDurationCompatibility(request.getDurationMinutes(), activeConfig.getDailyHours());

        // 6-10. Actualizar duración de turnos (maneja versión y desactivación internamente)
        CalendarConfiguration saved = configurationManagementService.updateAppointmentDuration(
                activeConfig,
                request.getDurationMinutes(),
                userId,
                request.getNotes() != null ? request.getNotes()
                        : "Configuración de duración de turnos: " + request.getDurationMinutes() + " minutos");
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

    @Override
    @Transactional
    public CalendarConfigurationResponse saveFullConfig(FullConfigRequest request, Long userId) {
        logger.info("Guardando configuración completa (atómico) - Usuario: {}", userId);

        // 1. Validar y mapear configuración semanal
        weeklyConfigValidator.validate(request.getWeeklyConfig());
        WeeklyConfig weeklyConfig = mapper.toWeeklyConfig(request.getWeeklyConfig());
        weeklyConfigValidator.validateDomain(weeklyConfig);

        // 2. Validar y mapear horarios diarios (pueden ser vacíos si todos los días están cerrados)
        List<DailyHours> dailyHoursList = new ArrayList<>();
        if (request.getDailyHours() != null && request.getDailyHours().getDailyHours() != null
                && !request.getDailyHours().getDailyHours().isEmpty()) {
            dailyHoursValidator.validate(request.getDailyHours());
            dailyHoursList = mapper.toDailyHoursList(request.getDailyHours());
            dailyHoursValidator.validateOnlyOpenDaysHaveHours(dailyHoursList, weeklyConfig);
            dailyHoursValidator.validateNoOverlaps(dailyHoursList);
            dailyHoursValidator.validateTimeRanges(dailyHoursList);
        }

        // 3. Validar duración (obligatoria) y compatibilidad con rangos
        if (request.getDurationMinutes() == null) {
            throw new ApiException("La duración del turno es obligatoria", HttpStatus.BAD_REQUEST);
        }
        AppointmentDurationRequest durationRequest = new AppointmentDurationRequest(
                request.getDurationMinutes(),
                request.getNotes());
        appointmentDurationValidator.validateRequest(durationRequest);
        if (request.getDurationMinutes() % 15 != 0) {
            throw new ApiException(
                    "La duración debe ser divisible por 15 minutos. Ejemplos válidos: 15, 30, 45, 60, 90, 120, etc.",
                    HttpStatus.BAD_REQUEST);
        }
        appointmentDurationValidator.validateDurationCompatibility(request.getDurationMinutes(), dailyHoursList);

        // 4. Guardar en una sola operación (una sola versión +1)
        String notes = request.getNotes() != null ? request.getNotes() : "Configuración completa (semanal, horarios, duración)";
        CalendarConfiguration saved = configurationManagementService.createFullConfiguration(
                weeklyConfig,
                dailyHoursList,
                request.getDurationMinutes(),
                userId,
                notes);

        logger.info("Configuración completa guardada - ID: {}, Versión: {}", saved.getId(), saved.getVersion());

        // 5. Notificaciones a admins y broadcast
        try {
            webSocketNotificationService.sendNotificationToAdmins(
                    NotificationType.CALENDAR_CONFIGURATION_CHANGED,
                    "Configuración de Calendario Actualizada",
                    String.format("Se ha actualizado la configuración completa (Versión %d). Revisa el impacto en turnos existentes.", saved.getVersion()),
                    RelatedEntityType.CALENDAR_CONFIGURATION,
                    saved.getId());
        } catch (Exception e) {
            logger.error("Error al enviar notificación WebSocket de cambio de configuración: {}", e.getMessage(), e);
        }
        try {
            java.util.Map<String, Object> additionalData = new java.util.HashMap<>();
            additionalData.put("configurationVersion", saved.getVersion());
            additionalData.put("durationMinutes", saved.getAppointmentDurationMinutes());
            webSocketNotificationService.broadcastGeneralAvailabilityUpdate(
                    String.format("La configuración del calendario ha cambiado (Versión %d). Por favor, actualiza la vista de disponibilidad.", saved.getVersion()),
                    additionalData);
        } catch (Exception e) {
            logger.error("Error al enviar actualización general de disponibilidad WebSocket: {}", e.getMessage(), e);
        }

        // 6. Procesar turnos afectados (mismo flujo que createWeeklyConfig)
        try {
            WeeklyConfigRequest weeklyReq = request.getWeeklyConfig();
            java.util.List<Long> appointmentIdsToCancel = weeklyReq != null ? weeklyReq.getAppointmentIdsToCancel() : null;
            Boolean autoCancel = weeklyReq != null && Boolean.TRUE.equals(weeklyReq.getAutoCancelAffectedAppointments());
            String cancellationReason = weeklyReq != null ? weeklyReq.getCancellationReason() : null;

            if (cancellationReason == null || cancellationReason.trim().isEmpty()) {
                cancellationReason = "Día cerrado según nueva configuración";
            }

            if (appointmentIdsToCancel == null || appointmentIdsToCancel.isEmpty()) {
                return mapper.toResponse(saved);
            }

            logger.info("Procesando {} turno(s) afectado(s) - AutoCancel: {}", appointmentIdsToCancel.size(), autoCancel);

            List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> affectedAppointments = appointmentRepository
                    .findAllById(appointmentIdsToCancel)
                    .stream()
                    .filter(a -> a.getState() == AppointmentState.CREATED || a.getState() == AppointmentState.CONFIRMED)
                    .collect(Collectors.toList());

            if (affectedAppointments.isEmpty()) {
                return mapper.toResponse(saved);
            }

            java.util.Map<Long, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo>> userAffectedAppointments = new java.util.HashMap<>();
            java.util.List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointmentsToCancel = new java.util.ArrayList<>();

            for (com.ak4n1.turn_management.feature.appointment.domain.Appointment appointment : affectedAppointments) {
                Long affectedUserId = appointment.getUserId();
                userAffectedAppointments.computeIfAbsent(affectedUserId, k -> new java.util.ArrayList<>())
                        .add(new com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo(
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

            for (java.util.Map.Entry<Long, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo>> entry : userAffectedAppointments.entrySet()) {
                try {
                    Long affectedUserId = entry.getKey();
                    java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> appointments = entry.getValue();
                    User affectedUser = usersMap.get(affectedUserId);
                    if (affectedUser == null) continue;

                    if (autoCancel) {
                        emailService.sendMassCancellationEmail(affectedUser, appointments, cancellationReason);
                        String message = appointments.size() == 1
                                ? String.format("Tu turno para el %s a las %s ha sido cancelado. Motivo: %s.", appointments.get(0).getDate(), appointments.get(0).getStartTime(), cancellationReason)
                                : String.format("Tus %d turno(s) han sido cancelados. Motivo: %s.", appointments.size(), cancellationReason);
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
                        String message = appointments.size() == 1
                                ? String.format("El día %s ha sido cerrado según la nueva configuración, pero tu turno a las %s seguirá siendo válido.", appointments.get(0).getDate(), appointments.get(0).getStartTime())
                                : String.format("Se han cerrado días según la nueva configuración, pero tus %d turno(s) siguen siendo válidos.", appointments.size());
                        webSocketNotificationService.sendNotificationToUser(
                                affectedUser.getId(),
                                NotificationType.DAY_CLOSED_WITH_APPOINTMENT,
                                appointments.size() == 1 ? "Día Cerrado - Turno Afectado" : "Días Cerrados - Turnos Afectados",
                                message,
                                RelatedEntityType.APPOINTMENT,
                                appointments.get(0).getAppointmentId(),
                                appointments.get(0).getAppointmentId());
                    }
                } catch (Exception e) {
                    logger.error("Error al enviar notificación - Usuario ID: {}, Error: {}", entry.getKey(), e.getMessage(), e);
                }
            }
        } catch (Exception e) {
            logger.error("Error al procesar turnos afectados: {}", e.getMessage(), e);
        }

        return mapper.toResponse(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public ConsolidatedCalendarResponse getConsolidatedCalendar(LocalDate startDate, LocalDate endDate) {
        logger.info("Solicitud de calendario consolidado - Rango: {} a {}", startDate, endDate);

        // 1. Validar rango de fechas
        DateUtils.validateDateRange(startDate, endDate);

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = configurationManagementService.getActiveConfiguration()
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
            ConsolidatedDayResponse day = dayEvaluationService.evaluateDay(currentDate, activeConfig, exceptionsInRange, blocksInRange);
            days.add(day);
            currentDate = currentDate.plusDays(1);
        }

        logger.info("Calendario consolidado generado - {} días evaluados", days.size());

        return new ConsolidatedCalendarResponse(days);
    }

    @Override
    @Transactional(readOnly = true)
    public PreviewImpactResponse previewImpact(PreviewImpactRequest request) {
        return impactCalculationService.previewImpact(request);
    }

    @Override
    @Transactional(readOnly = true)
    public AvailabilityResponse checkAvailability(LocalDate date) {
        logger.info("Evaluación de disponibilidad solicitada para fecha: {}", date);

        // 1. Validar que la fecha no sea pasada (usando GMT-3)
        LocalDate today = DateUtils.getTodayGMT3();
        if (date.isBefore(today)) {
            throw new ApiException(
                    "No se puede evaluar disponibilidad para fechas pasadas. Fecha solicitada: " + date,
                    HttpStatus.BAD_REQUEST);
        }

        // 2. Obtener configuración activa
        CalendarConfiguration activeConfig = configurationManagementService.getActiveConfiguration()
                .orElseThrow(() -> new ApiException(
                        "No existe una configuración activa. El sistema no está configurado.",
                        HttpStatus.SERVICE_UNAVAILABLE));

        // 3. Obtener excepciones y bloqueos para esta fecha
        List<CalendarException> exceptions = exceptionRepository
                .findByActiveTrueAndExceptionDateBetween(date, date);
        List<ManualBlock> blocks = manualBlockRepository
                .findByActiveTrueAndBlockDateBetween(date, date);

        // 4. Evaluar el día aplicando precedencia
        ConsolidatedDayResponse dayInfo = dayEvaluationService.evaluateDay(date, activeConfig, exceptions, blocks);

        // 5. Construir respuesta base
        AvailabilityResponse response = new AvailabilityResponse();
        response.setDate(date);
        response.setRuleApplied(dayInfo.getRuleType());
        response.setIsAvailable("OPEN".equals(dayInfo.getState()));
        response.setTimeRanges(dayInfo.getTimeRanges());
        response.setDescription(dayInfo.getRuleDescription());

        // 6. NUEVO: Si el día está cerrado, verificar si tiene turnos existentes
        if ("CLOSED".equals(dayInfo.getState()) || "PARTIAL".equals(dayInfo.getState())) {
            Integer existingAppointmentsCount = dayEvaluationService.countExistingAppointments(date);

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
        return slotGenerationService.getAvailableSlots(date);
    }

    @Override
    @Transactional(readOnly = true)
    public AvailabilityRangeResponse getAvailabilityRange(LocalDate startDate, LocalDate endDate) {
        logger.info("Solicitud de disponibilidad por rango recibida - Inicio: {}, Fin: {}", startDate, endDate);

        LocalDate today = DateUtils.getTodayGMT3();

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
        CalendarConfiguration activeConfig = configurationManagementService.getActiveConfiguration()
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
                ConsolidatedDayResponse dayInfo = dayEvaluationService.evaluateDay(currentDate, activeConfig, exceptions, blocks);

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
                        // Usar rangos base (sin excepciones ni bloqueos)
                        ConsolidatedDayResponse baseDayInfo = dayEvaluationService.evaluateDay(currentDate, activeConfig, new ArrayList<>(), new ArrayList<>());
                        if (baseDayInfo.getTimeRanges() != null && !baseDayInfo.getTimeRanges().isEmpty()) {
                            timeRangesToUse = baseDayInfo.getTimeRanges();
                        }
                    }
                }

                if (timeRangesToUse != null && !timeRangesToUse.isEmpty()) {
                    List<SlotResponse> allSlots = new ArrayList<>();

                    for (TimeRangeResponse timeRange : timeRangesToUse) {
                        List<SlotResponse> slotsFromRange = slotGenerationService.generateSlotsFromRange(
                                timeRange, appointmentDurationMinutes, blocksForDate);
                        allSlots.addAll(slotsFromRange);
                    }

                    // Excluir slots ocupados por turnos existentes (igual que /slots)
                    List<SlotResponse> slotsWithOccupancy = slotGenerationService.excludeOccupiedSlots(allSlots, currentDate);

                    totalSlots = allSlots.size();
                    availableSlots = (int) slotsWithOccupancy.stream()
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
        return configurationHistoryService.getConfigurationHistory();
    }
}
