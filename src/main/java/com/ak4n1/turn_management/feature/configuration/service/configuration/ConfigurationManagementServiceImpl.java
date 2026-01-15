package com.ak4n1.turn_management.feature.configuration.service.configuration;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class ConfigurationManagementServiceImpl implements ConfigurationManagementService {

    private static final Logger logger = LoggerFactory.getLogger(ConfigurationManagementServiceImpl.class);

    private final CalendarConfigurationRepository repository;
    private final ConfigurationVersionService versionService;

    public ConfigurationManagementServiceImpl(
            CalendarConfigurationRepository repository,
            ConfigurationVersionService versionService) {
        this.repository = repository;
        this.versionService = versionService;
    }

    @Override
    @Transactional
    public CalendarConfiguration createWeeklyConfiguration(WeeklyConfig weeklyConfig, Long userId, String notes) {
        logger.info("Creando nueva configuración semanal - Usuario: {}", userId);

        // 1. Calcular nueva versión
        Integer newVersion = versionService.calculateNextVersion();

        // 2. Desactivar configuración anterior (si existe)
        versionService.deactivatePreviousConfiguration();

        // 3. Crear nueva configuración
        CalendarConfiguration newConfiguration = new CalendarConfiguration(
                weeklyConfig,
                userId,
                notes);
        newConfiguration.setVersion(newVersion);
        newConfiguration.setActive(true);

        // 4. Guardar
        CalendarConfiguration saved = repository.save(newConfiguration);
        logger.info("Configuración semanal creada exitosamente - ID: {}, Versión: {}", saved.getId(),
                saved.getVersion());

        return saved;
    }

    @Override
    @Transactional
    public CalendarConfiguration updateDailyHours(
            CalendarConfiguration activeConfig,
            List<DailyHours> dailyHoursList,
            Long userId,
            String notes) {
        logger.info("Actualizando horarios diarios - Usuario: {}, Configuración activa: Versión {}",
                userId, activeConfig.getVersion());

        // 1. Calcular nueva versión
        Integer newVersion = versionService.calculateNextVersion();

        // 2. Desactivar configuración anterior
        versionService.deactivatePreviousConfiguration();

        // 3. Crear nueva configuración (copiando la configuración semanal base)
        CalendarConfiguration newConfiguration = new CalendarConfiguration(
                activeConfig.getWeeklyConfig(),
                userId,
                notes != null ? notes : "Configuración de horarios diarios");
        newConfiguration.setVersion(newVersion);
        newConfiguration.setActive(true);

        // 4. Asociar horarios diarios a la nueva configuración
        for (DailyHours dailyHours : dailyHoursList) {
            dailyHours.setCalendarConfiguration(newConfiguration);
        }
        newConfiguration.setDailyHours(dailyHoursList);

        // 5. Guardar
        CalendarConfiguration saved = repository.save(newConfiguration);
        logger.info("Horarios diarios configurados exitosamente - ID: {}, Versión: {}", saved.getId(),
                saved.getVersion());

        return saved;
    }

    @Override
    @Transactional
    public CalendarConfiguration updateAppointmentDuration(
            CalendarConfiguration activeConfig,
            Integer durationMinutes,
            Long userId,
            String notes) {
        logger.info("Actualizando duración de turnos - Usuario: {}, Duración: {} minutos, Configuración activa: Versión {}",
                userId, durationMinutes, activeConfig.getVersion());

        // 1. Calcular nueva versión
        Integer newVersion = versionService.calculateNextVersion();

        // 2. Desactivar configuración anterior
        versionService.deactivatePreviousConfiguration();

        // 3. Crear nueva configuración (copiando todo de la anterior)
        String configurationNotes = notes != null ? notes
                : "Configuración de duración de turnos: " + durationMinutes + " minutos";
        CalendarConfiguration newConfiguration = new CalendarConfiguration(
                activeConfig.getWeeklyConfig(),
                userId,
                configurationNotes);
        newConfiguration.setVersion(newVersion);
        newConfiguration.setActive(true);
        newConfiguration.setAppointmentDurationMinutes(durationMinutes);

        // 4. Copiar horarios diarios de la configuración anterior
        List<DailyHours> copiedDailyHours = new ArrayList<>();
        if (activeConfig.getDailyHours() != null) {
            for (DailyHours originalDailyHours : activeConfig.getDailyHours()) {
                DailyHours copiedDailyHoursEntity = new DailyHours(originalDailyHours.getDayOfWeek());
                copiedDailyHoursEntity.setTimeRanges(new ArrayList<>(originalDailyHours.getTimeRanges()));
                copiedDailyHoursEntity.setCalendarConfiguration(newConfiguration);
                copiedDailyHours.add(copiedDailyHoursEntity);
            }
        }
        newConfiguration.setDailyHours(copiedDailyHours);

        // 5. Guardar
        CalendarConfiguration saved = repository.save(newConfiguration);
        logger.info("Duración de turnos configurada exitosamente - ID: {}, Versión: {}, Duración: {} minutos",
                saved.getId(), saved.getVersion(), saved.getAppointmentDurationMinutes());

        return saved;
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<CalendarConfiguration> getActiveConfiguration() {
        return repository.findByActiveTrue();
    }
}
