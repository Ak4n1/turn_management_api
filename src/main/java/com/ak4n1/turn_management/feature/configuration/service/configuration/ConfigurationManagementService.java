package com.ak4n1.turn_management.feature.configuration.service.configuration;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;

import java.util.List;
import java.util.Optional;

/**
 * Servicio para gestionar las operaciones CRUD de configuraciones del calendario.
 * Maneja la creación, actualización, versionado y persistencia de configuraciones.
 */
public interface ConfigurationManagementService {

    /**
     * Crea una nueva configuración semanal base.
     * 
     * @param weeklyConfig Configuración semanal
     * @param userId ID del usuario que crea la configuración
     * @param notes Notas adicionales
     * @return Configuración guardada
     */
    CalendarConfiguration createWeeklyConfiguration(WeeklyConfig weeklyConfig, Long userId, String notes);

    /**
     * Actualiza los horarios diarios de la configuración activa.
     * Crea una nueva versión copiando la configuración semanal base.
     * 
     * @param activeConfig Configuración activa actual
     * @param dailyHoursList Lista de horarios diarios
     * @param userId ID del usuario que actualiza
     * @param notes Notas adicionales
     * @return Nueva configuración guardada con los horarios actualizados
     */
    CalendarConfiguration updateDailyHours(CalendarConfiguration activeConfig, List<DailyHours> dailyHoursList, Long userId, String notes);

    /**
     * Actualiza la duración de turnos de la configuración activa.
     * Crea una nueva versión copiando toda la configuración anterior.
     * 
     * @param activeConfig Configuración activa actual
     * @param durationMinutes Nueva duración en minutos
     * @param userId ID del usuario que actualiza
     * @param notes Notas adicionales
     * @return Nueva configuración guardada con la duración actualizada
     */
    CalendarConfiguration updateAppointmentDuration(CalendarConfiguration activeConfig, Integer durationMinutes, Long userId, String notes);

    /**
     * Guarda la configuración completa (semanal + horarios diarios + duración) en una sola operación.
     * Incrementa la versión una única vez. Debe invocarse dentro de una transacción tras validar todo.
     *
     * @param weeklyConfig      Configuración semanal
     * @param dailyHoursList    Horarios diarios
     * @param durationMinutes   Duración de turnos en minutos
     * @param userId            ID del usuario que guarda
     * @param notes             Notas opcionales
     * @return Configuración guardada con versión +1
     */
    CalendarConfiguration createFullConfiguration(
            WeeklyConfig weeklyConfig,
            List<DailyHours> dailyHoursList,
            Integer durationMinutes,
            Long userId,
            String notes);

    /**
     * Obtiene la configuración activa del calendario.
     * 
     * @return Configuración activa o Optional vacío si no existe
     */
    Optional<CalendarConfiguration> getActiveConfiguration();
}
