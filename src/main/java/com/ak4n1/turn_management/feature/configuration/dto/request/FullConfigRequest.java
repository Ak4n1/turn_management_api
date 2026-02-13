package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

/**
 * DTO para guardar la configuración completa del calendario en una sola operación:
 * configuración semanal, horarios diarios y duración de turnos.
 * Garantiza atomicidad (todo o nada) y un único incremento de versión.
 */
public class FullConfigRequest {

    @NotNull(message = "La configuración semanal es obligatoria")
    @Valid
    private WeeklyConfigRequest weeklyConfig;

    @Valid
    private DailyHoursConfigRequest dailyHours;

    @NotNull(message = "La duración del turno es obligatoria")
    private Integer durationMinutes;

    /**
     * Notas opcionales para la configuración guardada.
     */
    private String notes;

    public FullConfigRequest() {
    }

    public WeeklyConfigRequest getWeeklyConfig() {
        return weeklyConfig;
    }

    public void setWeeklyConfig(WeeklyConfigRequest weeklyConfig) {
        this.weeklyConfig = weeklyConfig;
    }

    public DailyHoursConfigRequest getDailyHours() {
        return dailyHours;
    }

    public void setDailyHours(DailyHoursConfigRequest dailyHours) {
        this.dailyHours = dailyHours;
    }

    public Integer getDurationMinutes() {
        return durationMinutes;
    }

    public void setDurationMinutes(Integer durationMinutes) {
        this.durationMinutes = durationMinutes;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
}
