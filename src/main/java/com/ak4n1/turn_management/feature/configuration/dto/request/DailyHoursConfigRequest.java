package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * DTO para configurar horarios diarios.
 * 
 * Estructura: Map donde la clave es el nombre del día (monday, tuesday, etc.)
 * y el valor es una lista de rangos horarios.
 */
public class DailyHoursConfigRequest {

    @Valid
    private Map<String, List<TimeRangeRequest>> dailyHours;

    /**
     * Notas opcionales para esta configuración.
     */
    private String notes;

    public DailyHoursConfigRequest() {
    }

    // Getters and Setters
    public Map<String, List<TimeRangeRequest>> getDailyHours() {
        return dailyHours;
    }

    public void setDailyHours(Map<String, List<TimeRangeRequest>> dailyHours) {
        this.dailyHours = dailyHours;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
}

