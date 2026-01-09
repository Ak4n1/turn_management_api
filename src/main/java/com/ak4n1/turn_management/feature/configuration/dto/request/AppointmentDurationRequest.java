package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

/**
 * DTO para configurar la duración de los turnos.
 * 
 * La duración debe ser:
 * - Entre 15 y 240 minutos
 * - Divisible por 15 minutos
 * - Compatible con los rangos horarios configurados (debe dividir cada rango)
 */
public class AppointmentDurationRequest {

    /**
     * Duración del turno en minutos.
     * Mínimo: 15 minutos
     * Máximo: 240 minutos (4 horas)
     * Debe ser divisible por 15
     */
    @NotNull(message = "La duración del turno es obligatoria")
    @Min(value = 15, message = "La duración mínima es de 15 minutos")
    @Max(value = 240, message = "La duración máxima es de 240 minutos (4 horas)")
    private Integer durationMinutes;

    /**
     * Notas o descripción de esta configuración (opcional).
     */
    private String notes;

    public AppointmentDurationRequest() {
    }

    public AppointmentDurationRequest(Integer durationMinutes, String notes) {
        this.durationMinutes = durationMinutes;
        this.notes = notes;
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

