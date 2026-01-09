package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;

/**
 * DTO para rango horario.
 */
public class TimeRangeRequest {

    @NotBlank(message = "El campo 'start' es requerido")
    @Pattern(regexp = "^([01]?[0-9]|2[0-3]):[0-5][0-9]$", message = "El formato de 'start' debe ser HH:mm (24 horas)")
    private String start;

    @NotBlank(message = "El campo 'end' es requerido")
    @Pattern(regexp = "^([01]?[0-9]|2[0-3]):[0-5][0-9]$", message = "El formato de 'end' debe ser HH:mm (24 horas)")
    private String end;

    public TimeRangeRequest() {
    }

    public TimeRangeRequest(String start, String end) {
        this.start = start;
        this.end = end;
    }

    // Getters and Setters
    public String getStart() {
        return start;
    }

    public void setStart(String start) {
        this.start = start;
    }

    public String getEnd() {
        return end;
    }

    public void setEnd(String end) {
        this.end = end;
    }
}

