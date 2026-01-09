package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.NotNull;
import java.util.List;

/**
 * DTO para horarios de un día específico.
 */
public class DailyHoursRequest {

    /**
     * Día de la semana: 1=Lunes, 2=Martes, ..., 7=Domingo
     */
    @NotNull(message = "El campo 'dayOfWeek' es requerido")
    @Min(value = 1, message = "El día de la semana debe estar entre 1 y 7")
    @Max(value = 7, message = "El día de la semana debe estar entre 1 y 7")
    private Integer dayOfWeek;

    /**
     * Lista de rangos horarios para este día.
     */
    @NotNull(message = "El campo 'timeRanges' es requerido")
    @Valid
    private List<TimeRangeRequest> timeRanges;

    public DailyHoursRequest() {
    }

    public DailyHoursRequest(Integer dayOfWeek, List<TimeRangeRequest> timeRanges) {
        this.dayOfWeek = dayOfWeek;
        this.timeRanges = timeRanges;
    }

    // Getters and Setters
    public Integer getDayOfWeek() {
        return dayOfWeek;
    }

    public void setDayOfWeek(Integer dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    public List<TimeRangeRequest> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRangeRequest> timeRanges) {
        this.timeRanges = timeRanges;
    }
}

