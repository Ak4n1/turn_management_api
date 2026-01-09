package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.util.List;

/**
 * DTO de respuesta para disponibilidad por rango de fechas.
 */
public class AvailabilityRangeResponse {

    /**
     * Fecha de inicio del rango evaluado.
     */
    private LocalDate startDate;

    /**
     * Fecha de fin del rango evaluado.
     */
    private LocalDate endDate;

    /**
     * Lista de días con su disponibilidad.
     * Los días están ordenados cronológicamente.
     */
    private List<DayAvailabilityResponse> days;

    public AvailabilityRangeResponse() {
    }

    public AvailabilityRangeResponse(LocalDate startDate, LocalDate endDate, List<DayAvailabilityResponse> days) {
        this.startDate = startDate;
        this.endDate = endDate;
        this.days = days;
    }

    // Getters and Setters
    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public List<DayAvailabilityResponse> getDays() {
        return days;
    }

    public void setDays(List<DayAvailabilityResponse> days) {
        this.days = days;
    }
}

