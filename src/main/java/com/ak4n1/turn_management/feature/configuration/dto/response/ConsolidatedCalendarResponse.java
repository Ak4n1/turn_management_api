package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.util.List;

/**
 * DTO de respuesta para el calendario consolidado.
 * Contiene la información de cada día en el rango consultado.
 */
public class ConsolidatedCalendarResponse {

    private List<ConsolidatedDayResponse> days;

    public ConsolidatedCalendarResponse() {
    }

    public ConsolidatedCalendarResponse(List<ConsolidatedDayResponse> days) {
        this.days = days;
    }

    public List<ConsolidatedDayResponse> getDays() {
        return days;
    }

    public void setDays(List<ConsolidatedDayResponse> days) {
        this.days = days;
    }
}

