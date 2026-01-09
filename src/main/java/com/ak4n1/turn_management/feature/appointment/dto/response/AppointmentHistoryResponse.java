package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO de respuesta para el historial completo de un turno.
 * 
 * Implementa US-T011.3.
 */
public class AppointmentHistoryResponse {
    
    private Long appointmentId;
    private List<HistoryItemResponse> history;

    public AppointmentHistoryResponse() {
    }

    public AppointmentHistoryResponse(Long appointmentId, List<HistoryItemResponse> history) {
        this.appointmentId = appointmentId;
        this.history = history;
    }

    public Long getAppointmentId() {
        return appointmentId;
    }

    public void setAppointmentId(Long appointmentId) {
        this.appointmentId = appointmentId;
    }

    public List<HistoryItemResponse> getHistory() {
        return history;
    }

    public void setHistory(List<HistoryItemResponse> history) {
        this.history = history;
    }
}

