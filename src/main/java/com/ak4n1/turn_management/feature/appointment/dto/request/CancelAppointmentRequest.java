package com.ak4n1.turn_management.feature.appointment.dto.request;

/**
 * DTO para cancelar un turno (appointment).
 */
public class CancelAppointmentRequest {

    /**
     * Motivo de la cancelación (opcional).
     */
    private String reason;

    public CancelAppointmentRequest() {
    }

    public CancelAppointmentRequest(String reason) {
        this.reason = reason;
    }

    // Getters and Setters
    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }
}

