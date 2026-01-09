package com.ak4n1.turn_management.feature.appointment.dto.request;

import jakarta.validation.constraints.Size;

/**
 * DTO para rechazar una solicitud de reprogramación.
 * 
 * Implementa US-T014.
 */
public class RejectRescheduleRequestRequest {

    /**
     * Motivo de rechazo (opcional pero recomendado).
     * Mínimo 5 caracteres si se proporciona.
     */
    @Size(min = 5, max = 500, message = "El motivo de rechazo debe tener entre 5 y 500 caracteres")
    private String rejectionReason;

    public RejectRescheduleRequestRequest() {
    }

    public RejectRescheduleRequestRequest(String rejectionReason) {
        this.rejectionReason = rejectionReason;
    }

    public String getRejectionReason() {
        return rejectionReason;
    }

    public void setRejectionReason(String rejectionReason) {
        this.rejectionReason = rejectionReason;
    }
}

