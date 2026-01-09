package com.ak4n1.turn_management.feature.appointment.dto.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;
import java.time.LocalTime;

/**
 * DTO de request para reprogramación de turno por administrador.
 * 
 * Implementa US-T023.
 */
public class AdminRescheduleAppointmentRequest {

    @NotNull(message = "newDate es requerido")
    private LocalDate newDate;

    @NotNull(message = "newStartTime es requerido")
    private LocalTime newStartTime;

    @NotNull(message = "reason es requerido")
    @Size(min = 10, max = 500, message = "El motivo debe tener entre 10 y 500 caracteres")
    private String reason;

    public AdminRescheduleAppointmentRequest() {
    }

    public LocalDate getNewDate() {
        return newDate;
    }

    public void setNewDate(LocalDate newDate) {
        this.newDate = newDate;
    }

    public LocalTime getNewStartTime() {
        return newStartTime;
    }

    public void setNewStartTime(LocalTime newStartTime) {
        this.newStartTime = newStartTime;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }
}

