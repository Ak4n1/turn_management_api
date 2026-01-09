package com.ak4n1.turn_management.feature.appointment.dto.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import org.springframework.format.annotation.DateTimeFormat;

import java.time.LocalDate;

/**
 * DTO para crear una solicitud de reprogramación de turno.
 */
public class CreateRescheduleRequestRequest {

    /**
     * Nueva fecha solicitada para el turno.
     * Formato: YYYY-MM-DD
     */
    @NotNull(message = "La nueva fecha es obligatoria")
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
    private LocalDate newDate;

    /**
     * Nueva hora de inicio solicitada.
     * Formato: HH:mm (24 horas)
     */
    @NotNull(message = "La nueva hora de inicio es obligatoria")
    @Pattern(regexp = "^([0-1][0-9]|2[0-3]):[0-5][0-9]$", message = "La nueva hora de inicio debe tener formato HH:mm")
    private String newStartTime;

    /**
     * Motivo de la solicitud (opcional).
     */
    private String reason;

    public CreateRescheduleRequestRequest() {
    }

    public CreateRescheduleRequestRequest(LocalDate newDate, String newStartTime, String reason) {
        this.newDate = newDate;
        this.newStartTime = newStartTime;
        this.reason = reason;
    }

    // Getters and Setters
    public LocalDate getNewDate() {
        return newDate;
    }

    public void setNewDate(LocalDate newDate) {
        this.newDate = newDate;
    }

    public String getNewStartTime() {
        return newStartTime;
    }

    public void setNewStartTime(String newStartTime) {
        this.newStartTime = newStartTime;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }
}

