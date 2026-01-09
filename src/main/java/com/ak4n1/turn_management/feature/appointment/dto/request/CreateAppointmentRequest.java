package com.ak4n1.turn_management.feature.appointment.dto.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import org.springframework.format.annotation.DateTimeFormat;

import java.time.LocalDate;

/**
 * DTO para crear un nuevo turno (appointment).
 */
public class CreateAppointmentRequest {

    /**
     * Fecha del turno.
     * Formato: YYYY-MM-DD
     * No puede ser una fecha pasada.
     */
    @NotNull(message = "La fecha es obligatoria")
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
    private LocalDate date;

    /**
     * Hora de inicio del turno.
     * Formato: HH:mm (24 horas)
     * Ejemplo: "10:00", "14:30"
     */
    @NotNull(message = "La hora de inicio es obligatoria")
    @Pattern(regexp = "^([0-1][0-9]|2[0-3]):[0-5][0-9]$", message = "La hora de inicio debe tener formato HH:mm")
    private String startTime;

    /**
     * Motivo opcional para la reserva.
     */
    private String notes;

    public CreateAppointmentRequest() {
    }

    public CreateAppointmentRequest(LocalDate date, String startTime, String notes) {
        this.date = date;
        this.startTime = startTime;
        this.notes = notes;
    }

    // Getters and Setters
    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public String getStartTime() {
        return startTime;
    }

    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
}

