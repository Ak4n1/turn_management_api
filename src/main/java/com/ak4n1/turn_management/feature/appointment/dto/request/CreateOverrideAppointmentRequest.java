package com.ak4n1.turn_management.feature.appointment.dto.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;
import java.time.LocalTime;

/**
 * DTO de request para creación forzada de turno por administrador.
 * 
 * Implementa US-T025.
 * Permite crear turnos fuera de las reglas normales (ej: fuera de horario, en día cerrado).
 */
public class CreateOverrideAppointmentRequest {

    @NotNull(message = "date es requerido")
    private LocalDate date;

    @NotNull(message = "startTime es requerido")
    private LocalTime startTime;

    @NotNull(message = "userId es requerido")
    private Long userId;

    @NotNull(message = "justification es requerido")
    @Size(min = 20, max = 1000, message = "La justificación debe tener entre 20 y 1000 caracteres")
    private String justification;

    public CreateOverrideAppointmentRequest() {
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public LocalTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalTime startTime) {
        this.startTime = startTime;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getJustification() {
        return justification;
    }

    public void setJustification(String justification) {
        this.justification = justification;
    }
}

