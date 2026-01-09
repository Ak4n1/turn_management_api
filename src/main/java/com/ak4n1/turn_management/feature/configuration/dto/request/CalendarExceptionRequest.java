package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;
import java.util.List;

/**
 * DTO para crear una excepción de calendario por fecha específica.
 * 
 * Las excepciones permiten:
 * - Abrir días que normalmente están cerrados
 * - Cerrar días que normalmente están abiertos
 * - Modificar horarios para días específicos
 */
public class CalendarExceptionRequest {

    /**
     * Fecha de la excepción.
     * Formato: YYYY-MM-DD
     * No puede ser una fecha pasada.
     */
    @NotNull(message = "La fecha es obligatoria")
    private LocalDate date;

    /**
     * Indica si el día está abierto (true) o cerrado (false).
     * Si es true, debe proporcionar timeRanges.
     * Si es false, no debe proporcionar timeRanges.
     */
    @NotNull(message = "El campo isOpen es obligatorio")
    private Boolean isOpen;

    /**
     * Rangos horarios para esta excepción.
     * Solo requerido si isOpen = true.
     * Debe estar vacío o null si isOpen = false.
     */
    @Valid
    private List<TimeRangeRequest> timeRanges;

    /**
     * Motivo o razón de la excepción.
     * Mínimo 10 caracteres.
     */
    @NotBlank(message = "El motivo es obligatorio")
    @Size(min = 10, max = 500, message = "El motivo debe tener entre 10 y 500 caracteres")
    private String reason;

    public CalendarExceptionRequest() {
    }

    public CalendarExceptionRequest(LocalDate date, Boolean isOpen, List<TimeRangeRequest> timeRanges, String reason) {
        this.date = date;
        this.isOpen = isOpen;
        this.timeRanges = timeRanges;
        this.reason = reason;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public Boolean getIsOpen() {
        return isOpen;
    }

    public void setIsOpen(Boolean isOpen) {
        this.isOpen = isOpen;
    }

    public List<TimeRangeRequest> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRangeRequest> timeRanges) {
        this.timeRanges = timeRanges;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }
}

