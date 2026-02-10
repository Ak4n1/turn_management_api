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

    /**
     * Si es true, cancela automáticamente los turnos afectados cuando se cierra un día o se reducen horarios.
     * Por defecto es false (solo notifica).
     */
    private Boolean autoCancelAffectedAppointments = false;

    /**
     * Razón personalizable para la cancelación de turnos afectados.
     * Solo se usa si autoCancelAffectedAppointments es true.
     */
    private String cancellationReason;

    /**
     * IDs de turnos a cancelar directamente (opcional).
     * Si se proporciona, se cancelan solo estos turnos sin recalcular.
     * Si no se proporciona, se calculan los turnos afectados automáticamente.
     */
    private java.util.List<Long> appointmentIdsToCancel;

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

    public Boolean getAutoCancelAffectedAppointments() {
        return autoCancelAffectedAppointments;
    }

    public void setAutoCancelAffectedAppointments(Boolean autoCancelAffectedAppointments) {
        this.autoCancelAffectedAppointments = autoCancelAffectedAppointments;
    }

    public String getCancellationReason() {
        return cancellationReason;
    }

    public void setCancellationReason(String cancellationReason) {
        this.cancellationReason = cancellationReason;
    }

    public java.util.List<Long> getAppointmentIdsToCancel() {
        return appointmentIdsToCancel;
    }

    public void setAppointmentIdsToCancel(java.util.List<Long> appointmentIdsToCancel) {
        this.appointmentIdsToCancel = appointmentIdsToCancel;
    }
}

