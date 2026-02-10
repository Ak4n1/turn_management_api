package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;

/**
 * DTO para crear un bloqueo operativo del calendario.
 * 
 * Los bloqueos operativos tienen prioridad máxima sobre todas las reglas.
 * Permiten bloquear días completos o rangos horarios específicos.
 */
public class ManualBlockRequest {

    /**
     * Fecha del bloqueo.
     * Formato: YYYY-MM-DD
     * No puede ser una fecha pasada.
     */
    @NotNull(message = "La fecha es obligatoria")
    private LocalDate date;

    /**
     * Indica si el bloqueo es para el día completo (true) o solo un rango horario (false).
     */
    @NotNull(message = "El campo isFullDay es obligatorio")
    private Boolean isFullDay;

    /**
     * Rango horario del bloqueo.
     * Solo requerido si isFullDay = false.
     * Debe ser null si isFullDay = true.
     */
    @Valid
    private TimeRangeRequest timeRange;

    /**
     * Motivo o razón del bloqueo.
     * Mínimo 10 caracteres.
     */
    @NotBlank(message = "El motivo es obligatorio")
    @Size(min = 10, max = 500, message = "El motivo debe tener entre 10 y 500 caracteres")
    private String reason;

    /**
     * Indica si el bloqueo afecta turnos existentes.
     * Si es false y hay turnos afectados, no se permite crear el bloqueo (error 409).
     * Si es true, se permite crear el bloqueo aunque haya turnos afectados.
     */
    @NotNull(message = "El campo affectsExistingAppointments es obligatorio")
    private Boolean affectsExistingAppointments;

    /**
     * Si es true, se cancelan automáticamente los turnos afectados y se envía email de cancelación.
     * Si es false, solo se notifica por email (sin cancelar).
     * Solo se usa cuando se envían appointmentIdsToCancel (tras preview de impacto).
     */
    private Boolean autoCancelAffectedAppointments;

    /**
     * Motivo de cancelación para los turnos afectados.
     * Solo se usa si autoCancelAffectedAppointments es true.
     */
    private String cancellationReason;

    /**
     * IDs de turnos a cancelar o notificar (del preview de impacto).
     * Si se envía, se procesan según autoCancelAffectedAppointments.
     */
    private java.util.List<Long> appointmentIdsToCancel;

    public ManualBlockRequest() {
    }

    public ManualBlockRequest(LocalDate date, Boolean isFullDay, TimeRangeRequest timeRange, 
                             String reason, Boolean affectsExistingAppointments) {
        this.date = date;
        this.isFullDay = isFullDay;
        this.timeRange = timeRange;
        this.reason = reason;
        this.affectsExistingAppointments = affectsExistingAppointments;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public Boolean getIsFullDay() {
        return isFullDay;
    }

    public void setIsFullDay(Boolean isFullDay) {
        this.isFullDay = isFullDay;
    }

    public TimeRangeRequest getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRangeRequest timeRange) {
        this.timeRange = timeRange;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public Boolean getAffectsExistingAppointments() {
        return affectsExistingAppointments;
    }

    public void setAffectsExistingAppointments(Boolean affectsExistingAppointments) {
        this.affectsExistingAppointments = affectsExistingAppointments;
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

