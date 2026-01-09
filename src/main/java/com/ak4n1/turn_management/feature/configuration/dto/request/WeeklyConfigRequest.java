package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.constraints.NotNull;

/**
 * DTO para crear/actualizar configuración semanal base.
 */
public class WeeklyConfigRequest {

    @NotNull(message = "El campo 'monday' es requerido")
    private Boolean monday;

    @NotNull(message = "El campo 'tuesday' es requerido")
    private Boolean tuesday;

    @NotNull(message = "El campo 'wednesday' es requerido")
    private Boolean wednesday;

    @NotNull(message = "El campo 'thursday' es requerido")
    private Boolean thursday;

    @NotNull(message = "El campo 'friday' es requerido")
    private Boolean friday;

    @NotNull(message = "El campo 'saturday' es requerido")
    private Boolean saturday;

    @NotNull(message = "El campo 'sunday' es requerido")
    private Boolean sunday;

    /**
     * Notas o descripción opcional para esta configuración.
     */
    private String notes;

    /**
     * Si es true, cancela automáticamente los turnos afectados cuando se cierra un día.
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

    public WeeklyConfigRequest() {
    }

    public WeeklyConfigRequest(Boolean monday, Boolean tuesday, Boolean wednesday,
                              Boolean thursday, Boolean friday, Boolean saturday, Boolean sunday) {
        this.monday = monday;
        this.tuesday = tuesday;
        this.wednesday = wednesday;
        this.thursday = thursday;
        this.friday = friday;
        this.saturday = saturday;
        this.sunday = sunday;
    }

    // Getters and Setters
    public Boolean getMonday() {
        return monday;
    }

    public void setMonday(Boolean monday) {
        this.monday = monday;
    }

    public Boolean getTuesday() {
        return tuesday;
    }

    public void setTuesday(Boolean tuesday) {
        this.tuesday = tuesday;
    }

    public Boolean getWednesday() {
        return wednesday;
    }

    public void setWednesday(Boolean wednesday) {
        this.wednesday = wednesday;
    }

    public Boolean getThursday() {
        return thursday;
    }

    public void setThursday(Boolean thursday) {
        this.thursday = thursday;
    }

    public Boolean getFriday() {
        return friday;
    }

    public void setFriday(Boolean friday) {
        this.friday = friday;
    }

    public Boolean getSaturday() {
        return saturday;
    }

    public void setSaturday(Boolean saturday) {
        this.saturday = saturday;
    }

    public Boolean getSunday() {
        return sunday;
    }

    public void setSunday(Boolean sunday) {
        this.sunday = sunday;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
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

