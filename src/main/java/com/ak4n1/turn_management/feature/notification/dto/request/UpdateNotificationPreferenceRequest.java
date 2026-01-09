package com.ak4n1.turn_management.feature.notification.dto.request;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

/**
 * DTO para actualizar preferencias de notificación.
 */
public class UpdateNotificationPreferenceRequest {

    @NotNull(message = "El estado de email habilitado es requerido")
    private Boolean emailEnabled;

    @NotNull(message = "El estado de notificación de turno creado es requerido")
    private Boolean appointmentCreated;

    @NotNull(message = "El estado de notificación de turno confirmado es requerido")
    private Boolean appointmentConfirmed;

    @NotNull(message = "El estado de notificación de turno cancelado es requerido")
    private Boolean appointmentCancelled;

    @NotNull(message = "El estado de notificación de turno reprogramado es requerido")
    private Boolean appointmentRescheduled;

    @NotNull(message = "El estado de recordatorios es requerido")
    private Boolean reminderEnabled;

    @Min(value = 1, message = "Las horas antes del recordatorio deben ser al menos 1")
    @Max(value = 168, message = "Las horas antes del recordatorio no pueden exceder 168 (7 días)")
    private Integer reminderHoursBefore;

    public UpdateNotificationPreferenceRequest() {
    }

    // Getters and Setters

    public Boolean getEmailEnabled() {
        return emailEnabled;
    }

    public void setEmailEnabled(Boolean emailEnabled) {
        this.emailEnabled = emailEnabled;
    }

    public Boolean getAppointmentCreated() {
        return appointmentCreated;
    }

    public void setAppointmentCreated(Boolean appointmentCreated) {
        this.appointmentCreated = appointmentCreated;
    }

    public Boolean getAppointmentConfirmed() {
        return appointmentConfirmed;
    }

    public void setAppointmentConfirmed(Boolean appointmentConfirmed) {
        this.appointmentConfirmed = appointmentConfirmed;
    }

    public Boolean getAppointmentCancelled() {
        return appointmentCancelled;
    }

    public void setAppointmentCancelled(Boolean appointmentCancelled) {
        this.appointmentCancelled = appointmentCancelled;
    }

    public Boolean getAppointmentRescheduled() {
        return appointmentRescheduled;
    }

    public void setAppointmentRescheduled(Boolean appointmentRescheduled) {
        this.appointmentRescheduled = appointmentRescheduled;
    }

    public Boolean getReminderEnabled() {
        return reminderEnabled;
    }

    public void setReminderEnabled(Boolean reminderEnabled) {
        this.reminderEnabled = reminderEnabled;
    }

    public Integer getReminderHoursBefore() {
        return reminderHoursBefore;
    }

    public void setReminderHoursBefore(Integer reminderHoursBefore) {
        this.reminderHoursBefore = reminderHoursBefore;
    }
}

