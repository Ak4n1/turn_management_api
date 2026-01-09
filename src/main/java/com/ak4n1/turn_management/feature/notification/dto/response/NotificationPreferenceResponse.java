package com.ak4n1.turn_management.feature.notification.dto.response;

import java.time.LocalDateTime;

/**
 * DTO para respuesta de preferencias de notificación.
 */
public class NotificationPreferenceResponse {

    private Long id;
    private Long userId;
    private Boolean emailEnabled;
    private Boolean appointmentCreated;
    private Boolean appointmentConfirmed;
    private Boolean appointmentCancelled;
    private Boolean appointmentRescheduled;
    private Boolean reminderEnabled;
    private Integer reminderHoursBefore;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public NotificationPreferenceResponse() {
    }

    public NotificationPreferenceResponse(Long id, Long userId, Boolean emailEnabled,
                                         Boolean appointmentCreated, Boolean appointmentConfirmed,
                                         Boolean appointmentCancelled, Boolean appointmentRescheduled,
                                         Boolean reminderEnabled, Integer reminderHoursBefore,
                                         LocalDateTime createdAt, LocalDateTime updatedAt) {
        this.id = id;
        this.userId = userId;
        this.emailEnabled = emailEnabled;
        this.appointmentCreated = appointmentCreated;
        this.appointmentConfirmed = appointmentConfirmed;
        this.appointmentCancelled = appointmentCancelled;
        this.appointmentRescheduled = appointmentRescheduled;
        this.reminderEnabled = reminderEnabled;
        this.reminderHoursBefore = reminderHoursBefore;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
    }

    // Getters and Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

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

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
}

