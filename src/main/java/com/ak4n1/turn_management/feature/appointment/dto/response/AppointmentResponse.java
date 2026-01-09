package com.ak4n1.turn_management.feature.appointment.dto.response;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * DTO de respuesta para un turno (appointment).
 */
public class AppointmentResponse {

    private Long id;
    private Long userId;
    private LocalDate date;
    private String startTime;
    private String endTime;
    private Integer durationMinutes;
    private AppointmentState state;
    private Integer calendarConfigVersion;
    private LocalDateTime expiresAt;
    private LocalDateTime confirmedAt;
    private Long previousAppointmentId;
    private Boolean overridden;
    private String overrideJustification;
    private Boolean reminderSent;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public AppointmentResponse() {
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

    public String getEndTime() {
        return endTime;
    }

    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    public Integer getDurationMinutes() {
        return durationMinutes;
    }

    public void setDurationMinutes(Integer durationMinutes) {
        this.durationMinutes = durationMinutes;
    }

    public AppointmentState getState() {
        return state;
    }

    public void setState(AppointmentState state) {
        this.state = state;
    }

    public Integer getCalendarConfigVersion() {
        return calendarConfigVersion;
    }

    public void setCalendarConfigVersion(Integer calendarConfigVersion) {
        this.calendarConfigVersion = calendarConfigVersion;
    }

    public LocalDateTime getExpiresAt() {
        return expiresAt;
    }

    public void setExpiresAt(LocalDateTime expiresAt) {
        this.expiresAt = expiresAt;
    }

    public LocalDateTime getConfirmedAt() {
        return confirmedAt;
    }

    public void setConfirmedAt(LocalDateTime confirmedAt) {
        this.confirmedAt = confirmedAt;
    }

    public Long getPreviousAppointmentId() {
        return previousAppointmentId;
    }

    public void setPreviousAppointmentId(Long previousAppointmentId) {
        this.previousAppointmentId = previousAppointmentId;
    }

    public Boolean getOverridden() {
        return overridden;
    }

    public void setOverridden(Boolean overridden) {
        this.overridden = overridden;
    }

    public String getOverrideJustification() {
        return overrideJustification;
    }

    public void setOverrideJustification(String overrideJustification) {
        this.overrideJustification = overrideJustification;
    }

    public Boolean getReminderSent() {
        return reminderSent;
    }

    public void setReminderSent(Boolean reminderSent) {
        this.reminderSent = reminderSent;
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

