package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO de respuesta para configuración de calendario.
 */
public class CalendarConfigurationResponse {

    private Long id;
    private Integer version;
    private Boolean active;
    private WeeklyConfigResponse weeklyConfig;
    private List<DailyHoursResponse> dailyHours;
    private Integer appointmentDurationMinutes;
    private Long createdByUserId;
    private String notes;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public CalendarConfigurationResponse() {
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getVersion() {
        return version;
    }

    public void setVersion(Integer version) {
        this.version = version;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public WeeklyConfigResponse getWeeklyConfig() {
        return weeklyConfig;
    }

    public void setWeeklyConfig(WeeklyConfigResponse weeklyConfig) {
        this.weeklyConfig = weeklyConfig;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
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

    public List<DailyHoursResponse> getDailyHours() {
        return dailyHours;
    }

    public void setDailyHours(List<DailyHoursResponse> dailyHours) {
        this.dailyHours = dailyHours;
    }

    public Integer getAppointmentDurationMinutes() {
        return appointmentDurationMinutes;
    }

    public void setAppointmentDurationMinutes(Integer appointmentDurationMinutes) {
        this.appointmentDurationMinutes = appointmentDurationMinutes;
    }
}

