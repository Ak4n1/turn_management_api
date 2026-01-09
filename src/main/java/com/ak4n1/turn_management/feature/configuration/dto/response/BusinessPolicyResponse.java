package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDateTime;

/**
 * DTO de respuesta para políticas de negocio.
 * 
 * Implementa US-T019.
 */
public class BusinessPolicyResponse {

    private Long id;
    private Boolean active;
    private MaxAppointmentsPerUserResponse maxAppointmentsPerUser;
    private TimeWindowsResponse timeWindows;
    private Boolean allowMultipleReservations;
    private String notes;
    private Long createdByUserId;
    private String createdByEmail;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // Clases anidadas para estructurar la response
    public static class MaxAppointmentsPerUserResponse {
        private Integer perDay;
        private Integer perWeek;
        private Integer perMonth;

        public Integer getPerDay() {
            return perDay;
        }

        public void setPerDay(Integer perDay) {
            this.perDay = perDay;
        }

        public Integer getPerWeek() {
            return perWeek;
        }

        public void setPerWeek(Integer perWeek) {
            this.perWeek = perWeek;
        }

        public Integer getPerMonth() {
            return perMonth;
        }

        public void setPerMonth(Integer perMonth) {
            this.perMonth = perMonth;
        }
    }

    public static class TimeWindowsResponse {
        private Integer minAdvanceHours;
        private Integer maxAdvanceDays;
        private Integer minimumCancellationWindowHours;
        private Integer createdAppointmentTtlMinutes;

        public Integer getMinAdvanceHours() {
            return minAdvanceHours;
        }

        public void setMinAdvanceHours(Integer minAdvanceHours) {
            this.minAdvanceHours = minAdvanceHours;
        }

        public Integer getMaxAdvanceDays() {
            return maxAdvanceDays;
        }

        public void setMaxAdvanceDays(Integer maxAdvanceDays) {
            this.maxAdvanceDays = maxAdvanceDays;
        }

        public Integer getMinimumCancellationWindowHours() {
            return minimumCancellationWindowHours;
        }

        public void setMinimumCancellationWindowHours(Integer minimumCancellationWindowHours) {
            this.minimumCancellationWindowHours = minimumCancellationWindowHours;
        }

        public Integer getCreatedAppointmentTtlMinutes() {
            return createdAppointmentTtlMinutes;
        }

        public void setCreatedAppointmentTtlMinutes(Integer createdAppointmentTtlMinutes) {
            this.createdAppointmentTtlMinutes = createdAppointmentTtlMinutes;
        }
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public MaxAppointmentsPerUserResponse getMaxAppointmentsPerUser() {
        return maxAppointmentsPerUser;
    }

    public void setMaxAppointmentsPerUser(MaxAppointmentsPerUserResponse maxAppointmentsPerUser) {
        this.maxAppointmentsPerUser = maxAppointmentsPerUser;
    }

    public TimeWindowsResponse getTimeWindows() {
        return timeWindows;
    }

    public void setTimeWindows(TimeWindowsResponse timeWindows) {
        this.timeWindows = timeWindows;
    }

    public Boolean getAllowMultipleReservations() {
        return allowMultipleReservations;
    }

    public void setAllowMultipleReservations(Boolean allowMultipleReservations) {
        this.allowMultipleReservations = allowMultipleReservations;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
    }

    public String getCreatedByEmail() {
        return createdByEmail;
    }

    public void setCreatedByEmail(String createdByEmail) {
        this.createdByEmail = createdByEmail;
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

