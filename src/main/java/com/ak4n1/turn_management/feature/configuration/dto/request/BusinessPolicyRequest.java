package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

/**
 * DTO de request para configurar políticas de negocio.
 * 
 * Implementa US-T019.
 */
public class BusinessPolicyRequest {

    @Valid
    @NotNull(message = "maxAppointmentsPerUser es requerido")
    private MaxAppointmentsPerUserRequest maxAppointmentsPerUser;

    @Valid
    @NotNull(message = "timeWindows es requerido")
    private TimeWindowsRequest timeWindows;

    private Boolean allowMultipleReservations = false;

    private String notes;

    // Clases anidadas para estructurar la request
    public static class MaxAppointmentsPerUserRequest {
        @NotNull(message = "perDay es requerido")
        @Min(value = 1, message = "perDay debe ser mayor o igual a 1")
        private Integer perDay;

        @NotNull(message = "perWeek es requerido")
        @Min(value = 1, message = "perWeek debe ser mayor o igual a 1")
        private Integer perWeek;

        @Min(value = 1, message = "perMonth debe ser mayor o igual a 1")
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

    public static class TimeWindowsRequest {
        @NotNull(message = "minAdvanceHours es requerido")
        @Min(value = 0, message = "minAdvanceHours debe ser mayor o igual a 0")
        private Integer minAdvanceHours;

        @NotNull(message = "maxAdvanceDays es requerido")
        @Min(value = 1, message = "maxAdvanceDays debe ser mayor o igual a 1")
        private Integer maxAdvanceDays;

        @NotNull(message = "minimumCancellationWindowHours es requerido")
        @Min(value = 0, message = "minimumCancellationWindowHours debe ser mayor o igual a 0")
        private Integer minimumCancellationWindowHours;

        @NotNull(message = "createdAppointmentTtlMinutes es requerido")
        @Min(value = 1, message = "createdAppointmentTtlMinutes debe ser mayor o igual a 1")
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

    public MaxAppointmentsPerUserRequest getMaxAppointmentsPerUser() {
        return maxAppointmentsPerUser;
    }

    public void setMaxAppointmentsPerUser(MaxAppointmentsPerUserRequest maxAppointmentsPerUser) {
        this.maxAppointmentsPerUser = maxAppointmentsPerUser;
    }

    public TimeWindowsRequest getTimeWindows() {
        return timeWindows;
    }

    public void setTimeWindows(TimeWindowsRequest timeWindows) {
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
}

