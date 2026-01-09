package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.time.LocalDate;

/**
 * DTO para representar información de un turno afectado por un día cerrado.
 * Usado para agrupar múltiples turnos por usuario en notificaciones.
 */
public class UserAffectedAppointmentInfo {
    private Long appointmentId;
    private LocalDate date;
    private String startTime;
    private String endTime;

    public UserAffectedAppointmentInfo() {
    }

    public UserAffectedAppointmentInfo(Long appointmentId, LocalDate date, String startTime, String endTime) {
        this.appointmentId = appointmentId;
        this.date = date;
        this.startTime = startTime;
        this.endTime = endTime;
    }

    public Long getAppointmentId() {
        return appointmentId;
    }

    public void setAppointmentId(Long appointmentId) {
        this.appointmentId = appointmentId;
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
}

