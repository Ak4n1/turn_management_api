package com.ak4n1.turn_management.feature.appointment.dto.response;

/**
 * DTO para un slot en el calendario de turnos.
 * 
 * Implementa US-T017.1.
 */
public class CalendarSlotResponse {
    
    private String startTime;
    private String endTime;
    private CalendarAppointmentInfo appointment;
    private boolean available;

    public CalendarSlotResponse() {
    }

    public CalendarSlotResponse(String startTime, String endTime, CalendarAppointmentInfo appointment, boolean available) {
        this.startTime = startTime;
        this.endTime = endTime;
        this.appointment = appointment;
        this.available = available;
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

    public CalendarAppointmentInfo getAppointment() {
        return appointment;
    }

    public void setAppointment(CalendarAppointmentInfo appointment) {
        this.appointment = appointment;
    }

    public boolean isAvailable() {
        return available;
    }

    public void setAvailable(boolean available) {
        this.available = available;
    }
}

