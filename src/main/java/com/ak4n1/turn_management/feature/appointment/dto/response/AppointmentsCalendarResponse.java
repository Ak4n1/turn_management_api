package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO de respuesta para el calendario de turnos (vista de administrador).
 * 
 * Implementa US-T017.1.
 */
public class AppointmentsCalendarResponse {
    
    private String startDate;
    private String endDate;
    private List<CalendarDayResponse> calendar;
    private int totalDays;
    private int totalSlots;
    private int totalAvailableSlots;
    private int totalOccupiedSlots;

    public AppointmentsCalendarResponse() {
    }

    public AppointmentsCalendarResponse(String startDate, String endDate, List<CalendarDayResponse> calendar,
                                       int totalDays, int totalSlots, int totalAvailableSlots, int totalOccupiedSlots) {
        this.startDate = startDate;
        this.endDate = endDate;
        this.calendar = calendar;
        this.totalDays = totalDays;
        this.totalSlots = totalSlots;
        this.totalAvailableSlots = totalAvailableSlots;
        this.totalOccupiedSlots = totalOccupiedSlots;
    }

    public String getStartDate() {
        return startDate;
    }

    public void setStartDate(String startDate) {
        this.startDate = startDate;
    }

    public String getEndDate() {
        return endDate;
    }

    public void setEndDate(String endDate) {
        this.endDate = endDate;
    }

    public List<CalendarDayResponse> getCalendar() {
        return calendar;
    }

    public void setCalendar(List<CalendarDayResponse> calendar) {
        this.calendar = calendar;
    }

    public int getTotalDays() {
        return totalDays;
    }

    public void setTotalDays(int totalDays) {
        this.totalDays = totalDays;
    }

    public int getTotalSlots() {
        return totalSlots;
    }

    public void setTotalSlots(int totalSlots) {
        this.totalSlots = totalSlots;
    }

    public int getTotalAvailableSlots() {
        return totalAvailableSlots;
    }

    public void setTotalAvailableSlots(int totalAvailableSlots) {
        this.totalAvailableSlots = totalAvailableSlots;
    }

    public int getTotalOccupiedSlots() {
        return totalOccupiedSlots;
    }

    public void setTotalOccupiedSlots(int totalOccupiedSlots) {
        this.totalOccupiedSlots = totalOccupiedSlots;
    }
}

