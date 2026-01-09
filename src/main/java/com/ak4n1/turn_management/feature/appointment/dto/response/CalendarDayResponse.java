package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO para un día en el calendario de turnos.
 * 
 * Implementa US-T017.1.
 */
public class CalendarDayResponse {
    
    private String date;
    private List<CalendarSlotResponse> slots;
    private int totalSlots;
    private int availableSlots;
    private int occupiedSlots;

    public CalendarDayResponse() {
    }

    public CalendarDayResponse(String date, List<CalendarSlotResponse> slots, 
                              int totalSlots, int availableSlots, int occupiedSlots) {
        this.date = date;
        this.slots = slots;
        this.totalSlots = totalSlots;
        this.availableSlots = availableSlots;
        this.occupiedSlots = occupiedSlots;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public List<CalendarSlotResponse> getSlots() {
        return slots;
    }

    public void setSlots(List<CalendarSlotResponse> slots) {
        this.slots = slots;
    }

    public int getTotalSlots() {
        return totalSlots;
    }

    public void setTotalSlots(int totalSlots) {
        this.totalSlots = totalSlots;
    }

    public int getAvailableSlots() {
        return availableSlots;
    }

    public void setAvailableSlots(int availableSlots) {
        this.availableSlots = availableSlots;
    }

    public int getOccupiedSlots() {
        return occupiedSlots;
    }

    public void setOccupiedSlots(int occupiedSlots) {
        this.occupiedSlots = occupiedSlots;
    }
}

