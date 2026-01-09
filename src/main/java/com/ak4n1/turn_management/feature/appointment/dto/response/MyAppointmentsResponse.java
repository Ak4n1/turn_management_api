package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO de respuesta para lista paginada de turnos del usuario.
 * 
 * Implementa US-T011.1.
 */
public class MyAppointmentsResponse {
    
    private List<AppointmentResponse> appointments;
    private long total;
    private int page;
    private int size;
    private int totalPages;

    public MyAppointmentsResponse() {
    }

    public MyAppointmentsResponse(List<AppointmentResponse> appointments, long total, int page, int size) {
        this.appointments = appointments;
        this.total = total;
        this.page = page;
        this.size = size;
        this.totalPages = size > 0 ? (int) Math.ceil((double) total / size) : 0;
    }

    public List<AppointmentResponse> getAppointments() {
        return appointments;
    }

    public void setAppointments(List<AppointmentResponse> appointments) {
        this.appointments = appointments;
    }

    public long getTotal() {
        return total;
    }

    public void setTotal(long total) {
        this.total = total;
    }

    public int getPage() {
        return page;
    }

    public void setPage(int page) {
        this.page = page;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getTotalPages() {
        return totalPages;
    }

    public void setTotalPages(int totalPages) {
        this.totalPages = totalPages;
    }
}

