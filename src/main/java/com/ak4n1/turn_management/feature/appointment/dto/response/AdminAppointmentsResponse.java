package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO de respuesta paginada para lista de turnos (vista de administrador).
 * 
 * Implementa US-T017.
 */
public class AdminAppointmentsResponse {
    
    private List<AdminAppointmentResponse> content;
    private long totalElements;
    private int totalPages;
    private int page;
    private int size;

    public AdminAppointmentsResponse() {
    }

    public AdminAppointmentsResponse(List<AdminAppointmentResponse> content, long totalElements, int totalPages, int page, int size) {
        this.content = content;
        this.totalElements = totalElements;
        this.totalPages = totalPages;
        this.page = page;
        this.size = size;
    }

    public List<AdminAppointmentResponse> getContent() {
        return content;
    }

    public void setContent(List<AdminAppointmentResponse> content) {
        this.content = content;
    }

    public long getTotalElements() {
        return totalElements;
    }

    public void setTotalElements(long totalElements) {
        this.totalElements = totalElements;
    }

    public int getTotalPages() {
        return totalPages;
    }

    public void setTotalPages(int totalPages) {
        this.totalPages = totalPages;
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
}

