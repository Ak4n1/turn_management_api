package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO de respuesta para la lista de solicitudes de reprogramación del admin.
 * Incluye información del usuario y paginación.
 */
public class AdminRescheduleRequestsResponse {

    private List<AdminRescheduleRequestResponse> requests;
    private long total;
    private int page;
    private int size;
    private int totalPages;

    public AdminRescheduleRequestsResponse() {
    }

    public AdminRescheduleRequestsResponse(List<AdminRescheduleRequestResponse> requests, 
                                         long total, 
                                         int page, 
                                         int size,
                                         int totalPages) {
        this.requests = requests;
        this.total = total;
        this.page = page;
        this.size = size;
        this.totalPages = totalPages;
    }

    // Getters and Setters
    public List<AdminRescheduleRequestResponse> getRequests() {
        return requests;
    }

    public void setRequests(List<AdminRescheduleRequestResponse> requests) {
        this.requests = requests;
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

