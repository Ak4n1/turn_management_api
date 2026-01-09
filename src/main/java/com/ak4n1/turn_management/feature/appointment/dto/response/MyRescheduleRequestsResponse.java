package com.ak4n1.turn_management.feature.appointment.dto.response;

import java.util.List;

/**
 * DTO de respuesta para la lista de solicitudes de reprogramación del usuario.
 * Incluye resumen con totales por estado.
 */
public class MyRescheduleRequestsResponse {

    private List<RescheduleRequestResponse> requests;
    private int total;
    private int pending;
    private int approved;
    private int rejected;
    private int expired;
    private int cancelled;

    public MyRescheduleRequestsResponse() {
    }

    public MyRescheduleRequestsResponse(List<RescheduleRequestResponse> requests, 
                                      int total, 
                                      int pending, 
                                      int approved, 
                                      int rejected,
                                      int expired,
                                      int cancelled) {
        this.requests = requests;
        this.total = total;
        this.pending = pending;
        this.approved = approved;
        this.rejected = rejected;
        this.expired = expired;
        this.cancelled = cancelled;
    }

    // Getters and Setters
    public List<RescheduleRequestResponse> getRequests() {
        return requests;
    }

    public void setRequests(List<RescheduleRequestResponse> requests) {
        this.requests = requests;
    }

    public int getTotal() {
        return total;
    }

    public void setTotal(int total) {
        this.total = total;
    }

    public int getPending() {
        return pending;
    }

    public void setPending(int pending) {
        this.pending = pending;
    }

    public int getApproved() {
        return approved;
    }

    public void setApproved(int approved) {
        this.approved = approved;
    }

    public int getRejected() {
        return rejected;
    }

    public void setRejected(int rejected) {
        this.rejected = rejected;
    }

    public int getExpired() {
        return expired;
    }

    public void setExpired(int expired) {
        this.expired = expired;
    }

    public int getCancelled() {
        return cancelled;
    }

    public void setCancelled(int cancelled) {
        this.cancelled = cancelled;
    }
}

