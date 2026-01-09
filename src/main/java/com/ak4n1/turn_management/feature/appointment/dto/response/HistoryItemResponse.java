package com.ak4n1.turn_management.feature.appointment.dto.response;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;

/**
 * DTO para un item del historial de un turno.
 * 
 * Implementa US-T011.3.
 */
public class HistoryItemResponse {
    
    private Long id;
    private String action;
    private AppointmentState previousState;
    private AppointmentState newState;
    private String timestamp;
    private Long performedByUserId;
    private String performedByEmail;
    private String reason;
    private Long relatedAppointmentId;

    public HistoryItemResponse() {
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public AppointmentState getPreviousState() {
        return previousState;
    }

    public void setPreviousState(AppointmentState previousState) {
        this.previousState = previousState;
    }

    public AppointmentState getNewState() {
        return newState;
    }

    public void setNewState(AppointmentState newState) {
        this.newState = newState;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }

    public Long getPerformedByUserId() {
        return performedByUserId;
    }

    public void setPerformedByUserId(Long performedByUserId) {
        this.performedByUserId = performedByUserId;
    }

    public String getPerformedByEmail() {
        return performedByEmail;
    }

    public void setPerformedByEmail(String performedByEmail) {
        this.performedByEmail = performedByEmail;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public Long getRelatedAppointmentId() {
        return relatedAppointmentId;
    }

    public void setRelatedAppointmentId(Long relatedAppointmentId) {
        this.relatedAppointmentId = relatedAppointmentId;
    }
}

