package com.ak4n1.turn_management.feature.appointment.dto.response;

import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * DTO de respuesta para una solicitud de reprogramación.
 */
public class RescheduleRequestResponse {

    private Long id;
    private Long appointmentId;
    private Long userId;
    private LocalDate currentDate;
    private String currentStartTime;
    private LocalDate requestedDate;
    private String requestedStartTime;
    private String reason;
    private RescheduleRequestState state;
    private String rejectionReason;
    private String expirationReason;
    private Long processedByAdminId;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime processedAt;

    public RescheduleRequestResponse() {
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getAppointmentId() {
        return appointmentId;
    }

    public void setAppointmentId(Long appointmentId) {
        this.appointmentId = appointmentId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public LocalDate getCurrentDate() {
        return currentDate;
    }

    public void setCurrentDate(LocalDate currentDate) {
        this.currentDate = currentDate;
    }

    public String getCurrentStartTime() {
        return currentStartTime;
    }

    public void setCurrentStartTime(String currentStartTime) {
        this.currentStartTime = currentStartTime;
    }

    public LocalDate getRequestedDate() {
        return requestedDate;
    }

    public void setRequestedDate(LocalDate requestedDate) {
        this.requestedDate = requestedDate;
    }

    public String getRequestedStartTime() {
        return requestedStartTime;
    }

    public void setRequestedStartTime(String requestedStartTime) {
        this.requestedStartTime = requestedStartTime;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public RescheduleRequestState getState() {
        return state;
    }

    public void setState(RescheduleRequestState state) {
        this.state = state;
    }

    public String getRejectionReason() {
        return rejectionReason;
    }

    public void setRejectionReason(String rejectionReason) {
        this.rejectionReason = rejectionReason;
    }

    public String getExpirationReason() {
        return expirationReason;
    }

    public void setExpirationReason(String expirationReason) {
        this.expirationReason = expirationReason;
    }

    public Long getProcessedByAdminId() {
        return processedByAdminId;
    }

    public void setProcessedByAdminId(Long processedByAdminId) {
        this.processedByAdminId = processedByAdminId;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    public LocalDateTime getProcessedAt() {
        return processedAt;
    }

    public void setProcessedAt(LocalDateTime processedAt) {
        this.processedAt = processedAt;
    }
}

