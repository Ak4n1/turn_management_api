package com.ak4n1.turn_management.feature.notification.dto.response;

import com.ak4n1.turn_management.feature.notification.domain.EmailNotificationType;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailStatus;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;

import java.time.LocalDateTime;

/**
 * DTO para respuesta de emails fallidos.
 */
public class FailedEmailNotificationResponse {

    private Long id;
    private EmailNotificationType notificationType;
    private String recipientEmail;
    private Long recipientUserId;
    private String subject;
    private RelatedEntityType relatedEntityType;
    private Long relatedEntityId;
    private FailedEmailStatus status;
    private Integer retryCount;
    private String errorMessage;
    private String errorType;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime lastRetryAt;
    private Long retriedByAdminId;

    public FailedEmailNotificationResponse() {
    }

    public FailedEmailNotificationResponse(Long id, EmailNotificationType notificationType, 
                                         String recipientEmail, Long recipientUserId,
                                         String subject, RelatedEntityType relatedEntityType,
                                         Long relatedEntityId, FailedEmailStatus status,
                                         Integer retryCount, String errorMessage, String errorType,
                                         LocalDateTime createdAt, LocalDateTime updatedAt,
                                         LocalDateTime lastRetryAt, Long retriedByAdminId) {
        this.id = id;
        this.notificationType = notificationType;
        this.recipientEmail = recipientEmail;
        this.recipientUserId = recipientUserId;
        this.subject = subject;
        this.relatedEntityType = relatedEntityType;
        this.relatedEntityId = relatedEntityId;
        this.status = status;
        this.retryCount = retryCount;
        this.errorMessage = errorMessage;
        this.errorType = errorType;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.lastRetryAt = lastRetryAt;
        this.retriedByAdminId = retriedByAdminId;
    }

    // Getters and Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public EmailNotificationType getNotificationType() {
        return notificationType;
    }

    public void setNotificationType(EmailNotificationType notificationType) {
        this.notificationType = notificationType;
    }

    public String getRecipientEmail() {
        return recipientEmail;
    }

    public void setRecipientEmail(String recipientEmail) {
        this.recipientEmail = recipientEmail;
    }

    public Long getRecipientUserId() {
        return recipientUserId;
    }

    public void setRecipientUserId(Long recipientUserId) {
        this.recipientUserId = recipientUserId;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public RelatedEntityType getRelatedEntityType() {
        return relatedEntityType;
    }

    public void setRelatedEntityType(RelatedEntityType relatedEntityType) {
        this.relatedEntityType = relatedEntityType;
    }

    public Long getRelatedEntityId() {
        return relatedEntityId;
    }

    public void setRelatedEntityId(Long relatedEntityId) {
        this.relatedEntityId = relatedEntityId;
    }

    public FailedEmailStatus getStatus() {
        return status;
    }

    public void setStatus(FailedEmailStatus status) {
        this.status = status;
    }

    public Integer getRetryCount() {
        return retryCount;
    }

    public void setRetryCount(Integer retryCount) {
        this.retryCount = retryCount;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public String getErrorType() {
        return errorType;
    }

    public void setErrorType(String errorType) {
        this.errorType = errorType;
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

    public LocalDateTime getLastRetryAt() {
        return lastRetryAt;
    }

    public void setLastRetryAt(LocalDateTime lastRetryAt) {
        this.lastRetryAt = lastRetryAt;
    }

    public Long getRetriedByAdminId() {
        return retriedByAdminId;
    }

    public void setRetriedByAdminId(Long retriedByAdminId) {
        this.retriedByAdminId = retriedByAdminId;
    }
}

