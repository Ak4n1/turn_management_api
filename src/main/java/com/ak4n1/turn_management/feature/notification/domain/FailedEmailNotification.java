package com.ak4n1.turn_management.feature.notification.domain;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * Entidad que registra emails que fallaron al enviarse.
 * 
 * Permite a los administradores ver y reenviar emails fallidos.
 * Implementa US-T031.1.
 */
@Entity
@Table(name = "failed_email_notifications", indexes = {
    @Index(name = "idx_failed_email_type", columnList = "notification_type"),
    @Index(name = "idx_failed_email_status", columnList = "status"),
    @Index(name = "idx_failed_email_created_at", columnList = "created_at")
})
public class FailedEmailNotification {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Tipo de notificación que falló.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "notification_type", nullable = false)
    private EmailNotificationType notificationType;

    /**
     * Email del destinatario.
     */
    @Column(name = "recipient_email", nullable = false, length = 255)
    private String recipientEmail;

    /**
     * ID del usuario destinatario (puede ser null si el usuario fue eliminado).
     */
    @Column(name = "recipient_user_id")
    private Long recipientUserId;

    /**
     * Asunto del email.
     */
    @Column(name = "subject", nullable = false, length = 500)
    private String subject;

    /**
     * Cuerpo del email (HTML).
     */
    @Column(name = "body", columnDefinition = "TEXT", nullable = false)
    private String body;

    /**
     * Tipo de entidad relacionada (ej: APPOINTMENT).
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "related_entity_type")
    private RelatedEntityType relatedEntityType;

    /**
     * ID de la entidad relacionada (ej: ID del turno).
     */
    @Column(name = "related_entity_id")
    private Long relatedEntityId;

    /**
     * Estado del email fallido.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    private FailedEmailStatus status;

    /**
     * Número de intentos de reenvío realizados.
     */
    @Column(name = "retry_count", nullable = false)
    private Integer retryCount = 0;

    /**
     * Mensaje de error original.
     */
    @Column(name = "error_message", columnDefinition = "TEXT")
    private String errorMessage;

    /**
     * Tipo de error (clase de la excepción).
     */
    @Column(name = "error_type", length = 255)
    private String errorType;

    /**
     * Fecha y hora del primer fallo.
     */
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * Fecha y hora de última actualización.
     */
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    /**
     * Fecha y hora del último intento de reenvío.
     */
    @Column(name = "last_retry_at")
    private LocalDateTime lastRetryAt;

    /**
     * ID del admin que realizó el último reenvío manual (si aplica).
     */
    @Column(name = "retried_by_admin_id")
    private Long retriedByAdminId;

    public FailedEmailNotification() {
        this.status = FailedEmailStatus.FAILED;
        this.retryCount = 0;
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    public FailedEmailNotification(EmailNotificationType notificationType, String recipientEmail, 
                                   Long recipientUserId, String subject, String body,
                                   RelatedEntityType relatedEntityType, Long relatedEntityId,
                                   String errorMessage, String errorType) {
        this();
        this.notificationType = notificationType;
        this.recipientEmail = recipientEmail;
        this.recipientUserId = recipientUserId;
        this.subject = subject;
        this.body = body;
        this.relatedEntityType = relatedEntityType;
        this.relatedEntityId = relatedEntityId;
        this.errorMessage = errorMessage;
        this.errorType = errorType;
    }

    /**
     * Marca el email como reenviado exitosamente.
     */
    public void markAsResent() {
        this.status = FailedEmailStatus.RESENT;
        this.updatedAt = LocalDateTime.now();
        this.lastRetryAt = LocalDateTime.now();
    }

    /**
     * Incrementa el contador de reintentos.
     */
    public void incrementRetryCount() {
        this.retryCount++;
        this.updatedAt = LocalDateTime.now();
        this.lastRetryAt = LocalDateTime.now();
    }

    /**
     * Marca el email como fallido nuevamente después de un reintento fallido.
     */
    public void markAsFailedAgain(String errorMessage, String errorType) {
        this.status = FailedEmailStatus.FAILED;
        this.errorMessage = errorMessage;
        this.errorType = errorType;
        this.updatedAt = LocalDateTime.now();
        this.lastRetryAt = LocalDateTime.now();
    }

    /**
     * Marca el email como descartado (no se reintentará más).
     */
    public void markAsDiscarded() {
        this.status = FailedEmailStatus.DISCARDED;
        this.updatedAt = LocalDateTime.now();
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

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
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

