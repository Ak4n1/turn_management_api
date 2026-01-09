package com.ak4n1.turn_management.feature.notification.domain;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * Entidad para notificaciones en el sistema (campanita).
 * 
 * Implementa US-T025.5.
 * Las notificaciones aparecen en el panel de administración y permiten
 * acciones rápidas como aprobar/rechazar solicitudes de reprogramación.
 */
@Entity
@Table(name = "system_notifications")
public class SystemNotification {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Tipo de notificación.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "type", nullable = false)
    private NotificationType type;

    /**
     * ID del usuario destinatario (normalmente admin).
     */
    @Column(name = "recipient_id", nullable = false)
    private Long recipientId;

    /**
     * Título de la notificación.
     */
    @Column(name = "title", nullable = false, length = 200)
    private String title;

    /**
     * Mensaje de la notificación.
     */
    @Column(name = "message", nullable = false, length = 1000)
    private String message;

    /**
     * Tipo de entidad relacionada (ej: RESCHEDULE_REQUEST, APPOINTMENT, etc.).
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "related_entity_type")
    private RelatedEntityType relatedEntityType;

    /**
     * ID de la entidad relacionada.
     */
    @Column(name = "related_entity_id")
    private Long relatedEntityId;

    /**
     * Indica si la notificación ha sido leída.
     */
    @Column(name = "is_read", nullable = false)
    private Boolean read = false;

    /**
     * Fecha y hora de lectura (null si no ha sido leída).
     */
    @Column(name = "read_at")
    private LocalDateTime readAt;

    /**
     * Fecha y hora de creación.
     */
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * Fecha y hora de última actualización.
     */
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public SystemNotification() {
        this.read = false;
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    public SystemNotification(NotificationType type, Long recipientId, String title, 
                              String message, RelatedEntityType relatedEntityType, 
                              Long relatedEntityId) {
        this.type = type;
        this.recipientId = recipientId;
        this.title = title;
        this.message = message;
        this.relatedEntityType = relatedEntityType;
        this.relatedEntityId = relatedEntityId;
        this.read = false;
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    /**
     * Marca la notificación como leída.
     */
    public void markAsRead() {
        this.read = true;
        this.readAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    // Lifecycle hooks
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        if (read == null) {
            read = false;
        }
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public NotificationType getType() {
        return type;
    }

    public void setType(NotificationType type) {
        this.type = type;
    }

    public Long getRecipientId() {
        return recipientId;
    }

    public void setRecipientId(Long recipientId) {
        this.recipientId = recipientId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
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

    public Boolean getRead() {
        return read;
    }

    public void setRead(Boolean read) {
        this.read = read;
    }

    public LocalDateTime getReadAt() {
        return readAt;
    }

    public void setReadAt(LocalDateTime readAt) {
        this.readAt = readAt;
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
}

