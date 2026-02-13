package com.ak4n1.turn_management.feature.notification.dto.request;

import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;

/**
 * DTO para solicitud de envío manual de notificaciones.
 * 
 * Implementa US-N011.
 */
public class SendManualNotificationRequest {

    /**
     * Tipo de destinatario: ALL_USERS o SPECIFIC_USER.
     */
    public enum RecipientType {
        ALL_USERS,
        SPECIFIC_USER
    }

    /**
     * Prioridad de la notificación (opcional).
     */
    public enum Priority {
        LOW,
        MEDIUM,
        HIGH
    }

    @NotNull(message = "El tipo de destinatario es requerido")
    private RecipientType recipientType;

    /**
     * Email del usuario específico (requerido si recipientType es SPECIFIC_USER).
     */
    private String recipientEmail;

    @NotNull(message = "El tipo de notificación es requerido")
    private NotificationType type;

    @NotBlank(message = "El título es requerido")
    @Size(max = 200, message = "El título no puede exceder 200 caracteres")
    private String title;

    @NotBlank(message = "El mensaje es requerido")
    @Size(max = 2000, message = "El mensaje no puede exceder 2000 caracteres")
    private String message;

    /**
     * Prioridad de la notificación (opcional, default: MEDIUM).
     */
    private Priority priority = Priority.MEDIUM;

    /**
     * Lista de emails de usuarios a excluir cuando recipientType es ALL_USERS (opcional).
     */
    private List<String> excludedEmails;

    public RecipientType getRecipientType() {
        return recipientType;
    }

    public void setRecipientType(RecipientType recipientType) {
        this.recipientType = recipientType;
    }

    public String getRecipientEmail() {
        return recipientEmail;
    }

    public void setRecipientEmail(String recipientEmail) {
        this.recipientEmail = recipientEmail;
    }

    public NotificationType getType() {
        return type;
    }

    public void setType(NotificationType type) {
        this.type = type;
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

    public Priority getPriority() {
        return priority;
    }

    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    public List<String> getExcludedEmails() {
        return excludedEmails;
    }

    public void setExcludedEmails(List<String> excludedEmails) {
        this.excludedEmails = excludedEmails;
    }
}

