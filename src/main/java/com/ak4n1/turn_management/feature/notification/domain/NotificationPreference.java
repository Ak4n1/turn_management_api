package com.ak4n1.turn_management.feature.notification.domain;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * Entidad para preferencias de notificación del usuario.
 * 
 * Permite a los usuarios configurar qué emails quieren recibir.
 * Implementa US-T035.
 */
@Entity
@Table(name = "notification_preferences", indexes = {
    @Index(name = "idx_preference_user", columnList = "user_id", unique = true)
})
public class NotificationPreference {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * ID del usuario propietario de las preferencias.
     */
    @Column(name = "user_id", nullable = false, unique = true)
    private Long userId;

    /**
     * Indica si el usuario quiere recibir emails en general.
     */
    @Column(name = "email_enabled", nullable = false)
    private Boolean emailEnabled = true;

    /**
     * Indica si el usuario quiere recibir emails cuando se crea un turno.
     */
    @Column(name = "appointment_created", nullable = false)
    private Boolean appointmentCreated = true;

    /**
     * Indica si el usuario quiere recibir emails cuando se confirma un turno.
     */
    @Column(name = "appointment_confirmed", nullable = false)
    private Boolean appointmentConfirmed = true;

    /**
     * Indica si el usuario quiere recibir emails cuando se cancela un turno.
     */
    @Column(name = "appointment_cancelled", nullable = false)
    private Boolean appointmentCancelled = true;

    /**
     * Indica si el usuario quiere recibir emails cuando se reprograma un turno.
     */
    @Column(name = "appointment_rescheduled", nullable = false)
    private Boolean appointmentRescheduled = true;

    /**
     * Indica si el usuario quiere recibir recordatorios de turnos.
     */
    @Column(name = "reminder_enabled", nullable = false)
    private Boolean reminderEnabled = true;

    /**
     * Horas antes del turno para enviar el recordatorio.
     * Por defecto: 12 horas.
     */
    @Column(name = "reminder_hours_before", nullable = false)
    private Integer reminderHoursBefore = 12;

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

    public NotificationPreference() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    public NotificationPreference(Long userId) {
        this();
        this.userId = userId;
    }

    /**
     * Actualiza la fecha de última modificación.
     */
    public void touch() {
        this.updatedAt = LocalDateTime.now();
    }

    // Getters and Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Boolean getEmailEnabled() {
        return emailEnabled;
    }

    public void setEmailEnabled(Boolean emailEnabled) {
        this.emailEnabled = emailEnabled;
    }

    public Boolean getAppointmentCreated() {
        return appointmentCreated;
    }

    public void setAppointmentCreated(Boolean appointmentCreated) {
        this.appointmentCreated = appointmentCreated;
    }

    public Boolean getAppointmentConfirmed() {
        return appointmentConfirmed;
    }

    public void setAppointmentConfirmed(Boolean appointmentConfirmed) {
        this.appointmentConfirmed = appointmentConfirmed;
    }

    public Boolean getAppointmentCancelled() {
        return appointmentCancelled;
    }

    public void setAppointmentCancelled(Boolean appointmentCancelled) {
        this.appointmentCancelled = appointmentCancelled;
    }

    public Boolean getAppointmentRescheduled() {
        return appointmentRescheduled;
    }

    public void setAppointmentRescheduled(Boolean appointmentRescheduled) {
        this.appointmentRescheduled = appointmentRescheduled;
    }

    public Boolean getReminderEnabled() {
        return reminderEnabled;
    }

    public void setReminderEnabled(Boolean reminderEnabled) {
        this.reminderEnabled = reminderEnabled;
    }

    public Integer getReminderHoursBefore() {
        return reminderHoursBefore;
    }

    public void setReminderHoursBefore(Integer reminderHoursBefore) {
        this.reminderHoursBefore = reminderHoursBefore;
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

