package com.ak4n1.turn_management.feature.appointment.domain;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * Entidad de auditoría para registrar todos los cambios de estado y acciones sobre turnos.
 * 
 * Cada cambio de estado o acción importante se registra aquí para mantener un historial completo.
 */
@Entity
@Table(name = "appointment_history", indexes = {
    @Index(name = "idx_history_appointment", columnList = "appointment_id"),
    @Index(name = "idx_history_user", columnList = "user_id"),
    @Index(name = "idx_history_created_at", columnList = "created_at")
})
public class AppointmentHistory {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * ID del turno relacionado.
     */
    @Column(name = "appointment_id", nullable = false)
    private Long appointmentId;

    /**
     * Usuario que realizó la acción.
     */
    @Column(name = "user_id", nullable = false)
    private Long userId;

    /**
     * Estado anterior del turno.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "previous_state")
    private AppointmentState previousState;

    /**
     * Nuevo estado del turno.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "new_state", nullable = false)
    private AppointmentState newState;

    /**
     * Tipo de acción realizada.
     * Ejemplos: CREATED, CONFIRMED, CANCELLED, RESCHEDULED, EXPIRED, NO_SHOW, COMPLETED
     */
    @Column(name = "action", nullable = false)
    private String action;

    /**
     * Motivo o descripción de la acción (opcional).
     */
    @Column(name = "reason", length = 500)
    private String reason;

    /**
     * Fecha y hora en que se registró la acción.
     */
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * IP desde donde se realizó la acción (para auditoría de seguridad).
     */
    @Column(name = "ip_address")
    private String ipAddress;

    public AppointmentHistory() {
    }

    public AppointmentHistory(Long appointmentId, Long userId, AppointmentState previousState,
                             AppointmentState newState, String action, String reason, String ipAddress) {
        this.appointmentId = appointmentId;
        this.userId = userId;
        this.previousState = previousState;
        this.newState = newState;
        this.action = action;
        this.reason = reason;
        this.ipAddress = ipAddress;
        this.createdAt = LocalDateTime.now();
    }

    @PrePersist
    protected void onCreate() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
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

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public String getIpAddress() {
        return ipAddress;
    }

    public void setIpAddress(String ipAddress) {
        this.ipAddress = ipAddress;
    }
}

