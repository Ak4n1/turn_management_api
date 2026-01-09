package com.ak4n1.turn_management.feature.appointment.domain;

import jakarta.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * Entidad que representa una solicitud de reprogramación de un turno.
 * 
 * Una solicitud de reprogramación permite al usuario solicitar cambiar
 * la fecha/hora de un turno confirmado. Requiere aprobación del administrador.
 */
@Entity
@Table(name = "reschedule_requests", indexes = {
    @Index(name = "idx_reschedule_appointment", columnList = "appointment_id"),
    @Index(name = "idx_reschedule_user", columnList = "user_id"),
    @Index(name = "idx_reschedule_state", columnList = "state"),
    @Index(name = "idx_reschedule_created_at", columnList = "created_at")
})
public class RescheduleRequest {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * ID del turno original que se desea reprogramar.
     */
    @Column(name = "appointment_id", nullable = false)
    private Long appointmentId;

    /**
     * ID del usuario que solicita la reprogramación.
     */
    @Column(name = "user_id", nullable = false)
    private Long userId;

    /**
     * Fecha actual del turno original.
     */
    @Column(name = "current_appointment_date", nullable = false)
    private LocalDate currentDate;

    /**
     * Hora de inicio actual del turno original.
     */
    @Column(name = "current_appointment_start_time", nullable = false)
    private LocalTime currentStartTime;

    /**
     * Nueva fecha solicitada.
     */
    @Column(name = "requested_date", nullable = false)
    private LocalDate requestedDate;

    /**
     * Nueva hora de inicio solicitada.
     */
    @Column(name = "requested_start_time", nullable = false)
    private LocalTime requestedStartTime;

    /**
     * Motivo de la solicitud (opcional).
     */
    @Column(name = "reason", length = 500)
    private String reason;

    /**
     * Estado actual de la solicitud.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "state", nullable = false)
    private RescheduleRequestState state;

    /**
     * Motivo de rechazo (si fue rechazada).
     */
    @Column(name = "rejection_reason", length = 500)
    private String rejectionReason;

    /**
     * Motivo de expiración (si expiró).
     */
    @Column(name = "expiration_reason", length = 500)
    private String expirationReason;

    /**
     * ID del administrador que procesó la solicitud (aprobó/rechazó).
     */
    @Column(name = "processed_by_admin_id")
    private Long processedByAdminId;

    /**
     * Fecha y hora de creación de la solicitud.
     */
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * Fecha y hora de última actualización.
     */
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    /**
     * Fecha y hora de procesamiento (aprobación/rechazo).
     */
    @Column(name = "processed_at")
    private LocalDateTime processedAt;

    public RescheduleRequest() {
    }

    public RescheduleRequest(Long appointmentId, Long userId, LocalDate currentDate,
                            LocalTime currentStartTime, LocalDate requestedDate,
                            LocalTime requestedStartTime, String reason) {
        this.appointmentId = appointmentId;
        this.userId = userId;
        this.currentDate = currentDate;
        this.currentStartTime = currentStartTime;
        this.requestedDate = requestedDate;
        this.requestedStartTime = requestedStartTime;
        this.reason = reason;
        this.state = RescheduleRequestState.PENDING_ADMIN_APPROVAL;
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }

    // Business logic methods

    /**
     * Aprueba la solicitud.
     */
    public void approve(Long adminId) {
        if (this.state != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            throw new IllegalStateException(
                "Solo se pueden aprobar solicitudes en estado PENDING_ADMIN_APPROVAL. Estado actual: " + this.state);
        }
        this.state = RescheduleRequestState.APPROVED;
        this.processedByAdminId = adminId;
        this.processedAt = LocalDateTime.now();
    }

    /**
     * Rechaza la solicitud.
     */
    public void reject(Long adminId, String rejectionReason) {
        if (this.state != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            throw new IllegalStateException(
                "Solo se pueden rechazar solicitudes en estado PENDING_ADMIN_APPROVAL. Estado actual: " + this.state);
        }
        this.state = RescheduleRequestState.REJECTED;
        this.processedByAdminId = adminId;
        this.rejectionReason = rejectionReason;
        this.processedAt = LocalDateTime.now();
    }

    /**
     * Expira la solicitud.
     */
    public void expire(String expirationReason) {
        if (this.state != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            throw new IllegalStateException(
                "Solo se pueden expirar solicitudes en estado PENDING_ADMIN_APPROVAL. Estado actual: " + this.state);
        }
        this.state = RescheduleRequestState.EXPIRED;
        this.expirationReason = expirationReason;
    }

    /**
     * Cancela la solicitud (automáticamente cuando se cancela el turno original).
     */
    public void cancel() {
        if (this.state != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            throw new IllegalStateException(
                "Solo se pueden cancelar solicitudes en estado PENDING_ADMIN_APPROVAL. Estado actual: " + this.state);
        }
        this.state = RescheduleRequestState.CANCELLED;
        this.expirationReason = "Turno original cancelado por el usuario";
    }

    /**
     * Cancela la solicitud manualmente por el usuario.
     */
    public void cancelByUser() {
        if (this.state != RescheduleRequestState.PENDING_ADMIN_APPROVAL) {
            throw new IllegalStateException(
                "Solo se pueden cancelar solicitudes en estado PENDING_ADMIN_APPROVAL. Estado actual: " + this.state);
        }
        this.state = RescheduleRequestState.CANCELLED;
        this.expirationReason = "Solicitud cancelada por el usuario";
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

    public LocalTime getCurrentStartTime() {
        return currentStartTime;
    }

    public void setCurrentStartTime(LocalTime currentStartTime) {
        this.currentStartTime = currentStartTime;
    }

    public LocalDate getRequestedDate() {
        return requestedDate;
    }

    public void setRequestedDate(LocalDate requestedDate) {
        this.requestedDate = requestedDate;
    }

    public LocalTime getRequestedStartTime() {
        return requestedStartTime;
    }

    public void setRequestedStartTime(LocalTime requestedStartTime) {
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

