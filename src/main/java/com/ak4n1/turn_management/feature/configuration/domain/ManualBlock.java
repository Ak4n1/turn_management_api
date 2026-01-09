package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Bloqueo operativo del calendario.
 * Tiene prioridad máxima sobre todas las reglas (base, excepciones, etc.).
 * Permite bloquear días completos o rangos horarios específicos.
 * 
 * Usos comunes:
 * - Mantenimiento programado
 * - Feriados inesperados
 * - Corte de luz
 * - Eventos internos
 */
@Entity
@Table(name = "manual_blocks", indexes = {
    @Index(name = "idx_block_date", columnList = "block_date"),
    @Index(name = "idx_active", columnList = "active")
})
public class ManualBlock {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Fecha del bloqueo.
     * Formato: YYYY-MM-DD
     */
    @Column(name = "block_date", nullable = false)
    private LocalDate blockDate;

    /**
     * Indica si el bloqueo es para el día completo (true) o solo un rango horario (false).
     */
    @Column(name = "is_full_day", nullable = false)
    private Boolean isFullDay;

    /**
     * Rango horario del bloqueo (solo si isFullDay = false).
     * Si isFullDay = true, este campo debe ser null.
     */
    @Embedded
    @AttributeOverrides({
        @AttributeOverride(name = "start", column = @Column(name = "block_start_time")),
        @AttributeOverride(name = "end", column = @Column(name = "block_end_time"))
    })
    private TimeRange timeRange;

    /**
     * Motivo o razón del bloqueo.
     * Obligatorio, mínimo 10 caracteres.
     */
    @Column(length = 500, nullable = false)
    private String reason;

    /**
     * Indica si el bloqueo está activo.
     */
    @Column(nullable = false)
    private Boolean active = true;

    /**
     * Indica si el bloqueo afecta turnos existentes.
     * Si es false y hay turnos afectados, no se permite crear el bloqueo.
     * Si es true, se permite crear el bloqueo aunque haya turnos afectados.
     */
    @Column(name = "affects_existing_appointments", nullable = false)
    private Boolean affectsExistingAppointments = false;

    /**
     * Usuario que creó este bloqueo (para auditoría).
     */
    @Column(name = "created_by_user_id", nullable = false)
    private Long createdByUserId;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public ManualBlock() {
    }

    public ManualBlock(LocalDate blockDate, Boolean isFullDay, TimeRange timeRange, 
                      String reason, Boolean affectsExistingAppointments, Long createdByUserId) {
        this.blockDate = blockDate;
        this.isFullDay = isFullDay;
        this.timeRange = timeRange;
        this.reason = reason;
        this.affectsExistingAppointments = affectsExistingAppointments;
        this.createdByUserId = createdByUserId;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalDate getBlockDate() {
        return blockDate;
    }

    public void setBlockDate(LocalDate blockDate) {
        this.blockDate = blockDate;
    }

    public Boolean getIsFullDay() {
        return isFullDay;
    }

    public void setIsFullDay(Boolean isFullDay) {
        this.isFullDay = isFullDay;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public Boolean getAffectsExistingAppointments() {
        return affectsExistingAppointments;
    }

    public void setAffectsExistingAppointments(Boolean affectsExistingAppointments) {
        this.affectsExistingAppointments = affectsExistingAppointments;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
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

