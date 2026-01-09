package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Excepción de calendario por fecha específica.
 * Permite abrir/cerrar días puntuales o modificar sus horarios.
 * Las excepciones tienen prioridad sobre las reglas base.
 */
@Entity
@Table(name = "calendar_exceptions", indexes = {
    @Index(name = "idx_exception_date", columnList = "exception_date"),
    @Index(name = "idx_active", columnList = "active")
})
public class CalendarException {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Fecha de la excepción.
     * Formato: YYYY-MM-DD
     */
    @Column(name = "exception_date", nullable = false, unique = true)
    private LocalDate exceptionDate;

    /**
     * Indica si el día está abierto (true) o cerrado (false).
     */
    @Column(name = "is_open", nullable = false)
    private Boolean isOpen;

    /**
     * Rangos horarios para esta excepción (solo si isOpen = true).
     * Relación OneToMany con TimeRange (embebida).
     */
    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(name = "calendar_exception_time_ranges", 
                     joinColumns = @JoinColumn(name = "exception_id"))
    private List<TimeRange> timeRanges = new ArrayList<>();

    /**
     * Motivo o razón de la excepción.
     */
    @Column(length = 500, nullable = false)
    private String reason;

    /**
     * Indica si la excepción está activa.
     */
    @Column(nullable = false)
    private Boolean active = true;

    /**
     * Usuario que creó esta excepción (para auditoría).
     */
    @Column(name = "created_by_user_id", nullable = false)
    private Long createdByUserId;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public CalendarException() {
    }

    public CalendarException(LocalDate exceptionDate, Boolean isOpen, String reason, Long createdByUserId) {
        this.exceptionDate = exceptionDate;
        this.isOpen = isOpen;
        this.reason = reason;
        this.createdByUserId = createdByUserId;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalDate getExceptionDate() {
        return exceptionDate;
    }

    public void setExceptionDate(LocalDate exceptionDate) {
        this.exceptionDate = exceptionDate;
    }

    public Boolean getIsOpen() {
        return isOpen;
    }

    public void setIsOpen(Boolean isOpen) {
        this.isOpen = isOpen;
    }

    public List<TimeRange> getTimeRanges() {
        return timeRanges != null ? timeRanges : new ArrayList<>();
    }

    public void setTimeRanges(List<TimeRange> timeRanges) {
        this.timeRanges = timeRanges != null ? timeRanges : new ArrayList<>();
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

