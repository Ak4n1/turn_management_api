package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

/**
 * Configuración de políticas de negocio.
 * 
 * Permite configurar límites de turnos, ventanas de tiempo y reglas de reservas.
 * Implementa US-T019.
 */
@Entity
@Table(name = "business_policies", indexes = {
    @Index(name = "idx_active", columnList = "active")
})
public class BusinessPolicy {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Indica si esta política está activa.
     * Solo una política puede estar activa a la vez.
     */
    @Column(nullable = false)
    private Boolean active = false;

    /**
     * Límite máximo de turnos por usuario por día.
     */
    @Column(name = "max_appointments_per_user_per_day", nullable = false)
    private Integer maxAppointmentsPerUserPerDay = 1;

    /**
     * Límite máximo de turnos por usuario por semana.
     */
    @Column(name = "max_appointments_per_user_per_week", nullable = false)
    private Integer maxAppointmentsPerUserPerWeek = 5;

    /**
     * Límite máximo de turnos por usuario por mes (opcional).
     */
    @Column(name = "max_appointments_per_user_per_month")
    private Integer maxAppointmentsPerUserPerMonth;

    /**
     * Anticipación mínima en horas para crear un turno.
     * Ej: 2 horas antes de la fecha/hora del turno.
     */
    @Column(name = "minimum_anticipation_hours", nullable = false)
    private Integer minimumAdvanceHours = 2;

    /**
     * Anticipación máxima en días para crear un turno.
     * Ej: hasta 30 días en el futuro.
     */
    @Column(name = "maximum_advance_days", nullable = false)
    private Integer maximumAdvanceDays = 30;

    /**
     * Ventana mínima en horas para cancelar un turno.
     * Ej: 2 horas antes de la fecha/hora del turno.
     */
    @Column(name = "minimum_cancellation_window_hours", nullable = false)
    private Integer minimumCancellationWindowHours = 2;

    /**
     * TTL (Time To Live) en minutos para turnos no confirmados.
     * Ej: 10 minutos.
     */
    @Column(name = "created_appointment_ttl_minutes", nullable = false)
    private Integer createdAppointmentTtlMinutes = 10;

    /**
     * Permite múltiples reservas simultáneas (aún no usado en el código).
     */
    @Column(name = "allow_multiple_reservations", nullable = false)
    private Boolean allowMultipleReservations = false;

    /**
     * Usuario que creó esta política (para auditoría).
     */
    @Column(name = "created_by_user_id", nullable = false)
    private Long createdByUserId;

    /**
     * Notas o descripción de esta política (opcional).
     */
    @Column(length = 500)
    private String notes;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public BusinessPolicy() {
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public Integer getMaxAppointmentsPerUserPerDay() {
        return maxAppointmentsPerUserPerDay;
    }

    public void setMaxAppointmentsPerUserPerDay(Integer maxAppointmentsPerUserPerDay) {
        this.maxAppointmentsPerUserPerDay = maxAppointmentsPerUserPerDay;
    }

    public Integer getMaxAppointmentsPerUserPerWeek() {
        return maxAppointmentsPerUserPerWeek;
    }

    public void setMaxAppointmentsPerUserPerWeek(Integer maxAppointmentsPerUserPerWeek) {
        this.maxAppointmentsPerUserPerWeek = maxAppointmentsPerUserPerWeek;
    }

    public Integer getMaxAppointmentsPerUserPerMonth() {
        return maxAppointmentsPerUserPerMonth;
    }

    public void setMaxAppointmentsPerUserPerMonth(Integer maxAppointmentsPerUserPerMonth) {
        this.maxAppointmentsPerUserPerMonth = maxAppointmentsPerUserPerMonth;
    }

    public Integer getMinimumAdvanceHours() {
        return minimumAdvanceHours;
    }

    public void setMinimumAdvanceHours(Integer minimumAdvanceHours) {
        this.minimumAdvanceHours = minimumAdvanceHours;
    }

    public Integer getMaximumAdvanceDays() {
        return maximumAdvanceDays;
    }

    public void setMaximumAdvanceDays(Integer maximumAdvanceDays) {
        this.maximumAdvanceDays = maximumAdvanceDays;
    }

    public Integer getMinimumCancellationWindowHours() {
        return minimumCancellationWindowHours;
    }

    public void setMinimumCancellationWindowHours(Integer minimumCancellationWindowHours) {
        this.minimumCancellationWindowHours = minimumCancellationWindowHours;
    }

    public Integer getCreatedAppointmentTtlMinutes() {
        return createdAppointmentTtlMinutes;
    }

    public void setCreatedAppointmentTtlMinutes(Integer createdAppointmentTtlMinutes) {
        this.createdAppointmentTtlMinutes = createdAppointmentTtlMinutes;
    }

    public Boolean getAllowMultipleReservations() {
        return allowMultipleReservations;
    }

    public void setAllowMultipleReservations(Boolean allowMultipleReservations) {
        this.allowMultipleReservations = allowMultipleReservations;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
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

