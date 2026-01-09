package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Configuración de calendario con versionado.
 * Permite que cambios futuros no afecten turnos existentes.
 * Cada versión es inmutable una vez creada.
 */
@Entity
@Table(name = "calendar_configurations", indexes = {
    @Index(name = "idx_active", columnList = "active"),
    @Index(name = "idx_version", columnList = "version")
})
public class CalendarConfiguration {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Versión de la configuración (incrementa automáticamente).
     * Permite mantener historial y que turnos existentes usen versiones anteriores.
     */
    @Column(nullable = false, unique = true)
    private Integer version;

    /**
     * Indica si esta configuración está activa.
     * Solo una configuración puede estar activa a la vez.
     */
    @Column(nullable = false)
    private Boolean active = false;

    /**
     * Configuración semanal base (días abiertos/cerrados).
     */
    @Embedded
    private WeeklyConfig weeklyConfig;

    /**
     * Horarios diarios (rangos horarios por día).
     * Relación OneToMany con DailyHours.
     */
    @OneToMany(mappedBy = "calendarConfiguration", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER)
    private List<DailyHours> dailyHours = new ArrayList<>();

    /**
     * Duración de los turnos en minutos.
     * Impacta directamente en la generación de slots disponibles.
     * Debe ser divisible por 15 minutos y compatible con los rangos horarios configurados.
     * Rango válido: 15-240 minutos.
     */
    @Column(name = "appointment_duration_minutes")
    private Integer appointmentDurationMinutes;

    /**
     * Usuario que creó esta configuración (para auditoría).
     */
    @Column(name = "created_by_user_id", nullable = false)
    private Long createdByUserId;

    /**
     * Notas o descripción de esta versión (opcional).
     */
    @Column(length = 500)
    private String notes;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public CalendarConfiguration() {
    }

    public CalendarConfiguration(WeeklyConfig weeklyConfig, Long createdByUserId, String notes) {
        this.weeklyConfig = weeklyConfig;
        this.createdByUserId = createdByUserId;
        this.notes = notes;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getVersion() {
        return version;
    }

    public void setVersion(Integer version) {
        this.version = version;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public WeeklyConfig getWeeklyConfig() {
        return weeklyConfig;
    }

    public void setWeeklyConfig(WeeklyConfig weeklyConfig) {
        this.weeklyConfig = weeklyConfig;
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

    public List<DailyHours> getDailyHours() {
        return dailyHours;
    }

    public void setDailyHours(List<DailyHours> dailyHours) {
        this.dailyHours = dailyHours != null ? dailyHours : new ArrayList<>();
    }

    public Integer getAppointmentDurationMinutes() {
        return appointmentDurationMinutes;
    }

    public void setAppointmentDurationMinutes(Integer appointmentDurationMinutes) {
        this.appointmentDurationMinutes = appointmentDurationMinutes;
    }
}

