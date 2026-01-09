package com.ak4n1.turn_management.feature.appointment.domain;

import jakarta.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * Entidad que representa un turno (appointment).
 * 
 * Un turno tiene:
 * - Fecha y hora específica
 * - Duración configurada
 * - Estado que puede cambiar (CREATED, CONFIRMED, etc.)
 * - Usuario que lo reservó
 * - Versión del calendario que estaba activa cuando se creó
 * - TTL para expiración automática (si está en CREATED)
 * - Relación con turno anterior (si fue reprogramado)
 * - Versionado para optimistic locking
 */
@Entity
    @Table(name = "appointments", indexes = {
    @Index(name = "idx_appointment_user_date", columnList = "user_id,appointment_date"),
    @Index(name = "idx_appointment_date_time", columnList = "appointment_date,start_time"),
    @Index(name = "idx_appointment_state", columnList = "state"),
    @Index(name = "idx_appointment_expires_at", columnList = "expires_at"),
    @Index(name = "idx_appointment_day_of_week", columnList = "day_of_week")
})
public class Appointment {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Usuario que reservó el turno.
     */
    @Column(name = "user_id", nullable = false)
    private Long userId;

    /**
     * Fecha del turno.
     */
    @Column(name = "appointment_date", nullable = false)
    private LocalDate appointmentDate;

    /**
     * Día de la semana del turno (1=Lunes, 2=Martes, ..., 7=Domingo).
     * Se calcula automáticamente a partir de appointmentDate.
     * Permite filtrado eficiente por día de la semana en SQL.
     */
    @Column(name = "day_of_week", nullable = false)
    private Integer dayOfWeek;

    /**
     * Hora de inicio del turno (formato HH:mm).
     */
    @Column(name = "start_time", nullable = false)
    private LocalTime startTime;

    /**
     * Hora de fin del turno (formato HH:mm).
     */
    @Column(name = "end_time", nullable = false)
    private LocalTime endTime;

    /**
     * Duración del turno en minutos.
     */
    @Column(name = "duration_minutes", nullable = false)
    private Integer durationMinutes;

    /**
     * Estado actual del turno.
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "state", nullable = false)
    private AppointmentState state;

    /**
     * Versión del calendario que estaba activa cuando se creó el turno.
     * Permite mantener integridad histórica.
     */
    @Column(name = "calendar_config_version", nullable = false)
    private Integer calendarConfigVersion;

    /**
     * Fecha y hora de expiración (solo para turnos en estado CREATED).
     * Si no se confirma antes de esta fecha, el turno expira automáticamente.
     */
    @Column(name = "expires_at")
    private LocalDateTime expiresAt;

    /**
     * Fecha y hora de confirmación (solo si está CONFIRMED o superior).
     */
    @Column(name = "confirmed_at")
    private LocalDateTime confirmedAt;

    /**
     * ID del turno anterior si este turno fue creado por reprogramación.
     * Permite rastrear la cadena de reprogramaciones.
     */
    @Column(name = "previous_appointment_id")
    private Long previousAppointmentId;

    /**
     * Versión para optimistic locking.
     * Se incrementa automáticamente en cada actualización.
     */
    @Version
    @Column(name = "entity_version")
    private Long entityVersion;

    /**
     * Fecha y hora de creación del turno.
     */
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * Fecha y hora de última actualización.
     */
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    /**
     * Clave de idempotencia para evitar duplicados.
     * Única por request.
     */
    @Column(name = "idempotency_key", unique = true)
    private String idempotencyKey;

    /**
     * Usuario que creó el turno (para auditoría).
     * Normalmente es el mismo que userId, pero puede ser diferente si es admin.
     */
    @Column(name = "created_by_user_id", nullable = false)
    private Long createdByUserId;

    /**
     * Indica si este turno fue creado forzando las reglas normales (por admin).
     * Solo para casos excepcionales.
     */
    @Column(name = "overridden", nullable = false)
    private Boolean overridden = false;

    /**
     * Justificación para la creación forzada del turno (solo si overridden = true).
     * Obligatoria cuando el turno es creado fuera de reglas normales.
     */
    @Column(name = "override_justification", length = 1000)
    private String overrideJustification;

    /**
     * Indica si se ha enviado el recordatorio de este turno.
     * Evita enviar recordatorios duplicados.
     */
    @Column(name = "reminder_sent", nullable = false)
    private Boolean reminderSent = false;

    public Appointment() {
        this.overridden = false;
        this.reminderSent = false;
    }

    // Constructors
    public Appointment(Long userId, LocalDate appointmentDate, LocalTime startTime, 
                      LocalTime endTime, Integer durationMinutes, AppointmentState state,
                      Integer calendarConfigVersion, LocalDateTime expiresAt, String idempotencyKey,
                      Long createdByUserId) {
        this.userId = userId;
        this.appointmentDate = appointmentDate;
        // Calcular día de la semana (1=Lunes, 7=Domingo)
        this.dayOfWeek = appointmentDate.getDayOfWeek().getValue();
        this.startTime = startTime;
        this.endTime = endTime;
        this.durationMinutes = durationMinutes;
        this.state = state;
        this.calendarConfigVersion = calendarConfigVersion;
        this.expiresAt = expiresAt;
        this.idempotencyKey = idempotencyKey;
        this.createdByUserId = createdByUserId;
        this.overridden = false;
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    // Business logic methods

    /**
     * Confirma el turno, pasándolo de CREATED a CONFIRMED.
     * Remueve el TTL de expiración.
     */
    public void confirm() {
        if (this.state != AppointmentState.CREATED) {
            throw new IllegalStateException(
                "Solo se pueden confirmar turnos en estado CREATED. Estado actual: " + this.state);
        }
        this.state = AppointmentState.CONFIRMED;
        this.confirmedAt = LocalDateTime.now();
        this.expiresAt = null; // Remover TTL
    }

    /**
     * Cancela el turno.
     * Solo se pueden cancelar turnos en CREATED o CONFIRMED.
     */
    public void cancel() {
        if (this.state != AppointmentState.CREATED && this.state != AppointmentState.CONFIRMED) {
            throw new IllegalStateException(
                "Solo se pueden cancelar turnos en estado CREATED o CONFIRMED. Estado actual: " + this.state);
        }
        this.state = AppointmentState.CANCELLED;
        this.expiresAt = null; // Remover TTL si existe
    }

    /**
     * Cancela el turno por administrador.
     * Permite cancelar turnos en cualquier estado (excepto ya cancelados o completados).
     * No respeta ventanas de cancelación.
     * Establece el estado a CANCELLED_BY_ADMIN.
     */
    public void cancelByAdmin() {
        // Validar que no esté ya cancelado o completado
        if (this.state == AppointmentState.CANCELLED || 
            this.state == AppointmentState.CANCELLED_BY_ADMIN ||
            this.state == AppointmentState.COMPLETED) {
            throw new IllegalStateException(
                "No se puede cancelar un turno que ya está cancelado o completado. Estado actual: " + this.state);
        }
        this.state = AppointmentState.CANCELLED_BY_ADMIN;
        this.expiresAt = null; // Remover TTL si existe
        this.updatedAt = LocalDateTime.now();
    }

    /**
     * Marca el turno como expirado.
     * Solo se puede aplicar a turnos en estado CREATED.
     */
    public void expire() {
        if (this.state != AppointmentState.CREATED) {
            throw new IllegalStateException(
                "Solo se pueden expirar turnos en estado CREATED. Estado actual: " + this.state);
        }
        this.state = AppointmentState.EXPIRED;
        this.expiresAt = null;
    }

    /**
     * Verifica si el turno ha expirado.
     */
    public boolean isExpired() {
        return expiresAt != null && LocalDateTime.now().isAfter(expiresAt);
    }

    /**
     * Verifica si el turno es futuro.
     */
    public boolean isFuture() {
        LocalDateTime appointmentDateTime = LocalDateTime.of(appointmentDate, startTime);
        return appointmentDateTime.isAfter(LocalDateTime.now());
    }

    /**
     * Verifica si el horario del turno ya pasó.
     */
    public boolean isPast() {
        LocalDateTime appointmentDateTime = LocalDateTime.of(appointmentDate, startTime);
        return appointmentDateTime.isBefore(LocalDateTime.now());
    }

    /**
     * Marca el turno como no-show (ausente).
     * Solo se puede aplicar a turnos en estado CONFIRMED después del horario.
     */
    public void markAsNoShow() {
        if (this.state != AppointmentState.CONFIRMED) {
            throw new IllegalStateException(
                "Solo se pueden marcar como no-show turnos en estado CONFIRMED. Estado actual: " + this.state);
        }
        if (!isPast()) {
            throw new IllegalStateException(
                "No se puede marcar como no-show antes del horario del turno");
        }
        this.state = AppointmentState.NO_SHOW;
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

    public LocalDate getAppointmentDate() {
        return appointmentDate;
    }

    public void setAppointmentDate(LocalDate appointmentDate) {
        this.appointmentDate = appointmentDate;
        // Actualizar dayOfWeek cuando cambia la fecha
        if (appointmentDate != null) {
            this.dayOfWeek = appointmentDate.getDayOfWeek().getValue();
        }
    }

    public Integer getDayOfWeek() {
        return dayOfWeek;
    }

    public void setDayOfWeek(Integer dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    public LocalTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalTime startTime) {
        this.startTime = startTime;
    }

    public LocalTime getEndTime() {
        return endTime;
    }

    public void setEndTime(LocalTime endTime) {
        this.endTime = endTime;
    }

    public Integer getDurationMinutes() {
        return durationMinutes;
    }

    public void setDurationMinutes(Integer durationMinutes) {
        this.durationMinutes = durationMinutes;
    }

    public AppointmentState getState() {
        return state;
    }

    public void setState(AppointmentState state) {
        this.state = state;
    }

    public Integer getCalendarConfigVersion() {
        return calendarConfigVersion;
    }

    public void setCalendarConfigVersion(Integer calendarConfigVersion) {
        this.calendarConfigVersion = calendarConfigVersion;
    }

    public LocalDateTime getExpiresAt() {
        return expiresAt;
    }

    public void setExpiresAt(LocalDateTime expiresAt) {
        this.expiresAt = expiresAt;
    }

    public LocalDateTime getConfirmedAt() {
        return confirmedAt;
    }

    public void setConfirmedAt(LocalDateTime confirmedAt) {
        this.confirmedAt = confirmedAt;
    }

    public Long getPreviousAppointmentId() {
        return previousAppointmentId;
    }

    public void setPreviousAppointmentId(Long previousAppointmentId) {
        this.previousAppointmentId = previousAppointmentId;
    }

    public Long getEntityVersion() {
        return entityVersion;
    }

    public void setEntityVersion(Long entityVersion) {
        this.entityVersion = entityVersion;
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

    public String getIdempotencyKey() {
        return idempotencyKey;
    }

    public void setIdempotencyKey(String idempotencyKey) {
        this.idempotencyKey = idempotencyKey;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
    }

    public Boolean getOverridden() {
        return overridden;
    }

    public void setOverridden(Boolean overridden) {
        this.overridden = overridden;
    }

    public String getOverrideJustification() {
        return overrideJustification;
    }

    public void setOverrideJustification(String overrideJustification) {
        this.overrideJustification = overrideJustification;
    }

    public Boolean getReminderSent() {
        return reminderSent;
    }

    public void setReminderSent(Boolean reminderSent) {
        this.reminderSent = reminderSent;
    }

}

