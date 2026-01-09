package com.ak4n1.turn_management.feature.appointment.domain;

/**
 * Estados posibles de un turno (appointment).
 * 
 * El flujo de estados es:
 * CREATED -> CONFIRMED -> COMPLETED
 * CREATED -> EXPIRED (automático)
 * CREATED/CONFIRMED -> CANCELLED
 * CONFIRMED -> RESCHEDULED -> (nuevo turno en CONFIRMED)
 * CONFIRMED -> NO_SHOW
 */
public enum AppointmentState {
    /**
     * Turno creado pero no confirmado.
     * Tiene TTL (time-to-live) y puede expirar automáticamente.
     * Estado inicial al crear un turno.
     */
    CREATED,

    /**
     * Turno confirmado por el usuario.
     * Es permanente y ya no puede expirar.
     */
    CONFIRMED,

    /**
     * Turno cancelado por el usuario.
     * El slot queda liberado.
     */
    CANCELLED,

    /**
     * Turno cancelado por el administrador.
     * El slot queda liberado.
     * No se respetan ventanas de cancelación.
     */
    CANCELLED_BY_ADMIN,

    /**
     * Turno reprogramado.
     * El turno original queda en este estado y se crea un nuevo turno en CONFIRMED.
     */
    RESCHEDULED,

    /**
     * Turno expirado automáticamente (no confirmado a tiempo).
     * El slot queda liberado.
     */
    EXPIRED,

    /**
     * Usuario no se presentó al turno.
     * Registrado por el administrador.
     */
    NO_SHOW,

    /**
     * Turno completado (atendido).
     * Estado final exitoso.
     */
    COMPLETED
}

