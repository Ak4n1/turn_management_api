package com.ak4n1.turn_management.feature.notification.domain;

/**
 * Tipos de notificaciones en el sistema.
 * 
 * Implementa US-T025.5 y US-N001.
 */
public enum NotificationType {
    /**
     * Solicitud de reprogramación pendiente de aprobación (admin).
     */
    RESCHEDULE_REQUEST_PENDING,

    /**
     * Turno forzado creado (para auditoría).
     */
    APPOINTMENT_OVERRIDE_CREATED,

    /**
     * Cambio en configuración de calendario.
     */
    CALENDAR_CONFIGURATION_CHANGED,

    /**
     * Bloqueo operativo creado.
     */
    BLOCK_CREATED,

    /**
     * Turno creado (usuario).
     */
    APPOINTMENT_CREATED,

    /**
     * Turno confirmado (usuario).
     */
    APPOINTMENT_CONFIRMED,

    /**
     * Turno cancelado (usuario).
     */
    APPOINTMENT_CANCELLED,

    /**
     * Turno cancelado por admin (usuario).
     */
    APPOINTMENT_CANCELLED_BY_ADMIN,

    /**
     * Turno reprogramado (usuario).
     */
    APPOINTMENT_RESCHEDULED,

    /**
     * Recordatorio de turno (usuario).
     */
    APPOINTMENT_REMINDER,

    /**
     * Turno expirado (usuario).
     */
    APPOINTMENT_EXPIRED,

    /**
     * Solicitud de reprogramación aprobada (usuario).
     */
    RESCHEDULE_REQUEST_APPROVED,

    /**
     * Solicitud de reprogramación rechazada (usuario).
     */
    RESCHEDULE_REQUEST_REJECTED,

    /**
     * Solicitud de reprogramación cancelada (admin).
     */
    RESCHEDULE_REQUEST_CANCELLED,

    /**
     * Turno marcado como no-show (usuario).
     */
    APPOINTMENT_NO_SHOW,

    /**
     * Anuncio del administrador (todos los usuarios).
     */
    ADMIN_ANNOUNCEMENT,

    /**
     * Mantenimiento del sistema (todos los usuarios).
     */
    SYSTEM_MAINTENANCE,

    /**
     * Actualización importante (todos los usuarios).
     */
    IMPORTANT_UPDATE,

    /**
     * Promoción u oferta (todos los usuarios).
     */
    PROMOTION,

    /**
     * Recordatorio general (todos los usuarios).
     */
    REMINDER,

    /**
     * NUEVO: Notificación enviada cuando un día es cerrado pero el usuario tiene un turno existente.
     */
    DAY_CLOSED_WITH_APPOINTMENT
}

