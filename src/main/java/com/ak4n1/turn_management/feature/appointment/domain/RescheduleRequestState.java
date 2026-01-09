package com.ak4n1.turn_management.feature.appointment.domain;

/**
 * Estados posibles de una solicitud de reprogramación (RescheduleRequest).
 * 
 * El flujo de estados es:
 * PENDING_ADMIN_APPROVAL -> APPROVED (si el admin aprueba)
 * PENDING_ADMIN_APPROVAL -> REJECTED (si el admin rechaza)
 * PENDING_ADMIN_APPROVAL -> EXPIRED (si expira automáticamente)
 * PENDING_ADMIN_APPROVAL -> CANCELLED (si el usuario cancela el turno original)
 */
public enum RescheduleRequestState {
    /**
     * Esperando aprobación del administrador.
     * Estado inicial al crear una solicitud de reprogramación.
     */
    PENDING_ADMIN_APPROVAL,

    /**
     * Aprobada por el administrador.
     * La reprogramación se ejecutó exitosamente.
     */
    APPROVED,

    /**
     * Rechazada por el administrador.
     * El usuario puede crear una nueva solicitud.
     */
    REJECTED,

    /**
     * Expirada automáticamente.
     * Ocurre cuando el slot solicitado ya no está disponible o pasó mucho tiempo.
     */
    EXPIRED,

    /**
     * Cancelada automáticamente.
     * Ocurre cuando el usuario cancela el turno original mientras hay una solicitud pendiente.
     */
    CANCELLED
}

