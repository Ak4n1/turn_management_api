package com.ak4n1.turn_management.feature.notification.domain;

/**
 * Enum que define los estados de un email fallido.
 */
public enum FailedEmailStatus {
    /**
     * Email falló al enviarse (estado inicial).
     */
    FAILED,
    
    /**
     * Email fue reenviado exitosamente.
     */
    RESENT,
    
    /**
     * Email fue descartado (no se reintentará más).
     */
    DISCARDED
}

