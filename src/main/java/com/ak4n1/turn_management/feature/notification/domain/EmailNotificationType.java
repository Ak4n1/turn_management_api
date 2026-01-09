package com.ak4n1.turn_management.feature.notification.domain;

/**
 * Enum que define los tipos de notificaciones por email.
 * 
 * Diferente de EmailTemplateType porque este enum se usa para
 * registrar emails fallidos y puede incluir tipos adicionales.
 */
public enum EmailNotificationType {
    APPOINTMENT_CREATED,
    APPOINTMENT_CONFIRMED,
    APPOINTMENT_CANCELLED_BY_USER,
    APPOINTMENT_CANCELLED_BY_ADMIN,
    APPOINTMENT_RESCHEDULED,
    RESCHEDULE_REJECTED,
    APPOINTMENT_REMINDER,
    WELCOME,
    EMAIL_VERIFICATION,
    PASSWORD_RESET,
    PASSWORD_RESET_CONFIRMATION,
    ACCOUNT_LOCKED
}

