package com.ak4n1.turn_management.feature.notification.service.template;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.templates.*;
import org.springframework.stereotype.Component;

@Component
public class EmailTemplateFactory {

    private final WelcomeEmailTemplate welcomeTemplate;
    private final EmailVerificationTemplate verificationTemplate;
    private final PasswordResetTemplate passwordResetTemplate;
    private final PasswordResetConfirmationTemplate passwordResetConfirmationTemplate;
    private final AccountLockedTemplate accountLockedTemplate;
    private final AppointmentCreatedTemplate appointmentCreatedTemplate;
    private final AppointmentConfirmedTemplate appointmentConfirmedTemplate;
    private final AppointmentCancelledByUserTemplate appointmentCancelledByUserTemplate;
    private final AppointmentCancelledByAdminTemplate appointmentCancelledByAdminTemplate;
    private final AppointmentRescheduledTemplate appointmentRescheduledTemplate;
    private final AppointmentReminderTemplate appointmentReminderTemplate;

    public EmailTemplateFactory(
            WelcomeEmailTemplate welcomeTemplate,
            EmailVerificationTemplate verificationTemplate,
            PasswordResetTemplate passwordResetTemplate,
            PasswordResetConfirmationTemplate passwordResetConfirmationTemplate,
            AccountLockedTemplate accountLockedTemplate,
            AppointmentCreatedTemplate appointmentCreatedTemplate,
            AppointmentConfirmedTemplate appointmentConfirmedTemplate,
            AppointmentCancelledByUserTemplate appointmentCancelledByUserTemplate,
            AppointmentCancelledByAdminTemplate appointmentCancelledByAdminTemplate,
            AppointmentRescheduledTemplate appointmentRescheduledTemplate,
            AppointmentReminderTemplate appointmentReminderTemplate) {
        this.welcomeTemplate = welcomeTemplate;
        this.verificationTemplate = verificationTemplate;
        this.passwordResetTemplate = passwordResetTemplate;
        this.passwordResetConfirmationTemplate = passwordResetConfirmationTemplate;
        this.accountLockedTemplate = accountLockedTemplate;
        this.appointmentCreatedTemplate = appointmentCreatedTemplate;
        this.appointmentConfirmedTemplate = appointmentConfirmedTemplate;
        this.appointmentCancelledByUserTemplate = appointmentCancelledByUserTemplate;
        this.appointmentCancelledByAdminTemplate = appointmentCancelledByAdminTemplate;
        this.appointmentRescheduledTemplate = appointmentRescheduledTemplate;
        this.appointmentReminderTemplate = appointmentReminderTemplate;
    }

    public EmailTemplate createTemplate(EmailTemplateType type, User user) {
        return switch (type) {
            case WELCOME -> welcomeTemplate.setUser(user);
            case EMAIL_VERIFICATION -> verificationTemplate.setUser(user);
            case PASSWORD_RESET -> passwordResetTemplate.setUser(user);
            case PASSWORD_RESET_CONFIRMATION -> passwordResetConfirmationTemplate.setUser(user);
            case ACCOUNT_LOCKED -> accountLockedTemplate.setUser(user);
            case APPOINTMENT_CREATED -> throw new IllegalArgumentException("APPOINTMENT_CREATED requiere Appointment. Use createAppointmentCreatedTemplate()");
            case APPOINTMENT_CONFIRMED -> throw new IllegalArgumentException("APPOINTMENT_CONFIRMED requiere Appointment. Use createAppointmentConfirmedTemplate()");
            case APPOINTMENT_CANCELLED_BY_USER -> throw new IllegalArgumentException("APPOINTMENT_CANCELLED_BY_USER requiere Appointment y reason. Use createAppointmentCancelledTemplate()");
            case APPOINTMENT_CANCELLED_BY_ADMIN -> throw new IllegalArgumentException("APPOINTMENT_CANCELLED_BY_ADMIN requiere Appointment y reason. Use createAppointmentCancelledTemplate()");
            case APPOINTMENT_RESCHEDULED -> throw new IllegalArgumentException("APPOINTMENT_RESCHEDULED requiere Appointment original, nuevo y reason. Use createAppointmentRescheduledTemplate()");
            case APPOINTMENT_REMINDER -> throw new IllegalArgumentException("APPOINTMENT_REMINDER requiere Appointment. Use createAppointmentReminderTemplate()");
        };
    }

    public EmailTemplate createVerificationTemplate(User user, String token) {
        return verificationTemplate.setUser(user).setVerificationToken(token);
    }

    public EmailTemplate createPasswordResetTemplate(User user, String token) {
        return passwordResetTemplate.setUser(user).setResetToken(token);
    }

    public EmailTemplate createAccountLockedTemplate(User user, int lockoutDurationMinutes) {
        return accountLockedTemplate.setUser(user).setLockoutDuration(lockoutDurationMinutes);
    }

    public EmailTemplate createAppointmentCreatedTemplate(User user, Appointment appointment) {
        return appointmentCreatedTemplate.setUser(user).setAppointment(appointment);
    }

    public EmailTemplate createAppointmentConfirmedTemplate(User user, Appointment appointment) {
        return appointmentConfirmedTemplate.setUser(user).setAppointment(appointment);
    }

    public EmailTemplate createAppointmentCancelledTemplate(User user, Appointment appointment, String reason, boolean cancelledByAdmin) {
        if (cancelledByAdmin) {
            return appointmentCancelledByAdminTemplate.setUser(user).setAppointment(appointment).setReason(reason);
        } else {
            return appointmentCancelledByUserTemplate.setUser(user).setAppointment(appointment).setReason(reason);
        }
    }

    public EmailTemplate createAppointmentRescheduledTemplate(User user, Appointment originalAppointment, Appointment newAppointment, String reason) {
        return appointmentRescheduledTemplate.setUser(user)
            .setOriginalAppointment(originalAppointment)
            .setNewAppointment(newAppointment)
            .setReason(reason);
    }

    public EmailTemplate createAppointmentReminderTemplate(User user, Appointment appointment) {
        return appointmentReminderTemplate.setUser(user).setAppointment(appointment);
    }

    public EmailTemplate createRescheduleRejectedTemplate(User user, Appointment appointment, String rejectionReason) {
        // Por ahora, usar un template simple. Se puede crear un template específico más adelante.
        // Usaremos el template de cancelación como base, pero con un mensaje diferente.
        return appointmentCancelledByAdminTemplate.setUser(user).setAppointment(appointment)
            .setReason("Solicitud de reprogramación rechazada: " + (rejectionReason != null ? rejectionReason : "Sin motivo especificado"));
    }
}

