package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;

public interface EmailService {

    void sendEmail(EmailTemplate template);

    void sendEmailAsync(EmailTemplate template);

    void sendWelcomeEmail(User user);

    void sendEmailVerification(User user, String token);

    void sendPasswordReset(User user, String token);

    void sendPasswordResetConfirmation(User user);

    void sendAccountLockedEmail(User user, int lockoutDurationMinutes);

    void sendAppointmentCreatedEmail(User user, Appointment appointment);

    void sendAppointmentConfirmedEmail(User user, Appointment appointment);

    void sendAppointmentCancelledEmail(User user, Appointment appointment, String reason, boolean cancelledByAdmin);

    void sendAppointmentRescheduledEmail(User user, Appointment originalAppointment, Appointment newAppointment, String reason);

    void sendRescheduleRejectedEmail(User user, Appointment appointment, String rejectionReason);

    void sendAppointmentReminderEmail(User user, Appointment appointment);

    /**
     * NUEVO: Envía email notificando que un día fue cerrado pero el turno del usuario sigue siendo válido.
     * @deprecated Usar sendDaysClosedNotificationEmail para enviar un solo email con múltiples turnos afectados.
     */
    @Deprecated
    void sendDayClosedNotificationEmail(User user, Appointment appointment, java.time.LocalDate closedDate, String reason);

    /**
     * NUEVO: Envía un solo email notificando que uno o más días fueron cerrados pero los turnos del usuario siguen siendo válidos.
     * Agrupa múltiples turnos afectados en un solo email para evitar spam.
     */
    void sendDaysClosedNotificationEmail(User user, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> affectedAppointments);

    /**
     * NUEVO: Envía un solo email notificando que uno o más turnos fueron cancelados masivamente por cierre de días.
     * Agrupa múltiples turnos cancelados en un solo email con la razón personalizable.
     * 
     * @param user Usuario afectado
     * @param cancelledAppointments Lista de turnos cancelados (con fecha y hora)
     * @param reason Razón de la cancelación (personalizable)
     */
    void sendMassCancellationEmail(User user, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> cancelledAppointments, String reason);
}

