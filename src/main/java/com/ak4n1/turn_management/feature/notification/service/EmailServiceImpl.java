package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.domain.EmailNotificationType;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailNotification;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.feature.notification.repository.FailedEmailNotificationRepository;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateFactory;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class EmailServiceImpl implements EmailService {

    private static final Logger logger = LoggerFactory.getLogger(EmailServiceImpl.class);

    private final JavaMailSender mailSender;
    private final EmailTemplateFactory templateFactory;
    private final FailedEmailNotificationRepository failedEmailRepository;
    private final NotificationPreferenceService preferenceService;

    public EmailServiceImpl(JavaMailSender mailSender, EmailTemplateFactory templateFactory,
                           FailedEmailNotificationRepository failedEmailRepository,
                           NotificationPreferenceService preferenceService) {
        this.mailSender = mailSender;
        this.templateFactory = templateFactory;
        this.failedEmailRepository = failedEmailRepository;
        this.preferenceService = preferenceService;
    }

    /**
     * Envía un email de forma síncrona.
     * 
     * NOTA: Este método lanza RuntimeException si falla.
     * Los métodos que llaman a este método desde operaciones principales
     * deben capturar las excepciones para no bloquear el flujo principal.
     * 
     * @param template Plantilla de email a enviar
     * @throws RuntimeException si falla el envío
     */
    @Override
    public void sendEmail(EmailTemplate template) {
        try {
            MimeMessage message = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");

            helper.setFrom(template.getFrom());
            helper.setTo(template.getTo());
            helper.setSubject(template.getSubject());
            helper.setText(template.getBody(), true); // true = HTML

            mailSender.send(message);
            logger.info("Email sent successfully to: {} - Subject: {}", template.getTo(), template.getSubject());
        } catch (MessagingException e) {
            // Log detallado del error
            logger.error("Error sending email to: {} - Subject: {} - Error type: {} - Message: {}", 
                template.getTo(), template.getSubject(), e.getClass().getSimpleName(), e.getMessage(), e);
            throw new RuntimeException("Failed to send email", e);
        } catch (Exception e) {
            // Capturar cualquier otro tipo de error
            logger.error("Unexpected error sending email to: {} - Subject: {} - Error type: {} - Message: {}", 
                template.getTo(), template.getSubject(), e.getClass().getSimpleName(), e.getMessage(), e);
            throw new RuntimeException("Failed to send email", e);
        }
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendEmailAsync(EmailTemplate template) {
        sendEmail(template);
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendWelcomeEmail(User user) {
        EmailTemplate template = templateFactory.createTemplate(EmailTemplateType.WELCOME, user);
        sendEmail(template); // Ya estamos en un método async
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendEmailVerification(User user, String token) {
        EmailTemplate template = templateFactory.createVerificationTemplate(user, token);
        sendEmail(template); // Ya estamos en un método async
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendPasswordReset(User user, String token) {
        EmailTemplate template = templateFactory.createPasswordResetTemplate(user, token);
        sendEmail(template); // Ya estamos en un método async, no necesitamos llamar a sendEmailAsync
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendPasswordResetConfirmation(User user) {
        EmailTemplate template = templateFactory.createTemplate(EmailTemplateType.PASSWORD_RESET_CONFIRMATION, user);
        sendEmail(template); // Ya estamos en un método async
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendAccountLockedEmail(User user, int lockoutDurationMinutes) {
        EmailTemplate template = templateFactory.createAccountLockedTemplate(user, lockoutDurationMinutes);
        sendEmail(template); // Ya estamos en un método async
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendAppointmentCreatedEmail(User user, Appointment appointment) {
        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "APPOINTMENT_CREATED")) {
            logger.debug("Email de turno creado no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = templateFactory.createAppointmentCreatedTemplate(user, appointment);
            sendEmail(template);
            logger.info("Email de turno creado enviado exitosamente - Usuario: {}, Turno ID: {}", 
                user.getEmail(), appointment.getId());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la creación del turno
            // Log detallado para debugging y auditoría
            logger.error("Error al enviar email de turno creado - Usuario: {}, Email: {}, Turno ID: {}, Fecha: {}, Hora: {} - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), appointment.getId(), appointment.getAppointmentDate(), 
                appointment.getStartTime(), e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_CREATED, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, appointment.getId(), e);
            }
        }
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendAppointmentConfirmedEmail(User user, Appointment appointment) {
        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "APPOINTMENT_CONFIRMED")) {
            logger.debug("Email de turno confirmado no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = templateFactory.createAppointmentConfirmedTemplate(user, appointment);
            sendEmail(template);
            logger.info("Email de turno confirmado enviado exitosamente - Usuario: {}, Turno ID: {}", 
                user.getEmail(), appointment.getId());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la confirmación del turno
            // Log detallado para debugging y auditoría
            logger.error("Error al enviar email de turno confirmado - Usuario: {}, Email: {}, Turno ID: {}, Fecha: {}, Hora: {} - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), appointment.getId(), appointment.getAppointmentDate(), 
                appointment.getStartTime(), e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_CONFIRMED, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, appointment.getId(), e);
            }
        }
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendAppointmentCancelledEmail(User user, Appointment appointment, String reason, boolean cancelledByAdmin) {
        // Verificar preferencias de notificación antes de enviar
        String notificationType = cancelledByAdmin ? "APPOINTMENT_CANCELLED_BY_ADMIN" : "APPOINTMENT_CANCELLED_BY_USER";
        if (!preferenceService.shouldSendNotification(user.getId(), notificationType)) {
            logger.debug("Email de turno cancelado no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = templateFactory.createAppointmentCancelledTemplate(user, appointment, reason, cancelledByAdmin);
            sendEmail(template);
            logger.info("Email de turno cancelado enviado exitosamente - Usuario: {}, Turno ID: {}, Cancelado por: {}", 
                user.getEmail(), appointment.getId(), cancelledByAdmin ? "Admin" : "Usuario");
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la cancelación del turno
            // Log detallado para debugging y auditoría
            logger.error("Error al enviar email de turno cancelado - Usuario: {}, Email: {}, Turno ID: {}, Cancelado por: {}, Fecha: {}, Hora: {} - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), appointment.getId(), cancelledByAdmin ? "Admin" : "Usuario",
                appointment.getAppointmentDate(), appointment.getStartTime(), e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                EmailNotificationType failedNotificationType = cancelledByAdmin 
                    ? EmailNotificationType.APPOINTMENT_CANCELLED_BY_ADMIN 
                    : EmailNotificationType.APPOINTMENT_CANCELLED_BY_USER;
                registerFailedEmail(failedNotificationType, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, appointment.getId(), e);
            }
        }
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendAppointmentRescheduledEmail(User user, Appointment originalAppointment, Appointment newAppointment, String reason) {
        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "APPOINTMENT_RESCHEDULED")) {
            logger.debug("Email de turno reprogramado no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = templateFactory.createAppointmentRescheduledTemplate(user, originalAppointment, newAppointment, reason);
            sendEmail(template);
            logger.info("Email de turno reprogramado enviado exitosamente - Usuario: {}, Turno original ID: {}, Nuevo turno ID: {}", 
                user.getEmail(), originalAppointment.getId(), newAppointment.getId());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la reprogramación del turno
            // Log detallado para debugging y auditoría
            logger.error("Error al enviar email de turno reprogramado - Usuario: {}, Email: {}, Turno original ID: {} ({} {}), Nuevo turno ID: {} ({} {}) - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), originalAppointment.getId(), originalAppointment.getAppointmentDate(), 
                originalAppointment.getStartTime(), newAppointment.getId(), newAppointment.getAppointmentDate(), 
                newAppointment.getStartTime(), e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido (relacionado con el nuevo turno)
            if (template != null) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_RESCHEDULED, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, newAppointment.getId(), e);
            }
        }
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendRescheduleRejectedEmail(User user, Appointment appointment, String rejectionReason) {
        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "RESCHEDULE_REJECTED")) {
            logger.debug("Email de rechazo de reprogramación no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = templateFactory.createRescheduleRejectedTemplate(user, appointment, rejectionReason);
            sendEmail(template);
            logger.info("Email de rechazo de reprogramación enviado exitosamente - Usuario: {}, Turno ID: {}", 
                user.getEmail(), appointment.getId());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear el rechazo
            logger.error("Error al enviar email de rechazo de reprogramación - Usuario: {}, Email: {}, Turno ID: {} - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), appointment.getId(), e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                registerFailedEmail(EmailNotificationType.RESCHEDULE_REJECTED, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, appointment.getId(), e);
            }
        }
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendAppointmentReminderEmail(User user, Appointment appointment) {
        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "APPOINTMENT_REMINDER")) {
            logger.debug("Email de recordatorio no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = templateFactory.createAppointmentReminderTemplate(user, appointment);
            sendEmail(template);
            logger.info("Email de recordatorio enviado exitosamente - Usuario: {}, Turno ID: {}", 
                user.getEmail(), appointment.getId());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear el proceso de recordatorios
            // Log detallado para debugging y auditoría
            logger.error("Error al enviar email de recordatorio - Usuario: {}, Email: {}, Turno ID: {}, Fecha: {}, Hora: {} - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), appointment.getId(), appointment.getAppointmentDate(), 
                appointment.getStartTime(), e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_REMINDER, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, appointment.getId(), e);
            }
        }
    }

    /**
     * NUEVO: Envía email notificando que un día fue cerrado pero el turno del usuario sigue siendo válido.
     */
    @Override
    @Async("emailTaskExecutor")
    public void sendDayClosedNotificationEmail(User user, Appointment appointment, java.time.LocalDate closedDate, String reason) {
        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "DAY_CLOSED_WITH_APPOINTMENT")) {
            logger.debug("Email de día cerrado no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = buildDayClosedNotificationEmailTemplate(user, appointment, closedDate, reason);
            sendEmail(template);
            logger.info("Email de día cerrado enviado exitosamente - Usuario: {}, Turno ID: {}, Fecha cerrada: {}", 
                user.getEmail(), appointment.getId(), closedDate);
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la creación de configuración
            logger.error("Error al enviar email de día cerrado - Usuario: {}, Email: {}, Turno ID: {}, Fecha: {} - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), appointment.getId(), closedDate, 
                e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_CANCELLED_BY_ADMIN, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, appointment.getId(), e);
            }
        }
    }

    /**
     * NUEVO: Envía un solo email notificando que uno o más días fueron cerrados pero los turnos del usuario siguen siendo válidos.
     * Agrupa múltiples turnos afectados en un solo email para evitar spam.
     */
    @Override
    @Async("emailTaskExecutor")
    public void sendDaysClosedNotificationEmail(User user, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> affectedAppointments) {
        if (affectedAppointments == null || affectedAppointments.isEmpty()) {
            logger.warn("Lista de turnos afectados vacía para usuario {}", user.getId());
            return;
        }

        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "DAY_CLOSED_WITH_APPOINTMENT")) {
            logger.debug("Email de días cerrados no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = buildDaysClosedNotificationEmailTemplate(user, affectedAppointments);
            sendEmail(template);
            logger.info("Email de días cerrados enviado exitosamente - Usuario: {}, {} turno(s) afectado(s)", 
                user.getEmail(), affectedAppointments.size());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la creación de configuración
            logger.error("Error al enviar email de días cerrados - Usuario: {}, Email: {}, {} turno(s) - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), affectedAppointments.size(), 
                e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_CANCELLED_BY_ADMIN, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, affectedAppointments.get(0).getAppointmentId(), e);
            }
        }
    }

    /**
     * NUEVO: Construye el template de email para notificación de día cerrado (método deprecado - usar versión con múltiples turnos).
     */
    @Deprecated
    private EmailTemplate buildDayClosedNotificationEmailTemplate(User user, Appointment appointment, 
                                                                  java.time.LocalDate closedDate, String reason) {
        String subject = String.format("Aviso: Día Cerrado - Tu Turno del %s", closedDate);
        String body = String.format(
            "<html><body>" +
            "<h2>Estimado/a %s %s,</h2>" +
            "<p>Te informamos que el día <strong>%s</strong> ha sido cerrado, " +
            "pero tu turno a las <strong>%s</strong> sigue siendo válido y será atendido.</p>" +
            "<p>Si necesitas reprogramar tu turno, por favor contacta con nosotros.</p>" +
            "<p>Gracias por tu comprensión.</p>" +
            "</body></html>",
            user.getFirstName(),
            user.getLastName(),
            closedDate,
            appointment.getStartTime()
        );
        
        // Crear implementación anónima de EmailTemplate
        return new EmailTemplate() {
            @Override
            public String getSubject() {
                return subject;
            }

            @Override
            public String getBody() {
                return body;
            }

            @Override
            public String getTo() {
                return user.getEmail();
            }

            @Override
            public String getFrom() {
                return "noreply@turnmanagement.com"; // Usar configuración por defecto
            }
        };
    }

    /**
     * NUEVO: Construye el template de email para notificación de múltiples días cerrados con turnos afectados.
     */
    private EmailTemplate buildDaysClosedNotificationEmailTemplate(User user, 
            java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> affectedAppointments) {
        
        // Agrupar turnos por fecha
        java.util.Map<java.time.LocalDate, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo>> appointmentsByDate = 
            affectedAppointments.stream()
                .collect(java.util.stream.Collectors.groupingBy(com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo::getDate));
        
        int uniqueDaysCount = appointmentsByDate.size();
        int totalAppointmentsCount = affectedAppointments.size();
        
        // Construir asunto
        String subject;
        if (uniqueDaysCount == 1) {
            subject = String.format("Aviso: Día Cerrado - %d Turno%s Afectado%s", 
                totalAppointmentsCount, 
                totalAppointmentsCount == 1 ? "" : "s",
                totalAppointmentsCount == 1 ? "" : "s");
        } else {
            subject = String.format("Aviso: %d Días Cerrados - %d Turno%s Afectado%s", 
                uniqueDaysCount, 
                totalAppointmentsCount,
                totalAppointmentsCount == 1 ? "" : "s",
                totalAppointmentsCount == 1 ? "" : "s");
        }
        
        // Construir cuerpo del email con estilos similares a los emails de verificación/registro
        String name = user.getFirstName() != null && user.getLastName() != null ? 
            user.getFirstName() + " " + user.getLastName() : 
            (user.getFirstName() != null ? user.getFirstName() : user.getEmail());
        
        java.lang.StringBuilder bodyBuilder = new java.lang.StringBuilder();
        bodyBuilder.append("""
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="UTF-8">
                <style>
                    body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
                    .header { background-color: #FF9800; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .appointment-list { list-style-type: none; padding-left: 0; margin: 20px 0; }
                    .appointment-item { margin-bottom: 15px; padding: 15px; background-color: #ffffff; border-left: 4px solid #FF9800; border-radius: 4px; }
                    .appointment-date { font-weight: bold; color: #333; margin-bottom: 8px; }
                    .appointment-times { list-style-type: none; padding-left: 0; margin-top: 8px; }
                    .appointment-time { padding: 5px 0; color: #666; }
                    .appointment-time strong { color: #FF9800; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                    .important { background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 20px 0; border-radius: 4px; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Aviso de Días Cerrados</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Te informamos que se ha actualizado la configuración del calendario y algunos días han sido cerrados. 
                        Sin embargo, <strong>todos tus turnos siguen siendo válidos y serán atendidos normalmente</strong>.</p>
            """.formatted(name));
        
        if (uniqueDaysCount == 1) {
            bodyBuilder.append(String.format("""
                        <p>El día <strong>%s</strong> ha sido cerrado, pero tienes %d turno(s) programado(s) que seguirán vigentes:</p>
                """, formatDate(affectedAppointments.get(0).getDate()), totalAppointmentsCount));
        } else {
            bodyBuilder.append(String.format("""
                        <p>Se han cerrado %d días, pero tienes %d turno(s) programado(s) que seguirán vigentes:</p>
                """, uniqueDaysCount, totalAppointmentsCount));
        }
        
        // Lista de turnos agrupados por fecha
        bodyBuilder.append("<ul class=\"appointment-list\">");
        appointmentsByDate.entrySet().stream()
            .sorted(java.util.Map.Entry.comparingByKey())
            .forEach(entry -> {
                java.time.LocalDate date = entry.getKey();
                java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> appointments = entry.getValue();
                
                bodyBuilder.append("<li class=\"appointment-item\">");
                bodyBuilder.append(String.format("<div class=\"appointment-date\">%s</div>", formatDate(date)));
                bodyBuilder.append("<ul class=\"appointment-times\">");
                appointments.stream()
                    .sorted((a, b) -> a.getStartTime().compareTo(b.getStartTime()))
                    .forEach(appt -> {
                        bodyBuilder.append(String.format("<li class=\"appointment-time\">Turno a las <strong>%s - %s</strong></li>", 
                            appt.getStartTime(), appt.getEndTime()));
                    });
                bodyBuilder.append("</ul>");
                bodyBuilder.append("</li>");
            });
        bodyBuilder.append("</ul>");
        
        bodyBuilder.append("""
                        <div class="important">
                            <p><strong>Importante:</strong> Si necesitas reprogramar alguno de estos turnos, por favor contacta con nosotros.</p>
                        </div>
                        <p>Gracias por tu comprensión.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """);
        
        String body = bodyBuilder.toString();
        
        // Crear implementación anónima de EmailTemplate
        return new EmailTemplate() {
            @Override
            public String getSubject() {
                return subject;
            }

            @Override
            public String getBody() {
                return body;
            }

            @Override
            public String getTo() {
                return user.getEmail();
            }

            @Override
            public String getFrom() {
                return "noreply@turnmanagement.com"; // Usar configuración por defecto
            }
        };
    }

    @Override
    @Async("emailTaskExecutor")
    public void sendMassCancellationEmail(User user, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> cancelledAppointments, String reason) {
        if (cancelledAppointments == null || cancelledAppointments.isEmpty()) {
            logger.warn("Lista de turnos cancelados vacía para usuario {}", user.getId());
            return;
        }

        // Verificar preferencias de notificación antes de enviar
        if (!preferenceService.shouldSendNotification(user.getId(), "APPOINTMENT_CANCELLED_BY_ADMIN")) {
            logger.debug("Email de cancelación masiva no enviado - Usuario {} tiene preferencia desactivada", user.getEmail());
            return;
        }

        EmailTemplate template = null;
        try {
            template = buildMassCancellationEmailTemplate(user, cancelledAppointments, reason);
            sendEmail(template);
            logger.info("Email de cancelación masiva enviado exitosamente - Usuario: {}, {} turno(s) cancelado(s)", 
                user.getEmail(), cancelledAppointments.size());
        } catch (Exception e) {
            // NO lanzar excepción - el email no debe bloquear la cancelación de turnos
            logger.error("Error al enviar email de cancelación masiva - Usuario: {}, Email: {}, {} turno(s) - Error type: {} - Message: {}", 
                user.getEmail(), user.getEmail(), cancelledAppointments.size(), 
                e.getClass().getSimpleName(), e.getMessage(), e);
            // Registrar email fallido
            if (template != null && !cancelledAppointments.isEmpty()) {
                registerFailedEmail(EmailNotificationType.APPOINTMENT_CANCELLED_BY_ADMIN, template, 
                    user.getId(), RelatedEntityType.APPOINTMENT, cancelledAppointments.get(0).getAppointmentId(), e);
            }
        }
    }

    /**
     * Construye el template de email para cancelación masiva de turnos por cierre de días.
     */
    private EmailTemplate buildMassCancellationEmailTemplate(User user, 
            java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> cancelledAppointments,
            String reason) {
        
        // Agrupar turnos por fecha
        java.util.Map<java.time.LocalDate, java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo>> appointmentsByDate = 
            cancelledAppointments.stream()
                .collect(java.util.stream.Collectors.groupingBy(com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo::getDate));
        
        int uniqueDaysCount = appointmentsByDate.size();
        int totalAppointmentsCount = cancelledAppointments.size();
        
        // Construir asunto
        String subject;
        if (uniqueDaysCount == 1) {
            subject = String.format("Turno%s Cancelado%s - Por favor, vuelva a solicitar", 
                totalAppointmentsCount == 1 ? "" : "s",
                totalAppointmentsCount == 1 ? "" : "s");
        } else {
            subject = String.format("%d Turno%s Cancelado%s - Por favor, vuelva a solicitar", 
                totalAppointmentsCount,
                totalAppointmentsCount == 1 ? "" : "s",
                totalAppointmentsCount == 1 ? "" : "s");
        }
        
        // Construir cuerpo del email
        String name = user.getFirstName() != null && user.getLastName() != null ? 
            user.getFirstName() + " " + user.getLastName() : 
            (user.getFirstName() != null ? user.getFirstName() : user.getEmail());
        
        java.lang.StringBuilder bodyBuilder = new java.lang.StringBuilder();
        bodyBuilder.append("""
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="UTF-8">
                <style>
                    body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
                    .header { background-color: #DC3545; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .appointment-list { list-style-type: none; padding-left: 0; margin: 20px 0; }
                    .appointment-item { margin-bottom: 15px; padding: 15px; background-color: #ffffff; border-left: 4px solid #DC3545; border-radius: 4px; }
                    .appointment-date { font-weight: bold; color: #333; margin-bottom: 8px; }
                    .appointment-times { list-style-type: none; padding-left: 0; margin-top: 8px; }
                    .appointment-time { padding: 5px 0; color: #666; }
                    .appointment-time strong { color: #DC3545; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                    .important { background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 20px 0; border-radius: 4px; }
                    .reason-box { background-color: #f8d7da; padding: 15px; border-left: 4px solid #DC3545; margin: 20px 0; border-radius: 4px; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Turnos Cancelados</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Lamentamos informarte que se ha actualizado la configuración del calendario y algunos de tus turnos han sido cancelados.</p>
            """.formatted(name));
        
        if (uniqueDaysCount == 1) {
            bodyBuilder.append(String.format("""
                        <p>Se ha cancelado %d turno(s) programado(s) para el día <strong>%s</strong>:</p>
                """, totalAppointmentsCount, formatDate(cancelledAppointments.get(0).getDate())));
        } else {
            bodyBuilder.append(String.format("""
                        <p>Se han cancelado %d turno(s) programado(s) en %d día(s):</p>
                """, totalAppointmentsCount, uniqueDaysCount));
        }
        
        // Lista de turnos cancelados agrupados por fecha
        bodyBuilder.append("<ul class=\"appointment-list\">");
        appointmentsByDate.entrySet().stream()
            .sorted(java.util.Map.Entry.comparingByKey())
            .forEach(entry -> {
                java.time.LocalDate date = entry.getKey();
                java.util.List<com.ak4n1.turn_management.feature.appointment.dto.response.UserAffectedAppointmentInfo> appointments = entry.getValue();
                
                bodyBuilder.append("<li class=\"appointment-item\">");
                bodyBuilder.append(String.format("<div class=\"appointment-date\">%s</div>", formatDate(date)));
                bodyBuilder.append("<ul class=\"appointment-times\">");
                appointments.stream()
                    .sorted((a, b) -> a.getStartTime().compareTo(b.getStartTime()))
                    .forEach(appt -> {
                        bodyBuilder.append(String.format("<li class=\"appointment-time\">Turno cancelado: <strong>%s - %s</strong></li>", 
                            appt.getStartTime(), appt.getEndTime()));
                    });
                bodyBuilder.append("</ul>");
                bodyBuilder.append("</li>");
            });
        bodyBuilder.append("</ul>");
        
        // Razón de la cancelación
        bodyBuilder.append(String.format("""
                        <div class="reason-box">
                            <p><strong>Razón de la cancelación:</strong></p>
                            <p>%s</p>
                        </div>
                """, reason != null ? reason : "Día cerrado según nueva configuración"));
        
        bodyBuilder.append("""
                        <div class="important">
                            <p><strong>Importante:</strong> Por favor, vuelva a solicitar un turno en un día hábil a través de nuestro sistema.</p>
                        </div>
                        <p>Disculpamos las molestias y agradecemos tu comprensión.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """);
        
        String body = bodyBuilder.toString();
        
        // Crear implementación anónima de EmailTemplate
        return new EmailTemplate() {
            @Override
            public String getSubject() {
                return subject;
            }

            @Override
            public String getBody() {
                return body;
            }

            @Override
            public String getTo() {
                return user.getEmail();
            }

            @Override
            public String getFrom() {
                return "noreply@turnmanagement.com"; // Usar configuración por defecto
            }
        };
    }

    /**
     * Formatea una fecha en formato legible en español.
     */
    private String formatDate(java.time.LocalDate date) {
        java.time.format.DateTimeFormatter formatter = java.time.format.DateTimeFormatter.ofPattern("EEEE, d 'de' MMMM 'de' yyyy", 
            java.util.Locale.forLanguageTag("es-ES"));
        String formatted = date.format(formatter);
        // Capitalizar primera letra
        return formatted.substring(0, 1).toUpperCase() + formatted.substring(1);
    }

    /**
     * Registra un email fallido en la base de datos para que pueda ser reenviado manualmente.
     * 
     * @param notificationType Tipo de notificación
     * @param template Plantilla del email que falló
     * @param recipientUserId ID del usuario destinatario
     * @param relatedEntityType Tipo de entidad relacionada
     * @param relatedEntityId ID de la entidad relacionada
     * @param exception Excepción que causó el fallo
     */
    @Transactional
    public void registerFailedEmail(EmailNotificationType notificationType, EmailTemplate template,
                                   Long recipientUserId, RelatedEntityType relatedEntityType,
                                   Long relatedEntityId, Exception exception) {
        try {
            FailedEmailNotification failedEmail = new FailedEmailNotification(
                notificationType,
                template.getTo(),
                recipientUserId,
                template.getSubject(),
                template.getBody(),
                relatedEntityType,
                relatedEntityId,
                exception.getMessage(),
                exception.getClass().getSimpleName()
            );
            failedEmailRepository.save(failedEmail);
            logger.info("Email fallido registrado - Tipo: {}, Destinatario: {}, ID: {}", 
                notificationType, template.getTo(), failedEmail.getId());
        } catch (Exception e) {
            // No lanzar excepción - el registro de fallo no debe afectar el flujo principal
            logger.error("Error al registrar email fallido - Tipo: {}, Destinatario: {} - Error: {}", 
                notificationType, template.getTo(), e.getMessage(), e);
        }
    }
}

