package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Component
public class AppointmentReminderTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    private User user;
    private Appointment appointment;

    public AppointmentReminderTemplate() {
    }

    public AppointmentReminderTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    public AppointmentReminderTemplate setAppointment(Appointment appointment) {
        this.appointment = appointment;
        return this;
    }

    @Override
    public String getSubject() {
        return "Recordatorio: Tu turno es mañana - " + appointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("dd/MM/yyyy")) + " " + appointment.getStartTime();
    }

    @Override
    public String getBody() {
        String name = user.getFirstName() != null ? user.getFirstName() : user.getEmail();
        String startTime = appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        String endTime = appointment.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        String duration = appointment.getDurationMinutes() + " minutos";
        
        // Formatear fecha completa para el mensaje
        String fullDate = appointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("EEEE, dd 'de' MMMM 'de' yyyy", java.util.Locale.forLanguageTag("es-AR")));
        
        return String.format("""
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="UTF-8">
                <style>
                    body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
                    .header { background-color: #FF9800; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .appointment-info { background-color: white; padding: 15px; margin: 15px 0; border-left: 4px solid #FF9800; }
                    .info-row { margin: 10px 0; }
                    .info-label { font-weight: bold; color: #555; }
                    .info-value { color: #333; }
                    .reminder { background-color: #FFF3E0; border-left: 4px solid #FF9800; padding: 15px; margin: 15px 0; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>⏰ Recordatorio de Turno</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Este es un recordatorio de tu turno confirmado:</p>
                        
                        <div class="appointment-info">
                            <div class="info-row">
                                <span class="info-label">Fecha:</span>
                                <span class="info-value">%s</span>
                            </div>
                            <div class="info-row">
                                <span class="info-label">Horario:</span>
                                <span class="info-value">%s - %s</span>
                            </div>
                            <div class="info-row">
                                <span class="info-label">Duración:</span>
                                <span class="info-value">%s</span>
                            </div>
                            <div class="info-row">
                                <span class="info-label">Estado:</span>
                                <span class="info-value">Confirmado ✓</span>
                            </div>
                        </div>
                        
                        <div class="reminder">
                            <p><strong>⏰ No olvides tu turno</strong></p>
                            <p>Te recordamos que tienes un turno confirmado. Por favor, asegúrate de asistir en la fecha y hora indicadas.</p>
                            <p>Si necesitas cancelar o reprogramar tu turno, puedes hacerlo desde tu panel de usuario.</p>
                        </div>
                        
                        <p>Te esperamos. Si tienes alguna pregunta, no dudes en contactarnos.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name, fullDate, startTime, endTime, duration);
    }

    @Override
    public String getTo() {
        return user.getEmail();
    }

    @Override
    public String getFrom() {
        return fromName + " <" + fromEmail + ">";
    }
}

