package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Component
public class AppointmentCancelledByUserTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    private User user;
    private Appointment appointment;
    private String reason;

    public AppointmentCancelledByUserTemplate() {
    }

    public AppointmentCancelledByUserTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    public AppointmentCancelledByUserTemplate setAppointment(Appointment appointment) {
        this.appointment = appointment;
        return this;
    }

    public AppointmentCancelledByUserTemplate setReason(String reason) {
        this.reason = reason;
        return this;
    }

    @Override
    public String getSubject() {
        return "Turno Cancelado - " + appointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("dd/MM/yyyy")) + " " + appointment.getStartTime();
    }

    @Override
    public String getBody() {
        String name = user.getFirstName() != null ? user.getFirstName() : user.getEmail();
        String startTime = appointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        String endTime = appointment.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        String duration = appointment.getDurationMinutes() + " minutos";
        
        // Formatear fecha completa para el mensaje
        String fullDate = appointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("EEEE, dd 'de' MMMM 'de' yyyy", java.util.Locale.forLanguageTag("es-AR")));
        
        String reasonText = (reason != null && !reason.isBlank()) 
            ? reason 
            : "Sin motivo especificado";
        
        return String.format("""
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="UTF-8">
                <style>
                    body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
                    .header { background-color: #F44336; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .appointment-info { background-color: white; padding: 15px; margin: 15px 0; border-left: 4px solid #F44336; }
                    .info-row { margin: 10px 0; }
                    .info-label { font-weight: bold; color: #555; }
                    .info-value { color: #333; }
                    .reason-box { background-color: #FFEBEE; border-left: 4px solid #F44336; padding: 15px; margin: 15px 0; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Turno Cancelado</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Tu turno ha sido cancelado exitosamente. Aquí están los detalles:</p>
                        
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
                                <span class="info-value">Cancelado</span>
                            </div>
                        </div>
                        
                        <div class="reason-box">
                            <p><strong>Motivo de cancelación:</strong></p>
                            <p>%s</p>
                        </div>
                        
                        <p>Si necesitas agendar un nuevo turno, puedes hacerlo desde tu panel de usuario.</p>
                        <p>Si tienes alguna pregunta o necesitas asistencia, no dudes en contactarnos.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name, fullDate, startTime, endTime, duration, reasonText);
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

