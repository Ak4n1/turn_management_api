package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Component
public class AppointmentRescheduledTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    private User user;
    private Appointment originalAppointment;
    private Appointment newAppointment;
    private String reason;

    public AppointmentRescheduledTemplate() {
    }

    public AppointmentRescheduledTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    public AppointmentRescheduledTemplate setOriginalAppointment(Appointment originalAppointment) {
        this.originalAppointment = originalAppointment;
        return this;
    }

    public AppointmentRescheduledTemplate setNewAppointment(Appointment newAppointment) {
        this.newAppointment = newAppointment;
        return this;
    }

    public AppointmentRescheduledTemplate setReason(String reason) {
        this.reason = reason;
        return this;
    }

    @Override
    public String getSubject() {
        return "Turno Reprogramado - " + newAppointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("dd/MM/yyyy")) + " " + newAppointment.getStartTime();
    }

    @Override
    public String getBody() {
        String name = user.getFirstName() != null ? user.getFirstName() : user.getEmail();
        
        // Formatear información del turno original
        String originalDate = originalAppointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("EEEE, dd 'de' MMMM 'de' yyyy", java.util.Locale.forLanguageTag("es-AR")));
        String originalTime = originalAppointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")) + " - " + originalAppointment.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        
        // Formatear información del nuevo turno
        String newDate = newAppointment.getAppointmentDate().format(DateTimeFormatter.ofPattern("EEEE, dd 'de' MMMM 'de' yyyy", java.util.Locale.forLanguageTag("es-AR")));
        String newTime = newAppointment.getStartTime().format(DateTimeFormatter.ofPattern("HH:mm")) + " - " + newAppointment.getEndTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        String duration = newAppointment.getDurationMinutes() + " minutos";
        
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
                    .header { background-color: #2196F3; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .appointment-box { background-color: white; padding: 15px; margin: 15px 0; border-left: 4px solid; }
                    .original-box { border-color: #F44336; }
                    .new-box { border-color: #4CAF50; }
                    .info-row { margin: 10px 0; }
                    .info-label { font-weight: bold; color: #555; }
                    .info-value { color: #333; }
                    .arrow { text-align: center; font-size: 24px; color: #2196F3; margin: 10px 0; }
                    .reason-box { background-color: #E3F2FD; border-left: 4px solid #2196F3; padding: 15px; margin: 15px 0; }
                    .success { background-color: #E8F5E9; border-left: 4px solid #4CAF50; padding: 15px; margin: 15px 0; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Turno Reprogramado</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Tu turno ha sido reprogramado. Aquí están los detalles del cambio:</p>
                        
                        <div class="appointment-box original-box">
                            <p><strong>Turno Anterior (Cancelado):</strong></p>
                            <div class="info-row">
                                <span class="info-label">Fecha:</span>
                                <span class="info-value">%s</span>
                            </div>
                            <div class="info-row">
                                <span class="info-label">Horario:</span>
                                <span class="info-value">%s</span>
                            </div>
                        </div>
                        
                        <div class="arrow">↓</div>
                        
                        <div class="appointment-box new-box">
                            <p><strong>Nuevo Turno (Confirmado):</strong></p>
                            <div class="info-row">
                                <span class="info-label">Fecha:</span>
                                <span class="info-value">%s</span>
                            </div>
                            <div class="info-row">
                                <span class="info-label">Horario:</span>
                                <span class="info-value">%s</span>
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
                        
                        <div class="reason-box">
                            <p><strong>Motivo de reprogramación:</strong></p>
                            <p>%s</p>
                        </div>
                        
                        <div class="success">
                            <p><strong>✓ Tu nuevo turno está confirmado</strong></p>
                            <p>El turno anterior ha sido cancelado y el nuevo turno está confirmado. Te esperamos en la nueva fecha y hora indicadas.</p>
                        </div>
                        
                        <p>Si tienes alguna pregunta o necesitas asistencia, no dudes en contactarnos.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name, originalDate, originalTime, newDate, newTime, duration, reasonText);
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

