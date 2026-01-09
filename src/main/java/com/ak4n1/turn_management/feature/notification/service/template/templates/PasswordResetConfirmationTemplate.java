package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class PasswordResetConfirmationTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    private User user;

    public PasswordResetConfirmationTemplate() {
    }

    public PasswordResetConfirmationTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    @Override
    public String getSubject() {
        return "Contraseña restablecida exitosamente";
    }

    @Override
    public String getBody() {
        String name = user.getFirstName() != null ? user.getFirstName() : user.getEmail();
        
        return String.format("""
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="UTF-8">
                <style>
                    body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
                    .header { background-color: #4CAF50; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                    .warning { background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 12px; margin: 20px 0; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Contraseña Restablecida</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Tu contraseña ha sido restablecida exitosamente.</p>
                        <div class="warning">
                            <strong>⚠️ Seguridad:</strong> Si no realizaste este cambio, por favor contacta inmediatamente con nuestro equipo de soporte.
                        </div>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name);
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

