package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class AccountLockedTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    private User user;
    private int lockoutDurationMinutes;

    public AccountLockedTemplate() {
    }

    public AccountLockedTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    public AccountLockedTemplate setLockoutDuration(int minutes) {
        this.lockoutDurationMinutes = minutes;
        return this;
    }

    @Override
    public String getSubject() {
        return "Cuenta temporalmente bloqueada";
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
                    .header { background-color: #f44336; color: white; padding: 20px; text-align: center; }
                    .content { padding: 20px; background-color: #f9f9f9; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                    .warning { background-color: #ffebee; border-left: 4px solid #f44336; padding: 12px; margin: 20px 0; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Cuenta Bloqueada</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Tu cuenta ha sido temporalmente bloqueada debido a múltiples intentos de inicio de sesión fallidos.</p>
                        <div class="warning">
                            <strong>⚠️ Bloqueo temporal:</strong> Tu cuenta estará bloqueada por %d minutos. Después de este tiempo, podrás intentar iniciar sesión nuevamente.
                        </div>
                        <p>Si no intentaste acceder a tu cuenta, por favor contacta inmediatamente con nuestro equipo de soporte.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name, lockoutDurationMinutes);
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

