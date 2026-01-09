package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class PasswordResetTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    @Value("${app.frontend-url:http://localhost:4200}")
    private String frontendUrl;

    private User user;
    private String resetToken;

    public PasswordResetTemplate() {
    }

    public PasswordResetTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    public PasswordResetTemplate setResetToken(String token) {
        this.resetToken = token;
        return this;
    }

    @Override
    public String getSubject() {
        return "Restablecer tu contraseña";
    }

    @Override
    public String getBody() {
        String name = user.getFirstName() != null ? user.getFirstName() : user.getEmail();
        // El frontend debe manejar este token como path parameter: /reset-password/{token}
        String resetUrl = frontendUrl + "/reset-password/" + resetToken;
        
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
                    .button { display: inline-block; padding: 12px 24px; background-color: #FF9800; color: white; text-decoration: none; border-radius: 5px; margin: 20px 0; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                    .warning { background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 12px; margin: 20px 0; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Restablecer Contraseña</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Recibimos una solicitud para restablecer la contraseña de tu cuenta.</p>
                        <p>Haz clic en el siguiente botón para cambiar tu contraseña:</p>
                        <p style="text-align: center;">
                            <a href="%s" class="button">Cambiar Contraseña</a>
                        </p>
                        <p>O copia y pega este enlace en tu navegador:</p>
                        <p style="word-break: break-all; color: #FF9800;">%s</p>
                        <div class="warning">
                            <strong>⚠️ Importante:</strong> Este enlace expirará en 1 hora. Si no solicitaste este cambio, ignora este email y tu contraseña permanecerá sin cambios.
                        </div>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name, resetUrl, resetUrl);
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

