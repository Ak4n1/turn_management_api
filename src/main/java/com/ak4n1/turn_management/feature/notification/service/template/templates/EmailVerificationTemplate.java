package com.ak4n1.turn_management.feature.notification.service.template.templates;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class EmailVerificationTemplate implements EmailTemplate {

    @Value("${mail.from}")
    private String fromEmail;

    @Value("${mail.from.name:Turn Management System}")
    private String fromName;

    @Value("${app.frontend-url:http://localhost:4200}")
    private String frontendUrl;

    private User user;
    private String verificationToken;

    public EmailVerificationTemplate() {
    }

    public EmailVerificationTemplate setUser(User user) {
        this.user = user;
        return this;
    }

    public EmailVerificationTemplate setVerificationToken(String token) {
        this.verificationToken = token;
        return this;
    }

    @Override
    public String getSubject() {
        return "Verifica tu dirección de email";
    }

    @Override
    public String getBody() {
        String name = user.getFirstName() != null ? user.getFirstName() : user.getEmail();
        String verificationUrl = frontendUrl + "/verify-email?token=" + verificationToken;
        
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
                    .button { display: inline-block; padding: 12px 24px; background-color: #2196F3; color: white; text-decoration: none; border-radius: 5px; margin: 20px 0; }
                    .footer { text-align: center; padding: 20px; font-size: 12px; color: #666; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>Verifica tu Email</h1>
                    </div>
                    <div class="content">
                        <p>Hola %s,</p>
                        <p>Por favor, verifica tu dirección de email haciendo clic en el siguiente botón:</p>
                        <p style="text-align: center;">
                            <a href="%s" class="button">Verificar Email</a>
                        </p>
                        <p>O copia y pega este enlace en tu navegador:</p>
                        <p style="word-break: break-all; color: #2196F3;">%s</p>
                        <p>Este enlace expirará en 24 horas.</p>
                        <p>Si no creaste esta cuenta, puedes ignorar este email.</p>
                        <p>Saludos,<br>El equipo de Turn Management System</p>
                    </div>
                    <div class="footer">
                        <p>Este es un email automático, por favor no respondas.</p>
                    </div>
                </div>
            </body>
            </html>
            """, name, verificationUrl, verificationUrl);
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

