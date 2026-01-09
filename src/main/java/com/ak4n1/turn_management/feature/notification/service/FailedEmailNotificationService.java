package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.notification.domain.EmailNotificationType;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailNotification;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailStatus;
import com.ak4n1.turn_management.feature.notification.dto.response.FailedEmailNotificationResponse;
import com.ak4n1.turn_management.feature.notification.repository.FailedEmailNotificationRepository;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplate;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.springframework.http.HttpStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Servicio para gestión de emails fallidos.
 * Implementa US-T031.1.
 */
@Service
public class FailedEmailNotificationService {

    private static final Logger logger = LoggerFactory.getLogger(FailedEmailNotificationService.class);
    private static final int MAX_RETRY_COUNT = 5;

    private final FailedEmailNotificationRepository failedEmailRepository;
    private final EmailService emailService;

    public FailedEmailNotificationService(FailedEmailNotificationRepository failedEmailRepository,
                                         EmailService emailService) {
        this.failedEmailRepository = failedEmailRepository;
        this.emailService = emailService;
    }

    /**
     * Consulta emails fallidos con filtros opcionales.
     * 
     * @param status Estado opcional (null para todos)
     * @param notificationType Tipo de notificación opcional (null para todos)
     * @param pageable Paginación
     * @return Página de emails fallidos
     */
    public Page<FailedEmailNotificationResponse> getFailedEmails(FailedEmailStatus status,
                                                                 EmailNotificationType notificationType,
                                                                 Pageable pageable) {
        Page<FailedEmailNotification> failedEmails = failedEmailRepository.findByFilters(status, notificationType, pageable);
        return failedEmails.map(this::mapToResponse);
    }

    /**
     * Reenvía un email fallido.
     * 
     * @param failedEmailId ID del email fallido
     * @param adminId ID del administrador que realiza el reenvío
     * @return Email fallido actualizado
     * @throws ApiException si el email no existe, ya fue reenviado, o excedió el límite de reintentos
     */
    @Transactional
    public FailedEmailNotificationResponse retryFailedEmail(Long failedEmailId, Long adminId) {
        FailedEmailNotification failedEmail = failedEmailRepository.findById(failedEmailId)
            .orElseThrow(() -> new ApiException("Email fallido no encontrado", HttpStatus.NOT_FOUND));

        // Validar que el email esté en estado FAILED
        if (failedEmail.getStatus() != FailedEmailStatus.FAILED) {
            throw new ApiException("El email ya fue reenviado o descartado. Estado actual: " + failedEmail.getStatus(), 
                HttpStatus.BAD_REQUEST);
        }

        // Validar límite de reintentos
        if (failedEmail.getRetryCount() >= MAX_RETRY_COUNT) {
            throw new ApiException("Se excedió el límite de reintentos (" + MAX_RETRY_COUNT + "). El email no puede ser reenviado.", 
                HttpStatus.BAD_REQUEST);
        }

        // Incrementar contador de reintentos
        failedEmail.incrementRetryCount();
        failedEmail.setRetriedByAdminId(adminId);

        try {
            // Crear template desde los datos guardados
            EmailTemplate template = createTemplateFromFailedEmail(failedEmail);
            
            // Intentar reenviar
            emailService.sendEmail(template);
            
            // Marcar como reenviado exitosamente
            failedEmail.markAsResent();
            failedEmailRepository.save(failedEmail);
            
            logger.info("Email fallido reenviado exitosamente - ID: {}, Tipo: {}, Destinatario: {}, Admin ID: {}", 
                failedEmail.getId(), failedEmail.getNotificationType(), failedEmail.getRecipientEmail(), adminId);
            
            return mapToResponse(failedEmail);
            
        } catch (Exception e) {
            // Marcar como fallido nuevamente
            failedEmail.markAsFailedAgain(e.getMessage(), e.getClass().getSimpleName());
            failedEmailRepository.save(failedEmail);
            
            logger.error("Error al reenviar email fallido - ID: {}, Tipo: {}, Destinatario: {} - Error: {}", 
                failedEmail.getId(), failedEmail.getNotificationType(), failedEmail.getRecipientEmail(), 
                e.getMessage(), e);
            
            throw new ApiException("Error al reenviar email: " + e.getMessage(), 
                HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Crea un EmailTemplate desde un FailedEmailNotification.
     * 
     * @param failedEmail Email fallido
     * @return Template de email
     */
    private EmailTemplate createTemplateFromFailedEmail(FailedEmailNotification failedEmail) {
        return new EmailTemplate() {
            @Override
            public String getSubject() {
                return failedEmail.getSubject();
            }

            @Override
            public String getBody() {
                return failedEmail.getBody();
            }

            @Override
            public String getTo() {
                return failedEmail.getRecipientEmail();
            }

            @Override
            public String getFrom() {
                // Usar el mismo remitente que se usa normalmente
                return "noreply@turnmanagement.com";
            }
        };
    }

    /**
     * Mapea un FailedEmailNotification a FailedEmailNotificationResponse.
     * 
     * @param failedEmail Email fallido
     * @return DTO de respuesta
     */
    private FailedEmailNotificationResponse mapToResponse(FailedEmailNotification failedEmail) {
        return new FailedEmailNotificationResponse(
            failedEmail.getId(),
            failedEmail.getNotificationType(),
            failedEmail.getRecipientEmail(),
            failedEmail.getRecipientUserId(),
            failedEmail.getSubject(),
            failedEmail.getRelatedEntityType(),
            failedEmail.getRelatedEntityId(),
            failedEmail.getStatus(),
            failedEmail.getRetryCount(),
            failedEmail.getErrorMessage(),
            failedEmail.getErrorType(),
            failedEmail.getCreatedAt(),
            failedEmail.getUpdatedAt(),
            failedEmail.getLastRetryAt(),
            failedEmail.getRetriedByAdminId()
        );
    }
}

