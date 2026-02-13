package com.ak4n1.turn_management.feature.notification.controller;

import com.ak4n1.turn_management.feature.notification.dto.request.SendManualNotificationRequest;
import com.ak4n1.turn_management.feature.notification.dto.response.SendManualNotificationResponse;
import com.ak4n1.turn_management.feature.notification.service.SystemNotificationService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para envío manual de notificaciones por administradores.
 * 
 * Implementa US-N011.
 */
@RestController
@RequestMapping("/api/admin/notifications")
@PreAuthorize("hasRole('ADMIN')")
public class AdminManualNotificationController {

    private static final Logger logger = LoggerFactory.getLogger(AdminManualNotificationController.class);

    private final SystemNotificationService notificationService;
    private final JwtTokenProvider jwtTokenProvider;

    public AdminManualNotificationController(SystemNotificationService notificationService,
                                             JwtTokenProvider jwtTokenProvider) {
        this.notificationService = notificationService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * Envía una notificación manual a todos los usuarios o a un usuario específico.
     * 
     * POST /api/admin/notifications/send
     * 
     * Implementa US-N011.
     */
    @PostMapping("/send")
    public ResponseEntity<SendManualNotificationResponse> sendManualNotification(
            @Valid @RequestBody SendManualNotificationRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud de envío manual de notificación - Tipo: {}, Destinatario: {}, Tipo de notificación: {}",
            request.getRecipientType(), request.getRecipientEmail(), request.getType());

        Long adminId = getCurrentUserId(httpRequest);

        // Validar que si es SPECIFIC_USER, el email esté presente
        if (request.getRecipientType() == SendManualNotificationRequest.RecipientType.SPECIFIC_USER) {
            if (request.getRecipientEmail() == null || request.getRecipientEmail().isBlank()) {
                throw new ApiException(
                    "El email del destinatario es requerido cuando recipientType es SPECIFIC_USER",
                    HttpStatus.BAD_REQUEST);
            }
        }

        try {
            SendManualNotificationResponse response = notificationService.sendManualNotification(
                request.getRecipientType(),
                request.getRecipientEmail(),
                request.getType(),
                request.getTitle(),
                request.getMessage(),
                adminId,
                request.getExcludedEmails()
            );

            logger.info("Notificación manual enviada exitosamente por admin {} - Total destinatarios: {}, Enviadas inmediatamente: {}",
                adminId, response.getTotalRecipients(), response.getSentImmediately());

            return ResponseEntity.ok(response);

        } catch (RuntimeException e) {
            logger.error("Error al enviar notificación manual: {}", e.getMessage(), e);
            throw new ApiException(e.getMessage(), HttpStatus.BAD_REQUEST);
        } catch (Exception e) {
            logger.error("Error inesperado al enviar notificación manual: {}", e.getMessage(), e);
            throw new ApiException("Error al enviar notificación: " + e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Obtiene el ID del usuario autenticado desde el token JWT.
     */
    private Long getCurrentUserId(HttpServletRequest request) {
        String token = extractTokenFromRequest(request);
        if (token != null) {
            try {
                Long userId = jwtTokenProvider.extractUserId(token);
                if (userId != null) {
                    return userId;
                }
            } catch (Exception e) {
                logger.warn("No se pudo extraer userId del token: {}", e.getMessage());
            }
        }

        throw new ApiException(
            "No se pudo obtener el ID del usuario autenticado",
            HttpStatus.INTERNAL_SERVER_ERROR);
    }

    /**
     * Extrae el token JWT desde las cookies o el header Authorization.
     */
    private String extractTokenFromRequest(HttpServletRequest request) {
        // Intentar obtener desde cookies (nombre: accessToken)
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        // Intentar obtener desde header Authorization: Bearer <token>
        String authHeader = request.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }

        return null;
    }
}

