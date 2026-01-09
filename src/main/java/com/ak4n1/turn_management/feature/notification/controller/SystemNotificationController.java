package com.ak4n1.turn_management.feature.notification.controller;

import com.ak4n1.turn_management.feature.notification.domain.SystemNotification;
import com.ak4n1.turn_management.feature.notification.dto.response.SystemNotificationResponse;
import com.ak4n1.turn_management.feature.notification.service.SystemNotificationService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Controller para gestión de notificaciones del sistema.
 * 
 * Implementa US-T025.5.
 * Endpoints protegidos con rol ADMIN.
 */
@RestController
@RequestMapping("/api/admin/notifications")
@PreAuthorize("hasRole('ADMIN')")
public class SystemNotificationController {

    private static final Logger logger = LoggerFactory.getLogger(SystemNotificationController.class);

    private final SystemNotificationService notificationService;
    private final JwtTokenProvider jwtTokenProvider;

    public SystemNotificationController(SystemNotificationService notificationService,
                                       JwtTokenProvider jwtTokenProvider) {
        this.notificationService = notificationService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * Obtiene todas las notificaciones no leídas del administrador autenticado.
     * 
     * GET /api/admin/notifications/unread
     * 
     * Implementa US-T025.5.
     */
    @GetMapping("/unread")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<List<SystemNotificationResponse>> getUnreadNotifications(
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud para obtener notificaciones no leídas");

        Long adminUserId = getCurrentUserId(httpRequest);

        List<SystemNotification> unreadNotifications = notificationService.getUnreadNotifications(adminUserId);
        List<SystemNotificationResponse> responses = unreadNotifications.stream()
            .map(notificationService::mapToResponse)
            .collect(Collectors.toList());

        return ResponseEntity.ok(responses);
    }

    /**
     * Marca una notificación como leída.
     * 
     * PUT /api/admin/notifications/{id}/read
     * 
     * Implementa US-T025.5.
     */
    @PutMapping("/{id}/read")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<SystemNotificationResponse> markAsRead(
            @PathVariable Long id,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud para marcar notificación como leída - ID: {}", id);

        Long adminUserId = getCurrentUserId(httpRequest);

        try {
            SystemNotification notification = notificationService.markAsRead(id, adminUserId);
            SystemNotificationResponse response = notificationService.mapToResponse(notification);
            return ResponseEntity.ok(response);
        } catch (RuntimeException e) {
            throw new ApiException(e.getMessage(), HttpStatus.BAD_REQUEST);
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

