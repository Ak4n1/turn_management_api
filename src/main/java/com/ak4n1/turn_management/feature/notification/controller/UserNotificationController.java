package com.ak4n1.turn_management.feature.notification.controller;

import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.SystemNotification;
import com.ak4n1.turn_management.feature.notification.dto.response.SystemNotificationResponse;
import com.ak4n1.turn_management.feature.notification.dto.response.SystemNotificationsResponse;
import com.ak4n1.turn_management.feature.notification.service.SystemNotificationService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Controller para gestión de notificaciones de usuarios.
 * 
 * Implementa US-N003, US-N004, US-N008.
 */
@RestController
@RequestMapping("/api/notifications")
@PreAuthorize("hasRole('USER') or hasRole('ADMIN')")
public class UserNotificationController {

    private static final Logger logger = LoggerFactory.getLogger(UserNotificationController.class);

    private final SystemNotificationService notificationService;
    private final JwtTokenProvider jwtTokenProvider;
    private final com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService webSocketNotificationService;

    public UserNotificationController(SystemNotificationService notificationService,
                                     JwtTokenProvider jwtTokenProvider,
                                     com.ak4n1.turn_management.feature.notification.service.WebSocketNotificationService webSocketNotificationService) {
        this.notificationService = notificationService;
        this.jwtTokenProvider = jwtTokenProvider;
        this.webSocketNotificationService = webSocketNotificationService;
    }

    /**
     * Obtiene todas las notificaciones del usuario autenticado con filtros y paginación.
     * 
     * GET /api/notifications?page=0&size=20&type=APPOINTMENT_CREATED&read=false&search=texto
     * 
     * Implementa US-N003.
     */
    @GetMapping
    public ResponseEntity<SystemNotificationsResponse> getNotifications(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) NotificationType type,
            @RequestParam(required = false) Boolean read,
            @RequestParam(required = false) String search,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud para obtener notificaciones - page: {}, size: {}, type: {}, read: {}, search: {}", 
            page, size, type, read, search);

        Long userId = getCurrentUserId(httpRequest);

        Page<SystemNotification> notificationPage = notificationService.getNotificationsWithFilters(
            userId, type, read, search, page, size);
        long unreadCount = notificationService.countUnreadNotifications(userId);

        List<SystemNotificationResponse> notificationResponses = notificationPage.getContent().stream()
            .map(notificationService::mapToResponse)
            .collect(Collectors.toList());

        SystemNotificationsResponse response = new SystemNotificationsResponse(
            notificationResponses,
            notificationPage.getTotalElements(),
            notificationPage.getTotalPages(),
            notificationPage.getNumber(),
            notificationPage.getSize(),
            unreadCount
        );

        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el contador de notificaciones no leídas.
     * 
     * GET /api/notifications/unread-count
     * 
     * Implementa US-N003.
     */
    @GetMapping("/unread-count")
    public ResponseEntity<Long> getUnreadCount(HttpServletRequest httpRequest) {
        Long userId = getCurrentUserId(httpRequest);
        long unreadCount = notificationService.countUnreadNotifications(userId);
        return ResponseEntity.ok(unreadCount);
    }

    /**
     * Marca una notificación como leída.
     * 
     * POST /api/notifications/{id}/read
     * 
     * Implementa US-N004.
     */
    @PostMapping("/{id}/read")
    public ResponseEntity<SystemNotificationResponse> markAsRead(
            @PathVariable Long id,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud para marcar notificación como leída - ID: {}", id);

        Long userId = getCurrentUserId(httpRequest);

        try {
            SystemNotification notification = notificationService.markAsRead(id, userId);
            SystemNotificationResponse response = notificationService.mapToResponse(notification);
            
            // Enviar actualización vía WebSocket (contador actualizado)
            try {
                webSocketNotificationService.sendUnreadCountUpdate(userId);
            } catch (Exception e) {
                // No lanzar excepción - la notificación WebSocket no debe bloquear la operación
                logger.error("Error al enviar actualización de contador WebSocket - Usuario: {}. Error: {}",
                    userId, e.getMessage(), e);
            }
            
            return ResponseEntity.ok(response);
        } catch (RuntimeException e) {
            throw new ApiException(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Marca todas las notificaciones como leídas.
     * 
     * POST /api/notifications/read-all
     * 
     * Implementa US-N004.
     */
    @PostMapping("/read-all")
    public ResponseEntity<Map<String, Object>> markAllAsRead(HttpServletRequest httpRequest) {
        logger.info("Solicitud para marcar todas las notificaciones como leídas");

        Long userId = getCurrentUserId(httpRequest);

        int markedCount = notificationService.markAllAsRead(userId);
        
        Map<String, Object> response = new HashMap<>();
        response.put("markedCount", markedCount);
        response.put("message", "Se marcaron " + markedCount + " notificaciones como leídas");
        
        // Enviar actualización vía WebSocket
        try {
            webSocketNotificationService.sendUnreadCountUpdate(userId);
        } catch (Exception e) {
            // No lanzar excepción - la notificación WebSocket no debe bloquear la operación
            logger.error("Error al enviar actualización de contador WebSocket - Usuario: {}. Error: {}",
                userId, e.getMessage(), e);
        }
        
        return ResponseEntity.ok(response);
    }

    /**
     * Elimina una notificación.
     * 
     * DELETE /api/notifications/{id}
     * 
     * Implementa US-N008.
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Map<String, Object>> deleteNotification(
            @PathVariable Long id,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud para eliminar notificación - ID: {}", id);

        Long userId = getCurrentUserId(httpRequest);

        try {
            notificationService.deleteNotification(id, userId);
            
            Map<String, Object> response = new HashMap<>();
            response.put("message", "Notificación eliminada exitosamente");
            response.put("id", id);
            
            // Enviar actualización vía WebSocket (contador actualizado)
            try {
                webSocketNotificationService.sendUnreadCountUpdate(userId);
            } catch (Exception e) {
                // No lanzar excepción - la notificación WebSocket no debe bloquear la operación
                logger.error("Error al enviar actualización de contador WebSocket - Usuario: {}. Error: {}",
                    userId, e.getMessage(), e);
            }
            
            return ResponseEntity.ok(response);
        } catch (RuntimeException e) {
            throw new ApiException(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Elimina múltiples notificaciones.
     * 
     * DELETE /api/notifications/batch
     * 
     * Implementa US-N008.
     */
    @DeleteMapping("/batch")
    public ResponseEntity<Map<String, Object>> deleteNotifications(
            @RequestBody List<Long> notificationIds,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud para eliminar {} notificaciones", notificationIds.size());

        Long userId = getCurrentUserId(httpRequest);

        if (notificationIds == null || notificationIds.isEmpty()) {
            throw new ApiException("La lista de IDs no puede estar vacía", HttpStatus.BAD_REQUEST);
        }

        if (notificationIds.size() > 50) {
            throw new ApiException("No se pueden eliminar más de 50 notificaciones a la vez", HttpStatus.BAD_REQUEST);
        }

        try {
            int deletedCount = notificationService.deleteNotifications(notificationIds, userId);
            
            Map<String, Object> response = new HashMap<>();
            response.put("deletedCount", deletedCount);
            response.put("message", "Se eliminaron " + deletedCount + " notificaciones");
            
            // Enviar actualización vía WebSocket
            try {
                webSocketNotificationService.sendUnreadCountUpdate(userId);
            } catch (Exception e) {
                // No lanzar excepción - la notificación WebSocket no debe bloquear la operación
                logger.error("Error al enviar actualización de contador WebSocket - Usuario: {}. Error: {}",
                    userId, e.getMessage(), e);
            }
            
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

