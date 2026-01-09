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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Controller para gestión de notificaciones del sistema por administradores.
 * 
 * Implementa US-N006.
 */
@RestController
@RequestMapping("/api/admin/notifications")
@PreAuthorize("hasRole('ADMIN')")
public class AdminNotificationController {

    private static final Logger logger = LoggerFactory.getLogger(AdminNotificationController.class);

    private final SystemNotificationService notificationService;
    private final JwtTokenProvider jwtTokenProvider;

    public AdminNotificationController(SystemNotificationService notificationService,
                                       JwtTokenProvider jwtTokenProvider) {
        this.notificationService = notificationService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * Obtiene todas las notificaciones del sistema con filtros avanzados.
     * 
     * GET /api/admin/notifications?page=0&size=50&userId=456&type=APPOINTMENT_CONFIRMED&read=false&dateFrom=2024-01-01&dateTo=2024-12-31&search=turno
     * 
     * Implementa US-N006.
     */
    @GetMapping
    public ResponseEntity<SystemNotificationsResponse> getNotifications(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size,
            @RequestParam(required = false) Long userId,
            @RequestParam(required = false) String type,
            @RequestParam(required = false) Boolean read,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String search,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud admin para obtener notificaciones - page: {}, size: {}, userId: {}, type: {}, read: {}, dateFrom: {}, dateTo: {}, search: {}", 
            page, size, userId, type, read, dateFrom, dateTo, search);

        getCurrentUserId(httpRequest); // Validar que el usuario esté autenticado

        NotificationType notificationType = null;
        if (type != null && !type.isBlank()) {
            try {
                notificationType = NotificationType.valueOf(type.toUpperCase());
            } catch (IllegalArgumentException e) {
                throw new ApiException("Tipo de notificación inválido: " + type, HttpStatus.BAD_REQUEST);
            }
        }

        LocalDateTime dateFromParsed = null;
        if (dateFrom != null && !dateFrom.isBlank()) {
            try {
                dateFromParsed = LocalDate.parse(dateFrom).atStartOfDay();
            } catch (Exception e) {
                throw new ApiException("Formato de fecha inválido para dateFrom: " + dateFrom + ". Use formato YYYY-MM-DD", HttpStatus.BAD_REQUEST);
            }
        }

        LocalDateTime dateToParsed = null;
        if (dateTo != null && !dateTo.isBlank()) {
            try {
                dateToParsed = LocalDate.parse(dateTo).atTime(23, 59, 59);
            } catch (Exception e) {
                throw new ApiException("Formato de fecha inválido para dateTo: " + dateTo + ". Use formato YYYY-MM-DD", HttpStatus.BAD_REQUEST);
            }
        }

        Page<SystemNotification> notificationPage = notificationService.getAdminNotifications(
            userId, notificationType, read, dateFromParsed, dateToParsed, search, page, size);

        List<SystemNotificationResponse> notificationResponses = notificationPage.getContent().stream()
            .map(notificationService::mapToResponse)
            .collect(Collectors.toList());

        SystemNotificationsResponse response = new SystemNotificationsResponse(
            notificationResponses,
            notificationPage.getTotalElements(),
            notificationPage.getTotalPages(),
            notificationPage.getNumber(),
            notificationPage.getSize(),
            notificationService.countUnreadNotifications(null) // Total de no leídas del sistema
        );

        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene estadísticas de notificaciones del sistema.
     * 
     * GET /api/admin/notifications/stats
     * 
     * Implementa US-N006.
     */
    @GetMapping("/stats")
    public ResponseEntity<Map<String, Object>> getStats(HttpServletRequest httpRequest) {
        logger.info("Solicitud admin para obtener estadísticas de notificaciones");

        getCurrentUserId(httpRequest); // Validar que el usuario esté autenticado

        Map<String, Object> stats = notificationService.getAdminNotificationStats();
        return ResponseEntity.ok(stats);
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

