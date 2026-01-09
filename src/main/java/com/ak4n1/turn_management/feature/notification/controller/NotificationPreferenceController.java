package com.ak4n1.turn_management.feature.notification.controller;

import com.ak4n1.turn_management.feature.notification.dto.request.UpdateNotificationPreferenceRequest;
import com.ak4n1.turn_management.feature.notification.dto.response.NotificationPreferenceResponse;
import com.ak4n1.turn_management.feature.notification.service.NotificationPreferenceService;
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
 * Controller para gestión de preferencias de notificación por usuarios.
 * Implementa US-T035.
 */
@RestController
@RequestMapping("/api/user/notification-preferences")
@PreAuthorize("hasRole('USER') or hasRole('ADMIN')")
public class NotificationPreferenceController {

    private static final Logger logger = LoggerFactory.getLogger(NotificationPreferenceController.class);

    private final NotificationPreferenceService preferenceService;
    private final JwtTokenProvider jwtTokenProvider;

    public NotificationPreferenceController(NotificationPreferenceService preferenceService,
                                           JwtTokenProvider jwtTokenProvider) {
        this.preferenceService = preferenceService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * GET /api/user/notification-preferences
     * 
     * Obtiene las preferencias de notificación del usuario autenticado.
     * 
     * @param httpRequest Request HTTP para obtener el usuario autenticado
     * @return Preferencias del usuario
     */
    @GetMapping
    public ResponseEntity<NotificationPreferenceResponse> getPreferences(HttpServletRequest httpRequest) {
        Long userId = getCurrentUserId(httpRequest);
        NotificationPreferenceResponse preferences = preferenceService.getPreferences(userId);
        return ResponseEntity.ok(preferences);
    }

    /**
     * PUT /api/user/notification-preferences
     * 
     * Actualiza las preferencias de notificación del usuario autenticado.
     * 
     * @param request Datos actualizados
     * @param httpRequest Request HTTP para obtener el usuario autenticado
     * @return Preferencias actualizadas
     */
    @PutMapping
    public ResponseEntity<NotificationPreferenceResponse> updatePreferences(
            @Valid @RequestBody UpdateNotificationPreferenceRequest request,
            HttpServletRequest httpRequest) {
        Long userId = getCurrentUserId(httpRequest);
        NotificationPreferenceResponse preferences = preferenceService.updatePreferences(userId, request);
        return ResponseEntity.ok(preferences);
    }

    /**
     * Obtiene el ID del usuario autenticado desde el token JWT.
     * 
     * @param request Request HTTP
     * @return ID del usuario autenticado
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

        throw new ApiException("Usuario no autenticado", HttpStatus.UNAUTHORIZED);
    }

    /**
     * Extrae el token JWT del request (cookie o header Authorization).
     * 
     * @param request Request HTTP
     * @return Token JWT o null si no se encuentra
     */
    private String extractTokenFromRequest(HttpServletRequest request) {
        // Intentar obtener de cookie
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        // Intentar obtener de header Authorization
        String authHeader = request.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }

        return null;
    }
}

