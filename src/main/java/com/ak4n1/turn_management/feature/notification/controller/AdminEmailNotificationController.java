package com.ak4n1.turn_management.feature.notification.controller;

import com.ak4n1.turn_management.feature.notification.domain.EmailNotificationType;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailStatus;
import com.ak4n1.turn_management.feature.notification.dto.response.FailedEmailNotificationResponse;
import com.ak4n1.turn_management.feature.notification.service.FailedEmailNotificationService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para gestión de emails fallidos por administradores.
 * Implementa US-T031.1.
 */
@RestController
@RequestMapping("/api/admin/notifications")
@PreAuthorize("hasRole('ADMIN')")
public class AdminEmailNotificationController {

    private final FailedEmailNotificationService failedEmailService;
    private final JwtTokenProvider jwtTokenProvider;

    public AdminEmailNotificationController(FailedEmailNotificationService failedEmailService,
                                          JwtTokenProvider jwtTokenProvider) {
        this.failedEmailService = failedEmailService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * GET /api/admin/notifications/failed
     * 
     * Consulta emails fallidos con filtros opcionales.
     * 
     * @param status Estado del email fallido (opcional)
     * @param notificationType Tipo de notificación (opcional)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20)
     * @return Página de emails fallidos
     */
    @GetMapping("/failed")
    public ResponseEntity<Page<FailedEmailNotificationResponse>> getFailedEmails(
            @RequestParam(required = false) FailedEmailStatus status,
            @RequestParam(required = false) EmailNotificationType notificationType,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        Page<FailedEmailNotificationResponse> failedEmails = failedEmailService.getFailedEmails(
            status, notificationType, pageable);
        
        return ResponseEntity.ok(failedEmails);
    }

    /**
     * POST /api/admin/notifications/{id}/retry
     * 
     * Reenvía un email fallido.
     * 
     * @param id ID del email fallido
     * @param httpRequest Request HTTP para obtener el usuario autenticado
     * @return Email fallido actualizado
     */
    @PostMapping("/{id}/retry")
    public ResponseEntity<FailedEmailNotificationResponse> retryFailedEmail(
            @PathVariable Long id,
            HttpServletRequest httpRequest) {
        
        Long adminUserId = getCurrentUserId(httpRequest);
        FailedEmailNotificationResponse response = failedEmailService.retryFailedEmail(id, adminUserId);
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el ID del usuario autenticado desde el token JWT.
     * 
     * @param request Request HTTP
     * @return ID del usuario autenticado
     */
    private Long getCurrentUserId(HttpServletRequest request) {
        // Intentar obtener userId del token JWT
        String token = extractTokenFromRequest(request);
        if (token != null) {
            try {
                Long userId = jwtTokenProvider.extractUserId(token);
                if (userId != null) {
                    return userId;
                }
            } catch (Exception e) {
                // Si falla, lanzar excepción
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

