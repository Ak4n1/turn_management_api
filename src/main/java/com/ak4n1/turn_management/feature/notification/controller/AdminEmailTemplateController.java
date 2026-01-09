package com.ak4n1.turn_management.feature.notification.controller;

import com.ak4n1.turn_management.feature.notification.dto.request.CreateEmailTemplateRequest;
import com.ak4n1.turn_management.feature.notification.dto.response.EmailTemplateResponse;
import com.ak4n1.turn_management.feature.notification.service.EmailTemplateManagementService;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;
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

import java.util.List;

/**
 * Controller para gestión de plantillas de email por administradores.
 * Implementa US-T034.
 */
@RestController
@RequestMapping("/api/admin/email-templates")
@PreAuthorize("hasRole('ADMIN')")
public class AdminEmailTemplateController {

    private static final Logger logger = LoggerFactory.getLogger(AdminEmailTemplateController.class);

    private final EmailTemplateManagementService templateService;
    private final JwtTokenProvider jwtTokenProvider;

    public AdminEmailTemplateController(EmailTemplateManagementService templateService,
                                        JwtTokenProvider jwtTokenProvider) {
        this.templateService = templateService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * GET /api/admin/email-templates
     * 
     * Obtiene todas las plantillas activas.
     * 
     * @return Lista de plantillas activas
     */
    @GetMapping
    public ResponseEntity<List<EmailTemplateResponse>> getAllActiveTemplates() {
        List<EmailTemplateResponse> templates = templateService.getAllActiveTemplates();
        return ResponseEntity.ok(templates);
    }

    /**
     * GET /api/admin/email-templates/{type}
     * 
     * Obtiene todas las versiones de un tipo de plantilla.
     * 
     * @param type Tipo de plantilla
     * @return Lista de plantillas del tipo especificado
     */
    @GetMapping("/{type}")
    public ResponseEntity<List<EmailTemplateResponse>> getTemplatesByType(
            @PathVariable EmailTemplateType type) {
        List<EmailTemplateResponse> templates = templateService.getTemplatesByType(type);
        return ResponseEntity.ok(templates);
    }

    /**
     * GET /api/admin/email-templates/{type}/active
     * 
     * Obtiene la plantilla activa de un tipo específico.
     * 
     * @param type Tipo de plantilla
     * @return Plantilla activa o 404 si no existe
     */
    @GetMapping("/{type}/active")
    public ResponseEntity<EmailTemplateResponse> getActiveTemplate(
            @PathVariable EmailTemplateType type) {
        EmailTemplateResponse template = templateService.getActiveTemplate(type);
        if (template == null) {
            throw new ApiException("No existe una plantilla activa para el tipo: " + type, HttpStatus.NOT_FOUND);
        }
        return ResponseEntity.ok(template);
    }

    /**
     * POST /api/admin/email-templates
     * 
     * Crea una nueva plantilla (nueva versión).
     * 
     * @param request Datos de la plantilla
     * @param httpRequest Request HTTP para obtener el usuario autenticado
     * @return Plantilla creada
     */
    @PostMapping
    public ResponseEntity<EmailTemplateResponse> createTemplate(
            @Valid @RequestBody CreateEmailTemplateRequest request,
            HttpServletRequest httpRequest) {
        Long userId = getCurrentUserId(httpRequest);
        EmailTemplateResponse template = templateService.createTemplate(request, userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(template);
    }

    /**
     * PUT /api/admin/email-templates/{id}
     * 
     * Actualiza una plantilla (crea una nueva versión).
     * 
     * @param id ID de la plantilla a actualizar
     * @param request Datos actualizados
     * @param httpRequest Request HTTP para obtener el usuario autenticado
     * @return Nueva versión de la plantilla
     */
    @PutMapping("/{id}")
    public ResponseEntity<EmailTemplateResponse> updateTemplate(
            @PathVariable Long id,
            @Valid @RequestBody CreateEmailTemplateRequest request,
            HttpServletRequest httpRequest) {
        Long userId = getCurrentUserId(httpRequest);
        EmailTemplateResponse template = templateService.updateTemplate(id, request, userId);
        return ResponseEntity.ok(template);
    }

    /**
     * PUT /api/admin/email-templates/{id}/active
     * 
     * Activa o desactiva una plantilla.
     * 
     * @param id ID de la plantilla
     * @param active Estado activo (true/false)
     * @param httpRequest Request HTTP para obtener el usuario autenticado
     * @return Plantilla actualizada
     */
    @PutMapping("/{id}/active")
    public ResponseEntity<EmailTemplateResponse> setTemplateActive(
            @PathVariable Long id,
            @RequestParam Boolean active,
            HttpServletRequest httpRequest) {
        Long userId = getCurrentUserId(httpRequest);
        EmailTemplateResponse template = templateService.setTemplateActive(id, active, userId);
        return ResponseEntity.ok(template);
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

