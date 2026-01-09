package com.ak4n1.turn_management.feature.configuration.controller;

import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.configuration.dto.request.BusinessPolicyRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.BusinessPolicyResponse;
import com.ak4n1.turn_management.feature.configuration.service.BusinessPolicyService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para gestión de políticas de negocio.
 * 
 * Implementa US-T019.
 */
@RestController
@RequestMapping("/api/admin/policies")
public class BusinessPolicyController {

    private static final Logger logger = LoggerFactory.getLogger(BusinessPolicyController.class);

    private final BusinessPolicyService policyService;
    private final JwtTokenProvider jwtTokenProvider;
    private final UserService userService;

    public BusinessPolicyController(BusinessPolicyService policyService, JwtTokenProvider jwtTokenProvider, UserService userService) {
        this.policyService = policyService;
        this.jwtTokenProvider = jwtTokenProvider;
        this.userService = userService;
    }

    /**
     * Crea o actualiza una política de negocio (solo admin).
     * 
     * POST /api/admin/policies
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T019.
     * 
     * @param request Request con los valores de la política
     * @param httpRequest HttpServletRequest para obtener el token
     * @return Política creada o actualizada (200 OK)
     */
    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<BusinessPolicyResponse> createOrUpdatePolicy(
            @Valid @RequestBody BusinessPolicyRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Creando o actualizando política de negocio (admin)");

        Long userId = getCurrentUserId(httpRequest);
        BusinessPolicyResponse response = policyService.createOrUpdatePolicy(request, userId);

        logger.info("Política creada/actualizada - ID: {}", response.getId());

        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene la política activa actual (solo admin).
     * 
     * GET /api/admin/policies/active
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T019.
     * 
     * @return Política activa si existe (200 OK) o 404 si no existe
     */
    @GetMapping("/active")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<BusinessPolicyResponse> getActivePolicy() {
        logger.info("Consultando política activa (admin)");

        BusinessPolicyResponse response = policyService.getActivePolicy();
        
        if (response == null) {
            logger.info("No existe política activa");
            return ResponseEntity.notFound().build();
        }

        logger.info("Política activa encontrada - ID: {}", response.getId());
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el ID del usuario autenticado desde el token JWT.
     */
    private Long getCurrentUserId(HttpServletRequest request) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new ApiException("Usuario no autenticado", HttpStatus.UNAUTHORIZED);
        }

        // Intentar obtener userId del token JWT
        String token = getAccessTokenFromRequest(request);
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

        // Fallback: obtener desde UserDetails
        Object principal = authentication.getPrincipal();
        if (principal instanceof UserDetails) {
            String email = ((UserDetails) principal).getUsername();
            try {
                return userService.findByEmail(email)
                    .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND))
                    .getId();
            } catch (Exception e) {
                logger.warn("No se pudo obtener userId desde email: {}", e.getMessage());
            }
        }
        
        throw new ApiException(
            "No se pudo obtener el ID del usuario autenticado",
            HttpStatus.INTERNAL_SERVER_ERROR);
    }

    /**
     * Obtiene el token de acceso desde cookies o Authorization header.
     */
    private String getAccessTokenFromRequest(HttpServletRequest request) {
        // Intentar desde cookies
        jakarta.servlet.http.Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (jakarta.servlet.http.Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        // Fallback: Authorization header
        String authHeader = request.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }

        return null;
    }
}

