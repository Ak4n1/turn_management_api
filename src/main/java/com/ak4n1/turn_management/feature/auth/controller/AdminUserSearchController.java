package com.ak4n1.turn_management.feature.auth.controller;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.dto.response.AdminUserSearchItemResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.AdminUsersPageResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.auth.service.AdminUserService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.websocket.AppointmentWebSocketHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Gestión y búsqueda de usuarios para UI administrativa.
 */
@RestController
@RequestMapping("/api/admin/users")
@PreAuthorize("hasRole('ADMIN')")
public class AdminUserSearchController {

    private static final Logger logger = LoggerFactory.getLogger(AdminUserSearchController.class);

    private final UserRepository userRepository;
    private final AdminUserService adminUserService;
    private final AppointmentWebSocketHandler webSocketHandler;

    public AdminUserSearchController(UserRepository userRepository, AdminUserService adminUserService,
                                    AppointmentWebSocketHandler webSocketHandler) {
        this.userRepository = userRepository;
        this.adminUserService = adminUserService;
        this.webSocketHandler = webSocketHandler;
    }

    /**
     * Obtiene el número y lista de emails de usuarios conectados vía WebSocket en tiempo real.
     * GET /api/admin/users/online-count
     */
    @GetMapping("/online-count")
    public ResponseEntity<Map<String, Object>> getOnlineUsersCount() {
        return ResponseEntity.ok(Map.of(
                "count", webSocketHandler.getOnlineUsersCount(),
                "emails", webSocketHandler.getOnlineUserEmails()));
    }

    /**
     * Busca usuarios por email o nombre (parcial).
     *
     * GET /api/admin/users/search?query=juan&limit=10
     */
    @GetMapping("/search")
    public ResponseEntity<List<AdminUserSearchItemResponse>> searchUsers(
            @RequestParam(name = "query") String query,
            @RequestParam(name = "limit", defaultValue = "10") int limit
    ) {
        String q = query == null ? "" : query.trim();
        if (q.length() < 2) {
            return ResponseEntity.ok(List.of());
        }
        if (limit < 1 || limit > 50) {
            throw new ApiException("El parámetro limit debe estar entre 1 y 50", HttpStatus.BAD_REQUEST);
        }

        try {
            List<Long> ids = userRepository.findUserIdsBySearchTerm(q);
            if (ids == null || ids.isEmpty()) {
                return ResponseEntity.ok(List.of());
            }

            List<Long> limitedIds = ids.size() > limit ? ids.subList(0, limit) : ids;
            List<User> users = userRepository.findAllById(limitedIds);
            if (users == null) {
                users = new ArrayList<>();
            }

            List<AdminUserSearchItemResponse> response = users.stream()
                    .map(u -> new AdminUserSearchItemResponse(
                            u.getId(),
                            u.getEmail(),
                            u.getFirstName(),
                            u.getLastName()
                    ))
                    .collect(Collectors.toList());

            return ResponseEntity.ok(response);
        } catch (RuntimeException e) {
            logger.error("Error buscando usuarios. Query: {}", q, e);
            throw e;
        }
    }

    /**
     * Lista usuarios con filtros y paginación.
     * GET /api/admin/users
     */
    @GetMapping
    public ResponseEntity<AdminUsersPageResponse> listUsers(
            @RequestParam(required = false) String search,
            @RequestParam(required = false) String createdFrom,
            @RequestParam(required = false) String createdTo,
            @RequestParam(required = false) Boolean emailVerified,
            @RequestParam(required = false) Boolean profileComplete,
            @RequestParam(required = false) String role,
            @RequestParam(required = false) Boolean enabled,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        if (size < 1 || size > 100) {
            throw new ApiException("El tamaño de página debe estar entre 1 y 100", HttpStatus.BAD_REQUEST);
        }
        AdminUsersPageResponse response = adminUserService.listUsers(
                search, createdFrom, createdTo, emailVerified, profileComplete, role, enabled, page, size);
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene detalle de un usuario.
     * GET /api/admin/users/{id}
     */
    @GetMapping("/{id}")
    public ResponseEntity<UserResponse> getUserById(@PathVariable Long id) {
        UserResponse response = adminUserService.getUserById(id);
        return ResponseEntity.ok(response);
    }

    /**
     * Reenvía email de verificación.
     * POST /api/admin/users/{id}/resend-verify
     */
    @PostMapping("/{id}/resend-verify")
    public ResponseEntity<Map<String, String>> resendVerification(@PathVariable Long id) {
        adminUserService.resendVerificationEmail(id);
        return ResponseEntity.ok(Map.of("message", "Email de verificación enviado"));
    }

    /**
     * Envía email para restablecer contraseña.
     * POST /api/admin/users/{id}/send-reset-password
     */
    @PostMapping("/{id}/send-reset-password")
    public ResponseEntity<Map<String, String>> sendResetPassword(@PathVariable Long id) {
        adminUserService.sendResetPasswordEmail(id);
        return ResponseEntity.ok(Map.of("message", "Email de restablecimiento de contraseña enviado"));
    }

    /**
     * Cambia el rol del usuario.
     * PATCH /api/admin/users/{id}/role
     */
    @PatchMapping("/{id}/role")
    public ResponseEntity<UserResponse> setRole(@PathVariable Long id, @RequestBody Map<String, String> body) {
        String role = body != null ? body.get("role") : null;
        if (role == null || role.isBlank()) {
            throw new ApiException("El campo 'role' es requerido", HttpStatus.BAD_REQUEST);
        }
        UserResponse response = adminUserService.setUserRole(id, role.trim());
        return ResponseEntity.ok(response);
    }

    /**
     * Habilita o deshabilita la cuenta.
     * PATCH /api/admin/users/{id}/enabled
     */
    @PatchMapping("/{id}/enabled")
    public ResponseEntity<UserResponse> setEnabled(@PathVariable Long id, @RequestBody Map<String, Boolean> body) {
        Boolean enabled = body != null ? body.get("enabled") : null;
        if (enabled == null) {
            throw new ApiException("El campo 'enabled' es requerido", HttpStatus.BAD_REQUEST);
        }
        UserResponse response = adminUserService.setUserEnabled(id, enabled);
        return ResponseEntity.ok(response);
    }
}

