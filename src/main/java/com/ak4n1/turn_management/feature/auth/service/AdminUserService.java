package com.ak4n1.turn_management.feature.auth.service;

import com.ak4n1.turn_management.feature.auth.dto.response.AdminUsersPageResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;

/**
 * Servicio para gestión de usuarios por administradores.
 */
public interface AdminUserService {

    /**
     * Lista usuarios con filtros y paginación.
     */
    AdminUsersPageResponse listUsers(
            String search,
            String createdFrom,
            String createdTo,
            Boolean emailVerified,
            Boolean profileComplete,
            String roleFilter,
            Boolean enabled,
            int page,
            int size);

    /**
     * Obtiene detalle de un usuario por ID.
     */
    UserResponse getUserById(Long id);

    /**
     * Reenvía email de verificación al usuario.
     */
    void resendVerificationEmail(Long userId);

    /**
     * Envía email para restablecer contraseña (admin trigger).
     */
    void sendResetPasswordEmail(Long userId);

    /**
     * Cambia el rol del usuario (agregar ROLE_ADMIN o quitar).
     */
    UserResponse setUserRole(Long userId, String role);

    /**
     * Habilita o deshabilita la cuenta del usuario.
     */
    UserResponse setUserEnabled(Long userId, boolean enabled);
}
