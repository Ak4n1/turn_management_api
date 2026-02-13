package com.ak4n1.turn_management.feature.auth.service;

import com.ak4n1.turn_management.feature.auth.domain.Role;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.dto.response.AdminUsersPageResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;
import com.ak4n1.turn_management.feature.auth.mapper.UserMapper;
import com.ak4n1.turn_management.feature.auth.repository.RoleRepository;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.domain.PasswordResetToken;
import com.ak4n1.turn_management.feature.notification.repository.PasswordResetTokenRepository;
import com.ak4n1.turn_management.feature.notification.service.EmailService;
import com.ak4n1.turn_management.feature.notification.service.EmailVerificationService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class AdminUserServiceImpl implements AdminUserService {

    private static final int PASSWORD_RESET_TOKEN_EXPIRATION_MINUTES = 15;

    private final UserRepository userRepository;
    private final RoleRepository roleRepository;
    private final UserMapper userMapper;
    private final EmailVerificationService emailVerificationService;
    private final EmailService emailService;
    private final PasswordResetTokenRepository passwordResetTokenRepository;

    public AdminUserServiceImpl(UserRepository userRepository,
                                RoleRepository roleRepository,
                                UserMapper userMapper,
                                EmailVerificationService emailVerificationService,
                                EmailService emailService,
                                PasswordResetTokenRepository passwordResetTokenRepository) {
        this.userRepository = userRepository;
        this.roleRepository = roleRepository;
        this.userMapper = userMapper;
        this.emailVerificationService = emailVerificationService;
        this.emailService = emailService;
        this.passwordResetTokenRepository = passwordResetTokenRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminUsersPageResponse listUsers(String search, String createdFrom, String createdTo,
                                            Boolean emailVerified, Boolean profileComplete,
                                            String roleFilter, Boolean enabled, int page, int size) {
        LocalDateTime from = parseDateToStartOfDay(createdFrom);
        LocalDateTime to = parseDateToEndOfDay(createdTo);

        String searchTrimmed = (search != null && !search.trim().isEmpty()) ? search.trim() : null;
        String role = (roleFilter != null && !roleFilter.trim().isEmpty()) ? roleFilter.trim() : null;

        PageRequest pageable = PageRequest.of(page, size);
        Page<User> result = userRepository.findAllWithFilters(
                searchTrimmed, from, to, emailVerified, profileComplete, role, enabled, pageable);

        List<UserResponse> content = result.getContent().stream()
                .map(userMapper::toUserResponse)
                .collect(Collectors.toList());

        return new AdminUsersPageResponse(
                content,
                result.getTotalElements(),
                result.getTotalPages(),
                result.getNumber(),
                result.getSize());
    }

    @Override
    @Transactional(readOnly = true)
    public UserResponse getUserById(Long id) {
        User user = userRepository.findByIdWithRoles(id)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));
        return userMapper.toUserResponse(user);
    }

    @Override
    @Transactional
    public void resendVerificationEmail(Long userId) {
        User user = userRepository.findByIdWithRoles(userId)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));
        emailVerificationService.resendVerificationEmail(user);
    }

    @Override
    @Transactional
    public void sendResetPasswordEmail(Long userId) {
        User user = userRepository.findByIdWithRoles(userId)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));

        Optional<PasswordResetToken> existing = passwordResetTokenRepository.findByUser(user);
        if (existing.isPresent()) {
            passwordResetTokenRepository.delete(existing.get());
            passwordResetTokenRepository.flush();
        }

        String token = UUID.randomUUID().toString();
        LocalDateTime expiresAt = LocalDateTime.now().plusMinutes(PASSWORD_RESET_TOKEN_EXPIRATION_MINUTES);
        PasswordResetToken resetToken = new PasswordResetToken(token, user, expiresAt);
        passwordResetTokenRepository.save(resetToken);

        emailService.sendPasswordReset(user, token);
    }

    @Override
    @Transactional
    public UserResponse setUserRole(Long userId, String role) {
        User user = userRepository.findByIdWithRoles(userId)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));

        Role adminRole = roleRepository.findByName("ROLE_ADMIN")
                .orElseThrow(() -> new IllegalStateException("Rol ROLE_ADMIN no existe"));
        Role userRole = roleRepository.findByName("ROLE_USER")
                .orElseThrow(() -> new IllegalStateException("Rol ROLE_USER no existe"));

        if ("ROLE_ADMIN".equals(role)) {
            if (!user.getRoles().contains(adminRole)) {
                user.addRole(adminRole);
            }
        } else if ("ROLE_USER".equals(role)) {
            user.getRoles().remove(adminRole);
            if (!user.getRoles().contains(userRole)) {
                user.addRole(userRole);
            }
        } else {
            throw new IllegalArgumentException("Rol no válido: " + role);
        }

        user = userRepository.save(user);
        return userMapper.toUserResponse(user);
    }

    @Override
    @Transactional
    public UserResponse setUserEnabled(Long userId, boolean enabled) {
        User user = userRepository.findByIdWithRoles(userId)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));
        user.setEnabled(enabled);
        user = userRepository.save(user);
        return userMapper.toUserResponse(user);
    }

    private LocalDateTime parseDateToStartOfDay(String dateStr) {
        if (dateStr == null || dateStr.trim().isEmpty()) return null;
        try {
            LocalDate d = LocalDate.parse(dateStr.trim());
            return d.atStartOfDay();
        } catch (Exception e) {
            return null;
        }
    }

    private LocalDateTime parseDateToEndOfDay(String dateStr) {
        if (dateStr == null || dateStr.trim().isEmpty()) return null;
        try {
            LocalDate d = LocalDate.parse(dateStr.trim());
            return d.atTime(LocalTime.MAX);
        } catch (Exception e) {
            return null;
        }
    }
}
