package com.ak4n1.turn_management.feature.auth.service;

import com.ak4n1.turn_management.feature.auth.domain.RefreshToken;
import com.ak4n1.turn_management.feature.auth.domain.Role;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.dto.request.*;
import com.ak4n1.turn_management.feature.auth.dto.response.ForgotPasswordResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.ResetPasswordResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;
import com.ak4n1.turn_management.feature.auth.mapper.UserMapper;
import com.ak4n1.turn_management.feature.auth.repository.RefreshTokenRepository;
import com.ak4n1.turn_management.feature.auth.repository.RoleRepository;
import com.ak4n1.turn_management.feature.notification.domain.PasswordResetToken;
import com.ak4n1.turn_management.feature.notification.repository.PasswordResetTokenRepository;
import com.ak4n1.turn_management.feature.notification.service.EmailService;
import com.ak4n1.turn_management.feature.notification.service.EmailVerificationService;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
public class AuthServiceImpl implements AuthService {

    private final UserService userService;
    private final PasswordService passwordService;
    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final RoleRepository roleRepository;
    private final UserMapper userMapper;
    private final EmailService emailService;
    private final EmailVerificationService emailVerificationService;
    private final PasswordResetTokenRepository passwordResetTokenRepository;

    private static final int MAX_LOGIN_ATTEMPTS = 5;
    private static final int LOCKOUT_DURATION_MINUTES = 30;
    private static final int PASSWORD_RESET_TOKEN_EXPIRATION_MINUTES = 15;

    public AuthServiceImpl(
            UserService userService,
            PasswordService passwordService,
            JwtTokenProvider jwtTokenProvider,
            RefreshTokenRepository refreshTokenRepository,
            RoleRepository roleRepository,
            UserMapper userMapper,
            EmailService emailService,
            EmailVerificationService emailVerificationService,
            PasswordResetTokenRepository passwordResetTokenRepository) {
        this.userService = userService;
        this.passwordService = passwordService;
        this.jwtTokenProvider = jwtTokenProvider;
        this.refreshTokenRepository = refreshTokenRepository;
        this.roleRepository = roleRepository;
        this.userMapper = userMapper;
        this.emailService = emailService;
        this.emailVerificationService = emailVerificationService;
        this.passwordResetTokenRepository = passwordResetTokenRepository;
    }

    @Override
    @Transactional
    public AuthResult login(LoginRequest request) {
        User user = userService.findByEmail(request.getEmail())
                .orElseThrow(() -> new BadCredentialsException("Email o contraseña inválidos"));

        if (userService.isAccountLocked(user)) {
            throw new BadCredentialsException("La cuenta está bloqueada. Por favor, intente nuevamente más tarde.");
        }

        if (!passwordService.matches(request.getPassword(), user.getPassword())) {
            userService.incrementFailedLoginAttempts(user);
            if (user.getFailedLoginAttempts() >= MAX_LOGIN_ATTEMPTS) {
                userService.lockAccount(user, LOCKOUT_DURATION_MINUTES);
                emailService.sendAccountLockedEmail(user, LOCKOUT_DURATION_MINUTES);
                throw new BadCredentialsException("La cuenta ha sido bloqueada debido a demasiados intentos fallidos de inicio de sesión.");
            }
            throw new BadCredentialsException("Email o contraseña inválidos");
        }

        if (!user.getEnabled()) {
            throw new BadCredentialsException("La cuenta está deshabilitada");
        }

        // Verificar que el email esté verificado antes de permitir el login
        if (!user.getEmailVerified()) {
            throw new BadCredentialsException("EMAIL_NOT_VERIFIED");
        }

        userService.resetFailedLoginAttempts(user);

        // Generate tokens using user ID
        String accessToken = jwtTokenProvider.generateToken(user.getId());
        String refreshToken = createRefreshToken(user);

        UserResponse userResponse = userMapper.toUserResponse(user);

        return new AuthResult(
                accessToken,
                refreshToken,
                jwtTokenProvider.getExpirationTime(),
                userResponse
        );
    }

    @Override
    @Transactional
    public UserResponse register(RegisterRequest request) {
        if (userService.existsByEmail(request.getEmail())) {
            throw new IllegalArgumentException("El email ya está registrado");
        }

        User user = new User();
        user.setEmail(request.getEmail());
        user.setPassword(passwordService.encodePassword(request.getPassword()));
        user.setFirstName(request.getFirstName());
        user.setLastName(request.getLastName());
        if (request.getPhone() != null) user.setPhone(request.getPhone());
        if (request.getStreet() != null) user.setStreet(request.getStreet());
        if (request.getStreetNumber() != null) user.setStreetNumber(request.getStreetNumber());
        if (request.getFloorApt() != null) user.setFloorApt(request.getFloorApt());
        if (request.getCity() != null) user.setCity(request.getCity());
        if (request.getPostalCode() != null) user.setPostalCode(request.getPostalCode());
        if (request.getBirthDate() != null) user.setBirthDate(request.getBirthDate());
        user.setEnabled(true);
        user.setAccountNonLocked(true);

        // Assign default role (ROLE_USER)
        Role userRole = roleRepository.findByName("ROLE_USER")
                .orElseGet(() -> {
                    Role role = new Role("ROLE_USER", "Default user role");
                    return roleRepository.save(role);
                });
        user.addRole(userRole);

        user = userService.save(user);

        // Send welcome email and verification email
        emailService.sendWelcomeEmail(user);
        String verificationToken = emailVerificationService.generateVerificationToken(user);
        emailService.sendEmailVerification(user, verificationToken);

        // No se generan tokens - el usuario debe hacer login manualmente
        // Pero devolvemos la información del usuario creado
        return userMapper.toUserResponse(user);
    }

    @Override
    @Transactional
    public AuthResult refreshToken(RefreshTokenRequest request) {
        String tokenString = request.getRefreshToken();
        
        // First, validate JWT structure and expiration
        if (!jwtTokenProvider.validateToken(tokenString)) {
            throw new BadCredentialsException("Token de actualización inválido o expirado");
        }
        
        // Extract user ID from JWT
        Long userId;
        try {
            userId = jwtTokenProvider.extractUserId(tokenString);
        } catch (Exception e) {
            throw new BadCredentialsException("Token de actualización inválido");
        }
        
        // Find token in database to check revocation status
        RefreshToken refreshToken = refreshTokenRepository.findByToken(tokenString)
                .orElseThrow(() -> new BadCredentialsException("Token de actualización no encontrado"));

        if (refreshToken.isExpired()) {
            refreshTokenRepository.delete(refreshToken);
            throw new BadCredentialsException("Token de actualización expirado");
        }

        if (refreshToken.getRevoked()) {
            throw new BadCredentialsException("Token de actualización revocado");
        }

        User user = refreshToken.getUser();
        if (!user.getEnabled()) {
            throw new BadCredentialsException("La cuenta de usuario está deshabilitada");
        }
        
        // Verificar que el email esté verificado antes de permitir el refresh
        if (!user.getEmailVerified()) {
            throw new BadCredentialsException("EMAIL_NOT_VERIFIED");
        }
        
        // Verify user ID matches
        if (!user.getId().equals(userId)) {
            throw new BadCredentialsException("El token no coincide con el usuario");
        }

        // Update last used
        refreshToken.setLastUsedAt(LocalDateTime.now());
        refreshTokenRepository.save(refreshToken);

        // Generate new tokens using user ID
        String newAccessToken = jwtTokenProvider.generateToken(user.getId());
        String newRefreshToken = rotateRefreshToken(refreshToken, user);

        UserResponse userResponse = userMapper.toUserResponse(user);

        return new AuthResult(
                newAccessToken,
                newRefreshToken,
                jwtTokenProvider.getExpirationTime(),
                userResponse
        );
    }

    @Override
    @Transactional
    public void logout(String refreshToken) {
        // Delete the refresh token from database
        refreshTokenRepository.findByToken(refreshToken)
                .ifPresent(refreshTokenRepository::delete);
    }

    @Override
    @Transactional
    public ForgotPasswordResponse forgotPassword(ForgotPasswordRequest request) {
        User user = userService.findByEmail(request.getEmail())
                .orElseThrow(() -> new IllegalArgumentException("Email no encontrado"));

        // Verify that the email is verified (security: only allow reset if email is verified)
        if (!user.getEmailVerified()) {
            throw new IllegalArgumentException("El email no está verificado. Por favor, verifica tu email primero.");
        }

        // Check if there's already a valid (non-expired and unused) token for this user
        // This prevents spam/abuse of the forgot password endpoint
        Optional<PasswordResetToken> existingToken = passwordResetTokenRepository.findByUser(user);
        if (existingToken.isPresent() && existingToken.get().isValid()) {
            PasswordResetToken token = existingToken.get();
            String timeRemaining = formatTimeRemaining(token.getExpiresAt());
            throw new IllegalArgumentException("Ya se ha enviado un token de restablecimiento de contraseña. Por favor, revisa tu email o espera " + timeRemaining + " antes de solicitar uno nuevo.");
        }

        // Delete existing token if any (expired or used tokens can be deleted)
        // This ensures we only have one token per user at a time
        if (existingToken.isPresent()) {
            passwordResetTokenRepository.delete(existingToken.get());
            passwordResetTokenRepository.flush();
        }

        // Generate new token
        String token = UUID.randomUUID().toString();
        LocalDateTime expiresAt = LocalDateTime.now().plusMinutes(PASSWORD_RESET_TOKEN_EXPIRATION_MINUTES);

        PasswordResetToken resetToken = new PasswordResetToken(token, user, expiresAt);
        passwordResetTokenRepository.save(resetToken);

        // Send password reset email (async - no bloquea la respuesta)
        emailService.sendPasswordReset(user, token);

        // Return success message with status
        return new ForgotPasswordResponse(200, "Se ha enviado un email con las instrucciones para restablecer tu contraseña. Por favor, revisa tu bandeja de entrada.");
    }

    @Override
    @Transactional
    public ResetPasswordResponse resetPassword(String token, ResetPasswordRequest request) {
        PasswordResetToken resetToken = passwordResetTokenRepository.findByToken(token)
                .orElseThrow(() -> new IllegalArgumentException("Token inválido o expirado"));

        if (!resetToken.isValid()) {
            throw new IllegalArgumentException("El token ha expirado o ya ha sido utilizado");
        }

        User user = resetToken.getUser();
        user.setPassword(passwordService.encodePassword(request.getNewPassword()));
        userService.save(user);

        // Mark token as used
        resetToken.setUsed(true);
        resetToken.setUsedAt(LocalDateTime.now());
        passwordResetTokenRepository.save(resetToken);

        // Send confirmation email
        emailService.sendPasswordResetConfirmation(user);

        // Return success message with status
        return new ResetPasswordResponse(200, "Tu contraseña ha sido restablecida exitosamente. Ya puedes iniciar sesión con tu nueva contraseña.");
    }

    @Override
    @Transactional
    public void resendVerificationEmail(String email) {
        User user = userService.findByEmail(email)
                .orElseThrow(() -> new IllegalArgumentException("Email no encontrado"));
        emailVerificationService.resendVerificationEmail(user);
    }

    /**
     * Formatea el tiempo restante hasta la expiración en formato legible (ej: "4m 30s").
     * 
     * @param expiresAt Fecha y hora de expiración
     * @return String formateado con el tiempo restante
     */
    private String formatTimeRemaining(LocalDateTime expiresAt) {
        LocalDateTime now = LocalDateTime.now();
        Duration duration = Duration.between(now, expiresAt);
        
        long totalSeconds = duration.getSeconds();
        
        if (totalSeconds <= 0) {
            return "0s";
        }
        
        long minutes = totalSeconds / 60;
        long seconds = totalSeconds % 60;
        
        if (minutes > 0 && seconds > 0) {
            return minutes + "m " + seconds + "s";
        } else if (minutes > 0) {
            return minutes + "m";
        } else {
            return seconds + "s";
        }
    }

    /**
     * Creates or updates a refresh token for the user.
     * Ensures only ONE active token per user exists in the database.
     * If an active token exists, it is updated. If not, a new one is created.
     * 
     * @param user The user for whom the token is created/updated
     * @return The generated JWT refresh token string
     */
    private String createRefreshToken(User user) {
        LocalDateTime now = LocalDateTime.now();
        
        // Find last token for this user (active or expired, doesn't matter)
        // This ensures we always update the same row instead of creating new ones
        List<RefreshToken> existingTokens = refreshTokenRepository.findLastTokenByUser(user, PageRequest.of(0, 1));
        
        // Generate new JWT refresh token using user ID
        String token = jwtTokenProvider.generateRefreshToken(user.getId());
        
        // Calculate expiration date from JWT expiration time
        LocalDateTime expiresAt = LocalDateTime.now().plusSeconds(
                jwtTokenProvider.getRefreshExpirationTime() / 1000
        );

        RefreshToken refreshToken;
        if (!existingTokens.isEmpty()) {
            // Update existing token (same row, no new record) - ONE TOKEN PER USER
            refreshToken = existingTokens.get(0);
            refreshToken.setToken(token);
            refreshToken.setExpiresAt(expiresAt);
            refreshToken.setRevoked(false);
            refreshToken.setLastUsedAt(now);
        } else {
            // Create new token only if user has never had a token before
            refreshToken = new RefreshToken(token, user, expiresAt);
            refreshToken.setLastUsedAt(now);
        }
        
        refreshTokenRepository.save(refreshToken);
        return token;
    }

    /**
     * Rotates a refresh token by updating the existing token with a new one.
     * This ensures only ONE active token per user exists.
     * 
     * @param currentToken The current refresh token to be rotated
     * @param user The user who owns the token
     * @return The new JWT refresh token string
     */
    private String rotateRefreshToken(RefreshToken currentToken, User user) {
        // Generate new JWT refresh token using user ID
        String newToken = jwtTokenProvider.generateRefreshToken(user.getId());
        
        // Calculate expiration date from JWT expiration time
        LocalDateTime expiresAt = LocalDateTime.now().plusSeconds(
                jwtTokenProvider.getRefreshExpirationTime() / 1000
        );

        // Update existing token instead of creating a new one (one token per user)
        currentToken.setToken(newToken);
        currentToken.setExpiresAt(expiresAt);
        currentToken.setRevoked(false);
        currentToken.setLastUsedAt(LocalDateTime.now());
        refreshTokenRepository.save(currentToken);

        return newToken;
    }

    @Override
    public UserResponse getProfile(Long userId) {
        User user = userService.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));
        return userMapper.toUserResponse(user);
    }

    @Override
    @Transactional
    public UserResponse updateProfile(Long userId, UpdateProfileRequest request) {
        User user = userService.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("Usuario no encontrado"));
        if (request.getFirstName() != null && !request.getFirstName().isBlank()) user.setFirstName(request.getFirstName().trim());
        if (request.getLastName() != null && !request.getLastName().isBlank()) user.setLastName(request.getLastName().trim());
        if (request.getPhone() != null) user.setPhone(request.getPhone().trim().isEmpty() ? null : request.getPhone().trim());
        if (request.getStreet() != null) user.setStreet(request.getStreet().trim().isEmpty() ? null : request.getStreet().trim());
        if (request.getStreetNumber() != null) user.setStreetNumber(request.getStreetNumber().trim().isEmpty() ? null : request.getStreetNumber().trim());
        if (request.getFloorApt() != null) user.setFloorApt(request.getFloorApt().trim().isEmpty() ? null : request.getFloorApt().trim());
        if (request.getCity() != null) user.setCity(request.getCity().trim().isEmpty() ? null : request.getCity().trim());
        if (request.getPostalCode() != null) user.setPostalCode(request.getPostalCode().trim().isEmpty() ? null : request.getPostalCode().trim());
        if (request.getBirthDate() != null) user.setBirthDate(request.getBirthDate());
        user = userService.save(user);
        return userMapper.toUserResponse(user);
    }
}

