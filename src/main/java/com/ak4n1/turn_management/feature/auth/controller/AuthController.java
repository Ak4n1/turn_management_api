package com.ak4n1.turn_management.feature.auth.controller;

import com.ak4n1.turn_management.feature.auth.dto.request.*;
import com.ak4n1.turn_management.feature.auth.dto.response.AuthResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.ForgotPasswordResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.LogoutResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.ResendVerificationResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.ResetPasswordResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.VerifyEmailResponse;
import com.ak4n1.turn_management.feature.auth.service.AuthService;
import com.ak4n1.turn_management.feature.auth.service.AuthResult;
import com.ak4n1.turn_management.feature.notification.service.EmailVerificationService;
import com.ak4n1.turn_management.shared.config.CookieConfig;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/auth")
public class AuthController {

    private final AuthService authService;
    private final CookieConfig cookieConfig;
    private final EmailVerificationService emailVerificationService;
    private final JwtTokenProvider jwtTokenProvider;

    public AuthController(AuthService authService, CookieConfig cookieConfig,
                          EmailVerificationService emailVerificationService,
                          JwtTokenProvider jwtTokenProvider) {
        this.authService = authService;
        this.cookieConfig = cookieConfig;
        this.emailVerificationService = emailVerificationService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(
            @Valid @RequestBody LoginRequest request,
            HttpServletResponse response) {
        AuthResult authResult = authService.login(request);
        setAccessTokenCookie(response, authResult.getAccessToken());
        setRefreshTokenCookie(response, authResult.getRefreshToken());
        AuthResponse responseBody = new AuthResponse(authResult.getUser());
        return ResponseEntity.ok(responseBody);
    }

    @PostMapping("/register")
    public ResponseEntity<AuthResponse> register(@Valid @RequestBody RegisterRequest request) {
        UserResponse userResponse = authService.register(request);
        AuthResponse responseBody = new AuthResponse(userResponse);
        return ResponseEntity.status(HttpStatus.CREATED).body(responseBody);
    }

    @PostMapping("/refresh")
    public ResponseEntity<AuthResponse> refreshToken(
            HttpServletRequest request,
            HttpServletResponse response) {
        String refreshToken = getRefreshTokenFromCookie(request);
        if (refreshToken == null) {
            return ResponseEntity.badRequest().build();
        }
        RefreshTokenRequest refreshRequest = new RefreshTokenRequest(refreshToken);
        AuthResult authResult = authService.refreshToken(refreshRequest);
        setAccessTokenCookie(response, authResult.getAccessToken());
        setRefreshTokenCookie(response, authResult.getRefreshToken());
        AuthResponse responseBody = new AuthResponse(authResult.getUser());
        return ResponseEntity.ok(responseBody);
    }

    @PostMapping("/logout")
    public ResponseEntity<LogoutResponse> logout(
            HttpServletRequest request,
            HttpServletResponse response) {
        String refreshToken = getRefreshTokenFromCookie(request);
        if (refreshToken != null) {
            authService.logout(refreshToken);
        }
        deleteAccessTokenCookie(response);
        deleteRefreshTokenCookie(response);
        LogoutResponse responseBody = new LogoutResponse(200, "Sesión cerrada exitosamente");
        return ResponseEntity.ok(responseBody);
    }

    @PostMapping("/forgot-password")
    public ResponseEntity<ForgotPasswordResponse> forgotPassword(@Valid @RequestBody ForgotPasswordRequest request) {
        ForgotPasswordResponse response = authService.forgotPassword(request);
        return ResponseEntity.ok(response);
    }

    @PostMapping("/reset-password/{token}")
    public ResponseEntity<ResetPasswordResponse> resetPassword(
            @PathVariable String token,
            @Valid @RequestBody ResetPasswordRequest request) {
        ResetPasswordResponse response = authService.resetPassword(token, request);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/verify-email")
    public ResponseEntity<VerifyEmailResponse> verifyEmail(@RequestParam String token) {
        boolean verified = emailVerificationService.verifyEmail(token);
        if (verified) {
            VerifyEmailResponse response = new VerifyEmailResponse(200, "Email verificado exitosamente");
            return ResponseEntity.ok(response);
        } else {
            VerifyEmailResponse response = new VerifyEmailResponse(400, "Token de verificación inválido o expirado");
            return ResponseEntity.badRequest().body(response);
        }
    }

    @PostMapping("/resend-verification")
    public ResponseEntity<ResendVerificationResponse> resendVerificationEmail(@RequestParam String email) {
        authService.resendVerificationEmail(email);
        ResendVerificationResponse response = new ResendVerificationResponse(200, "Se ha reenviado el email de verificación. Por favor, revisa tu bandeja de entrada.");
        return ResponseEntity.ok(response);
    }

    @GetMapping("/me")
    public ResponseEntity<UserResponse> getProfile(HttpServletRequest request) {
        Long userId = getCurrentUserId(request);
        UserResponse user = authService.getProfile(userId);
        return ResponseEntity.ok(user);
    }

    @PatchMapping("/me")
    public ResponseEntity<UserResponse> updateProfile(
            HttpServletRequest request,
            @Valid @RequestBody UpdateProfileRequest body) {
        Long userId = getCurrentUserId(request);
        UserResponse user = authService.updateProfile(userId, body);
        return ResponseEntity.ok(user);
    }

    private Long getCurrentUserId(HttpServletRequest request) {
        String token = getAccessTokenFromCookie(request);
        if (token == null) {
            String authHeader = request.getHeader("Authorization");
            if (authHeader != null && authHeader.startsWith("Bearer ")) {
                token = authHeader.substring(7);
            }
        }
        if (token == null) {
            throw new IllegalArgumentException("No se pudo obtener el token de autenticación");
        }
        Long userId = jwtTokenProvider.extractUserId(token);
        if (userId == null) {
            throw new IllegalArgumentException("Token inválido");
        }
        return userId;
    }

    private String getAccessTokenFromCookie(HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }
        return null;
    }

    private void setAccessTokenCookie(HttpServletResponse response, String accessToken) {
        String sameSite = cookieConfig.getSameSite();
        StringBuilder cookieHeader = new StringBuilder();
        cookieHeader.append("accessToken=").append(accessToken);
        cookieHeader.append("; Path=").append(cookieConfig.getPath());
        cookieHeader.append("; Max-Age=").append(cookieConfig.getMaxAge());
        cookieHeader.append("; HttpOnly");
        if (cookieConfig.isSecure()) {
            cookieHeader.append("; Secure");
        }
        cookieHeader.append("; SameSite=").append(sameSite);
        response.addHeader("Set-Cookie", cookieHeader.toString());
    }

    private void setRefreshTokenCookie(HttpServletResponse response, String refreshToken) {
        String sameSite = cookieConfig.getSameSite();
        StringBuilder cookieHeader = new StringBuilder();
        cookieHeader.append("refreshToken=").append(refreshToken);
        cookieHeader.append("; Path=").append(cookieConfig.getPath());
        cookieHeader.append("; Max-Age=").append(cookieConfig.getMaxAge());
        cookieHeader.append("; HttpOnly");
        if (cookieConfig.isSecure()) {
            cookieHeader.append("; Secure");
        }
        cookieHeader.append("; SameSite=").append(sameSite);
        response.addHeader("Set-Cookie", cookieHeader.toString());
    }

    private void deleteAccessTokenCookie(HttpServletResponse response) {
        Cookie cookie = new Cookie("accessToken", null);
        cookie.setHttpOnly(cookieConfig.isHttpOnly());
        cookie.setSecure(cookieConfig.isSecure());
        cookie.setPath(cookieConfig.getPath());
        cookie.setMaxAge(0);
        
        if (!cookieConfig.getDomain().equals("localhost")) {
            cookie.setDomain(cookieConfig.getDomain());
        }

        response.addCookie(cookie);
    }

    private void deleteRefreshTokenCookie(HttpServletResponse response) {
        Cookie cookie = new Cookie("refreshToken", null);
        cookie.setHttpOnly(cookieConfig.isHttpOnly());
        cookie.setSecure(cookieConfig.isSecure());
        cookie.setPath(cookieConfig.getPath());
        cookie.setMaxAge(0);
        
        if (!cookieConfig.getDomain().equals("localhost")) {
            cookie.setDomain(cookieConfig.getDomain());
        }

        response.addCookie(cookie);
    }

    private String getRefreshTokenFromCookie(HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("refreshToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }
        return null;
    }
}

