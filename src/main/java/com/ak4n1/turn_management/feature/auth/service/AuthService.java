package com.ak4n1.turn_management.feature.auth.service;

import com.ak4n1.turn_management.feature.auth.dto.request.*;
import com.ak4n1.turn_management.feature.auth.dto.response.ForgotPasswordResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.ResetPasswordResponse;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;

public interface AuthService {

    AuthResult login(LoginRequest request);

    UserResponse register(RegisterRequest request);

    AuthResult refreshToken(RefreshTokenRequest request);

    void logout(String refreshToken);

    ForgotPasswordResponse forgotPassword(ForgotPasswordRequest request);

    ResetPasswordResponse resetPassword(String token, ResetPasswordRequest request);

    void resendVerificationEmail(String email);
}

