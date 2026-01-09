package com.ak4n1.turn_management.feature.auth.service;

import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;

public class AuthResult {
    private final String accessToken;
    private final String refreshToken;
    private final Long expiresIn;
    private final UserResponse user;

    public AuthResult(String accessToken, String refreshToken, Long expiresIn, UserResponse user) {
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.expiresIn = expiresIn;
        this.user = user;
    }

    public String getAccessToken() {
        return accessToken;
    }

    public String getRefreshToken() {
        return refreshToken;
    }

    public Long getExpiresIn() {
        return expiresIn;
    }

    public UserResponse getUser() {
        return user;
    }
}

