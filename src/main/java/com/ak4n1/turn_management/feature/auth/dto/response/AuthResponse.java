package com.ak4n1.turn_management.feature.auth.dto.response;

public class AuthResponse {

    private UserResponse user;

    public AuthResponse() {
    }

    public AuthResponse(UserResponse user) {
        this.user = user;
    }

    public UserResponse getUser() {
        return user;
    }

    public void setUser(UserResponse user) {
        this.user = user;
    }
}

