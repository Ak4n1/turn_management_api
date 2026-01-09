package com.ak4n1.turn_management.feature.auth.dto.response;

public class ForgotPasswordResponse {

    private int status;
    private String message;

    public ForgotPasswordResponse() {
    }

    public ForgotPasswordResponse(int status, String message) {
        this.status = status;
        this.message = message;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}

