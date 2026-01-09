package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.auth.domain.User;

public interface EmailVerificationService {

    String generateVerificationToken(User user);

    boolean verifyEmail(String token);

    void resendVerificationEmail(User user);
}

