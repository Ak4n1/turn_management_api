package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.domain.EmailVerificationToken;
import com.ak4n1.turn_management.feature.notification.repository.EmailVerificationTokenRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

@Service
public class EmailVerificationServiceImpl implements EmailVerificationService {

    private static final int TOKEN_EXPIRATION_MINUTES = 15;

    private final EmailVerificationTokenRepository tokenRepository;
    private final UserRepository userRepository;
    private final EmailService emailService;

    public EmailVerificationServiceImpl(
            EmailVerificationTokenRepository tokenRepository,
            UserRepository userRepository,
            EmailService emailService) {
        this.tokenRepository = tokenRepository;
        this.userRepository = userRepository;
        this.emailService = emailService;
    }

    @Override
    @Transactional
    public String generateVerificationToken(User user) {
        // Check if there's already a valid (non-expired and unused) token for this user
        // This prevents spam/abuse of the resend verification endpoint
        Optional<EmailVerificationToken> existingToken = tokenRepository.findByUser(user);
        if (existingToken.isPresent() && existingToken.get().isValid()) {
            EmailVerificationToken token = existingToken.get();
            String timeRemaining = formatTimeRemaining(token.getExpiresAt());
            throw new IllegalStateException("Ya se ha enviado un token de verificación. Por favor, revisa tu email o espera " + timeRemaining + " antes de solicitar uno nuevo.");
        }

        // Delete existing token if any (expired or used tokens can be deleted)
        // This ensures we only have one token per user at a time
        if (existingToken.isPresent()) {
            tokenRepository.delete(existingToken.get());
            tokenRepository.flush();
        }

        // Generate new token
        String token = UUID.randomUUID().toString();
        LocalDateTime expiresAt = LocalDateTime.now().plusMinutes(TOKEN_EXPIRATION_MINUTES);

        EmailVerificationToken verificationToken = new EmailVerificationToken(token, user, expiresAt);
        tokenRepository.save(verificationToken);

        return token;
    }

    @Override
    @Transactional
    public boolean verifyEmail(String token) {
        EmailVerificationToken verificationToken = tokenRepository.findByToken(token)
                .orElse(null);

        if (verificationToken == null) {
            return false;
        }

        if (!verificationToken.isValid()) {
            return false;
        }

        User user = verificationToken.getUser();
        user.setEmailVerified(true);
        userRepository.save(user);

        verificationToken.setUsed(true);
        verificationToken.setUsedAt(LocalDateTime.now());
        tokenRepository.save(verificationToken);

        return true;
    }

    @Override
    @Transactional
    public void resendVerificationEmail(User user) {
        if (user.getEmailVerified()) {
            throw new IllegalStateException("El email ya está verificado");
        }

        String token = generateVerificationToken(user);
        emailService.sendEmailVerification(user, token);
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
}

