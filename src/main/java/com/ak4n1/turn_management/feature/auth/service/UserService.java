package com.ak4n1.turn_management.feature.auth.service;

import com.ak4n1.turn_management.feature.auth.domain.User;

import java.util.Optional;

public interface UserService {

    Optional<User> findByEmail(String email);

    Optional<User> findById(Long id);

    User save(User user);

    boolean existsByEmail(String email);

    void incrementFailedLoginAttempts(User user);

    void resetFailedLoginAttempts(User user);

    void lockAccount(User user, int lockoutDurationMinutes);

    boolean isAccountLocked(User user);
}

