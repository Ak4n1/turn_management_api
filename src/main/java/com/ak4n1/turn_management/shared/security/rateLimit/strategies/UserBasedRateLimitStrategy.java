package com.ak4n1.turn_management.shared.security.rateLimit.strategies;

import com.ak4n1.turn_management.shared.security.rateLimit.RateLimitStrategy;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class UserBasedRateLimitStrategy implements RateLimitStrategy {

    @Override
    public String getKey(HttpServletRequest request) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated() || "anonymousUser".equals(authentication.getPrincipal())) {
            return null; // No user authenticated
        }
        String username = authentication.getName();
        String endpoint = request.getRequestURI();
        return "rate_limit:user:" + username + ":" + endpoint;
    }

    @Override
    public boolean isAllowed(String key, int maxRequests, long windowSeconds) {
        if (key == null) {
            return true; // No user, skip rate limiting
        }
        // This method is not used directly - RateLimitService handles the actual limiting
        return true;
    }
}

