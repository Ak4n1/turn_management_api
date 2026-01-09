package com.ak4n1.turn_management.shared.security.rateLimit.strategies;

import com.ak4n1.turn_management.shared.security.rateLimit.RateLimitStrategy;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class DevRateLimitStrategy implements RateLimitStrategy {

    @Value("${rate-limit.enabled:true}")
    private boolean rateLimitEnabled;

    @Value("${rate-limit.environment:dev}")
    private String environment;

    @Override
    public String getKey(HttpServletRequest request) {
        return "rate_limit:dev:" + request.getRequestURI();
    }

    @Override
    public boolean isAllowed(String key, int maxRequests, long windowSeconds) {
        // In dev mode, if rate limiting is disabled, always allow
        if ("dev".equals(environment) && !rateLimitEnabled) {
            return true;
        }
        // Otherwise, use very permissive limits
        return true;
    }
}

