package com.ak4n1.turn_management.shared.security.rateLimit;

import com.ak4n1.turn_management.shared.security.rateLimit.strategies.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class RateLimitStrategyFactory {

    private final IpBasedRateLimitStrategy ipBasedStrategy;
    private final UserBasedRateLimitStrategy userBasedStrategy;
    private final CombinedRateLimitStrategy combinedStrategy;
    private final DevRateLimitStrategy devStrategy;

    @Value("${rate-limit.environment:dev}")
    private String environment;

    public RateLimitStrategyFactory(
            IpBasedRateLimitStrategy ipBasedStrategy,
            UserBasedRateLimitStrategy userBasedStrategy,
            CombinedRateLimitStrategy combinedStrategy,
            DevRateLimitStrategy devStrategy) {
        this.ipBasedStrategy = ipBasedStrategy;
        this.userBasedStrategy = userBasedStrategy;
        this.combinedStrategy = combinedStrategy;
        this.devStrategy = devStrategy;
    }

    public RateLimitStrategy createStrategy(RateLimitType type) {
        // In dev environment, use dev strategy if explicitly requested
        if (type == RateLimitType.DEV || ("dev".equals(environment) && type == null)) {
            return devStrategy;
        }

        return switch (type) {
            case IP_BASED -> ipBasedStrategy;
            case USER_BASED -> userBasedStrategy;
            case COMBINED -> combinedStrategy;
            case DEV -> devStrategy;
            default -> {
                // Default based on environment
                if ("dev".equals(environment)) {
                    yield devStrategy;
                } else {
                    yield combinedStrategy; // Most secure for production
                }
            }
        };
    }

    public RateLimitStrategy createStrategyForEndpoint(String endpoint) {
        // Determine strategy based on endpoint
        if (endpoint.contains("/auth/login") || endpoint.contains("/auth/forgot-password")) {
            return createStrategy(RateLimitType.COMBINED); // Most restrictive for auth endpoints
        } else if (endpoint.contains("/auth/refresh")) {
            return createStrategy(RateLimitType.USER_BASED); // User-based for refresh
        } else if ("dev".equals(environment)) {
            return createStrategy(RateLimitType.DEV);
        } else {
            return createStrategy(RateLimitType.IP_BASED); // IP-based for public endpoints
        }
    }
}

