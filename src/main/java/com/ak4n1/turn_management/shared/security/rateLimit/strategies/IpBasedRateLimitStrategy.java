package com.ak4n1.turn_management.shared.security.rateLimit.strategies;

import com.ak4n1.turn_management.shared.security.rateLimit.RateLimitStrategy;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.stereotype.Component;

@Component
public class IpBasedRateLimitStrategy implements RateLimitStrategy {

    @Override
    public String getKey(HttpServletRequest request) {
        String ip = getClientIpAddress(request);
        String endpoint = request.getRequestURI();
        return "rate_limit:ip:" + ip + ":" + endpoint;
    }

    @Override
    public boolean isAllowed(String key, int maxRequests, long windowSeconds) {
        // This method is not used directly - RateLimitService handles the actual limiting
        return true;
    }

    private String getClientIpAddress(HttpServletRequest request) {
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
            return xForwardedFor.split(",")[0].trim();
        }
        String xRealIp = request.getHeader("X-Real-IP");
        if (xRealIp != null && !xRealIp.isEmpty()) {
            return xRealIp;
        }
        return request.getRemoteAddr();
    }
}

