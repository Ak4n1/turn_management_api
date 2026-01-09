package com.ak4n1.turn_management.shared.security.rateLimit.strategies;

import com.ak4n1.turn_management.shared.security.rateLimit.RateLimitStrategy;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class CombinedRateLimitStrategy implements RateLimitStrategy {

    private final IpBasedRateLimitStrategy ipStrategy;

    public CombinedRateLimitStrategy(IpBasedRateLimitStrategy ipStrategy) {
        this.ipStrategy = ipStrategy;
    }

    @Override
    public String getKey(HttpServletRequest request) {
        String ipKey = ipStrategy.getKey(request);
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        
        if (authentication != null && authentication.isAuthenticated() && !"anonymousUser".equals(authentication.getPrincipal())) {
            String username = authentication.getName();
            String endpoint = request.getRequestURI();
            return "rate_limit:combined:" + username + ":" + ipKey.split(":")[2] + ":" + endpoint;
        }
        
        return ipKey;
    }

    @Override
    public boolean isAllowed(String key, int maxRequests, long windowSeconds) {
        // This method is not used directly - RateLimitService handles the actual limiting
        return true;
    }
}

