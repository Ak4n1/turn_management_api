package com.ak4n1.turn_management.shared.security.rateLimit;

import jakarta.servlet.http.HttpServletRequest;

public interface RateLimitStrategy {

    String getKey(HttpServletRequest request);

    boolean isAllowed(String key, int maxRequests, long windowSeconds);
}

