package com.ak4n1.turn_management.shared.security.rateLimit;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Component
public class RateLimitFilter extends OncePerRequestFilter {

    private final RateLimitStrategyFactory strategyFactory;
    private final RateLimitService rateLimitService;
    private final ObjectMapper objectMapper;

    @Value("${rate-limit.login.max-requests:5}")
    private int loginMaxRequests;

    @Value("${rate-limit.login.window-seconds:60}")
    private long loginWindowSeconds;

    @Value("${rate-limit.refresh.max-requests:10}")
    private int refreshMaxRequests;

    @Value("${rate-limit.refresh.window-seconds:60}")
    private long refreshWindowSeconds;

    @Value("${rate-limit.forgot-password.max-requests:3}")
    private int forgotPasswordMaxRequests;

    @Value("${rate-limit.forgot-password.window-seconds:3600}")
    private long forgotPasswordWindowSeconds;

    @Value("${rate-limit.public.max-requests:100}")
    private int publicMaxRequests;

    @Value("${rate-limit.public.window-seconds:60}")
    private long publicWindowSeconds;

    @Value("${rate-limit.authenticated.max-requests:200}")
    private int authenticatedMaxRequests;

    @Value("${rate-limit.authenticated.window-seconds:60}")
    private long authenticatedWindowSeconds;

    public RateLimitFilter(RateLimitStrategyFactory strategyFactory, RateLimitService rateLimitService, ObjectMapper objectMapper) {
        this.strategyFactory = strategyFactory;
        this.rateLimitService = rateLimitService;
        this.objectMapper = objectMapper;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        String endpoint = request.getRequestURI();
        RateLimitStrategy strategy = strategyFactory.createStrategyForEndpoint(endpoint);
        String key = strategy.getKey(request);

        if (key == null) {
            filterChain.doFilter(request, response);
            return;
        }

        int maxRequests;
        long windowSeconds;

        // Determine limits based on endpoint
        if (endpoint.contains("/auth/login")) {
            maxRequests = loginMaxRequests;
            windowSeconds = loginWindowSeconds;
        } else if (endpoint.contains("/auth/refresh")) {
            maxRequests = refreshMaxRequests;
            windowSeconds = refreshWindowSeconds;
        } else if (endpoint.contains("/auth/forgot-password")) {
            maxRequests = forgotPasswordMaxRequests;
            windowSeconds = forgotPasswordWindowSeconds;
        } else if (endpoint.startsWith("/api/")) {
            maxRequests = authenticatedMaxRequests;
            windowSeconds = authenticatedWindowSeconds;
        } else {
            maxRequests = publicMaxRequests;
            windowSeconds = publicWindowSeconds;
        }

        if (!rateLimitService.isAllowed(key, maxRequests, windowSeconds)) {
            handleRateLimitExceeded(response);
            return;
        }

        filterChain.doFilter(request, response);
    }

    private void handleRateLimitExceeded(HttpServletResponse response) throws IOException {
        response.setStatus(HttpStatus.TOO_MANY_REQUESTS.value());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);

        Map<String, Object> error = new HashMap<>();
        error.put("status", HttpStatus.TOO_MANY_REQUESTS.value());
        error.put("error", "Too Many Requests");
        error.put("message", "Rate limit exceeded. Please try again later.");

        response.getWriter().write(objectMapper.writeValueAsString(error));
    }
}

