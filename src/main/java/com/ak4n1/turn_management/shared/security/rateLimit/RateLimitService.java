package com.ak4n1.turn_management.shared.security.rateLimit;

import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class RateLimitService {

    @Value("${rate-limit.enabled:true}")
    private boolean rateLimitEnabled;

    @Value("${rate-limit.environment:dev}")
    private String environment;

    private final Map<String, Bucket> buckets = new ConcurrentHashMap<>();

    @PostConstruct
    public void init() {
        // Cleanup expired buckets periodically
        // This is a simple implementation - in production, consider using Redis
    }

    public boolean isAllowed(String key, int maxRequests, long windowSeconds) {
        if (!rateLimitEnabled && "dev".equals(environment)) {
            return true;
        }

        Bucket bucket = buckets.computeIfAbsent(key, k -> createBucket(maxRequests, windowSeconds));
        return bucket.tryConsume(1);
    }

    private Bucket createBucket(int maxRequests, long windowSeconds) {
        Bandwidth limit = Bandwidth.builder()
                .capacity(maxRequests)
                .refillIntervally(maxRequests, Duration.ofSeconds(windowSeconds))
                .build();
        return Bucket.builder()
                .addLimit(limit)
                .build();
    }

    public void resetBucket(String key) {
        buckets.remove(key);
    }
}

