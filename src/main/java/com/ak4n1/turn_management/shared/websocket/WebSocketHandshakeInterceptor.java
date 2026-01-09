package com.ak4n1.turn_management.shared.websocket;

import com.ak4n1.turn_management.feature.auth.domain.User;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.HandshakeInterceptor;

import java.util.Map;

/**
 * Interceptor para el handshake de WebSocket.
 * 
 * <p>Valida el token JWT desde cookies o query parameter, el origen, aplica rate limiting
 * y almacena información del usuario en los attributes antes de aceptar la conexión.
 * 
 * @author ak4n1
 * @since 1.0
 */
@Component
public class WebSocketHandshakeInterceptor implements HandshakeInterceptor {

    private static final Logger logger = LoggerFactory.getLogger(WebSocketHandshakeInterceptor.class);

    private final WebSocketSecurityValidator securityValidator;

    public WebSocketHandshakeInterceptor(WebSocketSecurityValidator securityValidator) {
        this.securityValidator = securityValidator;
    }

    @Override
    public boolean beforeHandshake(ServerHttpRequest request, ServerHttpResponse response,
                                   WebSocketHandler wsHandler, Map<String, Object> attributes) throws Exception {
        
        logger.info("🔌 [WebSocket] Handshake attempt - URI: {}, Method: {}", request.getURI(), request.getMethod());
        
        if (!(request instanceof ServletServerHttpRequest)) {
            logger.error("❌ [WebSocket] Invalid request type for WebSocket handshake");
            return false;
        }

        ServletServerHttpRequest servletRequest = (ServletServerHttpRequest) request;
        HttpServletRequest httpRequest = servletRequest.getServletRequest();
        
        String origin = httpRequest.getHeader("Origin");
        String userAgent = httpRequest.getHeader("User-Agent");
        String ipAddress = getClientIpAddress(httpRequest);
        
        logger.info("🔌 [WebSocket] Request details - Origin: {}, IP: {}, User-Agent: {}", 
                origin, ipAddress, userAgent);
        
        // Log todas las cookies recibidas
        if (httpRequest.getCookies() != null) {
            logger.info("🍪 [WebSocket] Cookies received: {} cookies", httpRequest.getCookies().length);
            for (jakarta.servlet.http.Cookie cookie : httpRequest.getCookies()) {
                logger.debug("🍪 [WebSocket] Cookie: {} = {} (httpOnly: {}, secure: {})", 
                        cookie.getName(), 
                        cookie.getValue() != null && cookie.getValue().length() > 20 
                            ? cookie.getValue().substring(0, 20) + "..." 
                            : cookie.getValue(),
                        cookie.isHttpOnly(),
                        cookie.getSecure());
            }
        } else {
            logger.warn("🍪 [WebSocket] No cookies received in request");
        }

        // 1. Extraer token del query parameter o de cookies
        String token = httpRequest.getParameter("token");
        logger.info("🔑 [WebSocket] Token from query parameter: {}", token != null ? "present" : "not present");
        
        // Si no está en query parameter, intentar leer de cookies
        if (token == null || token.isBlank()) {
            if (httpRequest.getCookies() != null) {
                for (jakarta.servlet.http.Cookie cookie : httpRequest.getCookies()) {
                    if ("accessToken".equals(cookie.getName())) {
                        token = cookie.getValue();
                        logger.info("🔑 [WebSocket] Token found in cookies: {}", token != null && token.length() > 20 
                                ? token.substring(0, 20) + "..." 
                                : token);
                        break;
                    }
                }
            }
        }
        
        if (token == null || token.isBlank()) {
            logger.warn("❌ [WebSocket] Handshake rejected: No token provided (checked query param and cookies)");
            return false;
        }

        // 2. Validar origen
        if (!securityValidator.validateOrigin(origin)) {
            logger.warn("❌ [WebSocket] Handshake rejected: Invalid origin: {}", origin);
            return false;
        }
        logger.info("✅ [WebSocket] Origin validated: {}", origin);

        // 3. Validar rate limiting por IP
        if (!securityValidator.validateRateLimitByIp(ipAddress)) {
            logger.warn("❌ [WebSocket] Handshake rejected: Rate limit exceeded for IP: {}", ipAddress);
            return false;
        }
        logger.info("✅ [WebSocket] Rate limit check passed for IP: {}", ipAddress);

        // 4. Validar token y obtener usuario
        User user;
        try {
            logger.info("🔑 [WebSocket] Validating token...");
            user = securityValidator.validateToken(token);
            logger.info("✅ [WebSocket] Token validated successfully for user: {}", user.getEmail());
        } catch (SecurityException e) {
            logger.warn("❌ [WebSocket] Handshake rejected: Token validation failed: {}", e.getMessage());
            return false;
        }

        // 5. Validar rate limiting por usuario
        if (!securityValidator.validateRateLimitByUser(user.getEmail())) {
            logger.warn("❌ [WebSocket] Handshake rejected: Max connections exceeded for user: {}", user.getEmail());
            return false;
        }
        logger.info("✅ [WebSocket] Rate limit check passed for user: {}", user.getEmail());

        // 6. Almacenar información en attributes para el handler
        attributes.put("userEmail", user.getEmail());
        attributes.put("userId", user.getId());
        attributes.put("token", token);
        attributes.put("ipAddress", ipAddress);

        logger.info("✅ [WebSocket] Handshake ACCEPTED for user: {}", user.getEmail());
        return true;
    }

    @Override
    public void afterHandshake(ServerHttpRequest request, ServerHttpResponse response,
                              WebSocketHandler wsHandler, Exception exception) {
        if (exception != null) {
            logger.error("Error during WebSocket handshake: {}", exception.getMessage(), exception);
        }
    }

    /**
     * Obtiene la dirección IP real del cliente, considerando proxies.
     * 
     * @param request HttpServletRequest
     * @return la dirección IP del cliente
     */
    private String getClientIpAddress(HttpServletRequest request) {
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
            // Tomar la primera IP (la del cliente original)
            return xForwardedFor.split(",")[0].trim();
        }

        String xRealIp = request.getHeader("X-Real-IP");
        if (xRealIp != null && !xRealIp.isEmpty()) {
            return xRealIp;
        }

        return request.getRemoteAddr();
    }
}

