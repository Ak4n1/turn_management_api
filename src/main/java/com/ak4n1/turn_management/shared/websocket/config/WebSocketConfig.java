package com.ak4n1.turn_management.shared.websocket.config;

import com.ak4n1.turn_management.shared.websocket.AppointmentWebSocketHandler;
import com.ak4n1.turn_management.shared.websocket.WebSocketHandshakeInterceptor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

import java.util.Arrays;
import java.util.List;

/**
 * Configuración de WebSocket para actualizaciones en tiempo real de turnos.
 * 
 * <p>Configura el endpoint WebSocket en /api/ws/appointments y registra
 * el handler y el interceptor de handshake.
 * 
 * @author ak4n1
 * @since 1.0
 */
@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {

    private final AppointmentWebSocketHandler webSocketHandler;
    private final WebSocketHandshakeInterceptor handshakeInterceptor;

    @Value("${security.allowed-origins}")
    private String allowedOrigins;

    public WebSocketConfig(AppointmentWebSocketHandler webSocketHandler,
                          WebSocketHandshakeInterceptor handshakeInterceptor) {
        this.webSocketHandler = webSocketHandler;
        this.handshakeInterceptor = handshakeInterceptor;
    }

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        List<String> origins = Arrays.asList(allowedOrigins.split(","));
        
        registry.addHandler(webSocketHandler, "/api/ws/appointments")
                .addInterceptors(handshakeInterceptor)
                .setAllowedOrigins(origins.toArray(new String[0]));
    }
}

