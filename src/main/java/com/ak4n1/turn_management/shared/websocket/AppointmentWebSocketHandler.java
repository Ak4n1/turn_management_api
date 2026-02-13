package com.ak4n1.turn_management.shared.websocket;

import com.ak4n1.turn_management.shared.websocket.dto.WebSocketMessage;
import com.ak4n1.turn_management.shared.websocket.util.WebSocketMessageType;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Handler principal para conexiones WebSocket de actualizaciones en tiempo real de turnos.
 * 
 * <p>Maneja conexiones, desconexiones, mensajes, heartbeat (ping/pong),
 * idle timeout y envío de actualizaciones a usuarios conectados.
 * 
 * @author ak4n1
 * @since 1.0
 */
@Component
public class AppointmentWebSocketHandler extends TextWebSocketHandler {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentWebSocketHandler.class);

    private final WebSocketSecurityValidator securityValidator;
    private final ObjectMapper objectMapper;

    // Mapa de sesiones activas: sessionId -> WebSocketSession de Spring
    private final Map<String, WebSocketSession> activeSessions = new ConcurrentHashMap<>();
    
    // Mapa de sesiones por usuario: userEmail -> Set de sessionIds
    private final Map<String, Set<String>> sessionsByUser = new ConcurrentHashMap<>();
    
    // Mapa de última actividad: sessionId -> timestamp
    private final Map<String, Long> lastActivity = new ConcurrentHashMap<>();
    
    // Mapa de tokens por sesión: sessionId -> token
    private final Map<String, String> tokensBySession = new ConcurrentHashMap<>();

    // Configuración
    private static final long HEARTBEAT_INTERVAL_MS = 30_000; // 30 segundos
    private static final long IDLE_TIMEOUT_MS = 30 * 60 * 1000; // 30 minutos
    private static final long IDLE_WARNING_MS = 25 * 60 * 1000; // 25 minutos (5 min antes)

    // Scheduler para heartbeat y cleanup
    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(2);

    public AppointmentWebSocketHandler(WebSocketSecurityValidator securityValidator,
                                      ObjectMapper objectMapper) {
        this.securityValidator = securityValidator;
        this.objectMapper = objectMapper;
        
        // Iniciar tareas programadas
        startHeartbeat();
        startIdleTimeoutChecker();
        startCleanupTask();
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        String userEmail = (String) session.getAttributes().get("userEmail");
        Long userId = (Long) session.getAttributes().get("userId");
        String token = (String) session.getAttributes().get("token");
        String sessionId = session.getId();

        logger.info("🔌 [WebSocket] Connection established - sessionId: {}, userEmail: {}, remoteAddress: {}", 
                sessionId, userEmail, session.getRemoteAddress());

        if (userEmail == null || userId == null) {
            logger.error("❌ [WebSocket] Session attributes missing, closing connection. sessionId: {}, userEmail: {}", 
                    sessionId, userEmail);
            session.close(CloseStatus.POLICY_VIOLATION);
            return;
        }

        // Registrar sesión
        activeSessions.put(sessionId, session);
        sessionsByUser.computeIfAbsent(userEmail, k -> new HashSet<>()).add(sessionId);
        lastActivity.put(sessionId, System.currentTimeMillis());
        if (token != null) {
            tokensBySession.put(sessionId, token);
        }

        // Incrementar contador de conexiones activas
        securityValidator.incrementActiveConnections(userEmail);

        logger.info("✅ [WebSocket] Connection registered - sessionId: {}, user: {}, totalActiveSessions: {}, userSessions: {}", 
                sessionId, userEmail, activeSessions.size(), sessionsByUser.get(userEmail).size());

        broadcastOnlineUsersCount();
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
        String sessionId = session.getId();
        String userEmail = (String) session.getAttributes().get("userEmail");

        logger.info("🔌 [WebSocket] Connection closing - sessionId: {}, user: {}, status: {} ({})", 
                sessionId, userEmail, status.getCode(), status.getReason());

        if (userEmail == null) {
            logger.warn("⚠️ [WebSocket] Session attributes missing on close - sessionId: {}, userEmail: {}", 
                    sessionId, userEmail);
            return;
        }

        // Limpiar sesión
        cleanupSession(sessionId, userEmail);

        logger.info("✅ [WebSocket] Connection closed and cleaned up - sessionId: {}, user: {}, remainingActiveSessions: {}",
                sessionId, userEmail, activeSessions.size());
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        String sessionId = session.getId();

        if (sessionId == null) {
            return;
        }

        // Actualizar última actividad
        lastActivity.put(sessionId, System.currentTimeMillis());

        try {
            WebSocketMessage wsMessage = objectMapper.readValue(message.getPayload(), WebSocketMessage.class);

            switch (wsMessage.getType()) {
                case PONG:
                    handlePong(sessionId);
                    break;
                case ACK:
                    handleAck(sessionId, wsMessage.getAppointmentId());
                    break;
                default:
                    logger.warn("Unknown message type received: {}", wsMessage.getType());
            }
        } catch (Exception e) {
            logger.error("Error processing WebSocket message: {}", e.getMessage(), e);
        }
    }

    @Override
    public void handleTransportError(WebSocketSession session, Throwable exception) throws Exception {
        String sessionId = session.getId();
        String userEmail = (String) session.getAttributes().get("userEmail");
        
        logger.error("WebSocket transport error: session={}, user={}, error={}", 
                    sessionId, userEmail, exception.getMessage(), exception);
        
        if (sessionId != null && userEmail != null) {
            cleanupSession(sessionId, userEmail);
        }
    }

    /**
     * Envía un mensaje a un usuario específico.
     * 
     * @param userEmail el email del usuario
     * @param message el mensaje a enviar
     */
    public void sendMessageToUser(String userEmail, WebSocketMessage message) {
        logger.info("📨 [WebSocket] Attempting to send message to user: {}", userEmail);
        
        Set<String> sessionIds = sessionsByUser.get(userEmail);
        if (sessionIds == null || sessionIds.isEmpty()) {
            logger.warn("❌ [WebSocket] No active sessions for user: {}", userEmail);
            return;
        }
        
        logger.info("✅ [WebSocket] Found {} active session(s) for user: {}", sessionIds.size(), userEmail);

        // Enviar a todas las sesiones del usuario
        int successCount = 0;
        for (String sessionId : sessionIds) {
            WebSocketSession wsSession = activeSessions.get(sessionId);
            if (wsSession != null && wsSession.isOpen()) {
                try {
                    String json = objectMapper.writeValueAsString(message);
                    wsSession.sendMessage(new TextMessage(json));
                    successCount++;
                    logger.info("✅ [WebSocket] Message sent successfully to session: {} (user: {})", 
                            sessionId, userEmail);
                } catch (Exception e) {
                    logger.error("❌ [WebSocket] Error sending message to session {} (user: {}): {}", 
                            sessionId, userEmail, e.getMessage(), e);
                    cleanupSession(sessionId, userEmail);
                }
            } else {
                logger.warn("⚠️ [WebSocket] Session {} not found or closed (user: {})", sessionId, userEmail);
                cleanupSession(sessionId, userEmail);
            }
        }
        
        logger.info("📊 [WebSocket] Message delivery summary - user: {}, totalSessions: {}, success: {}", 
                userEmail, sessionIds.size(), successCount);
    }

    /**
     * Envía un mensaje a todos los usuarios conectados (útil para admins).
     * 
     * @param message el mensaje a enviar
     */
    public void broadcastMessage(WebSocketMessage message) {
        logger.info("📢 [WebSocket] Broadcasting message to all {} active sessions", activeSessions.size());
        
        int successCount = 0;
        List<String> failedSessions = new ArrayList<>();
        
        for (Map.Entry<String, WebSocketSession> entry : activeSessions.entrySet()) {
            String sessionId = entry.getKey();
            WebSocketSession wsSession = entry.getValue();
            
            if (wsSession != null && wsSession.isOpen()) {
                try {
                    String json = objectMapper.writeValueAsString(message);
                    wsSession.sendMessage(new TextMessage(json));
                    successCount++;
                } catch (Exception e) {
                    logger.error("Error broadcasting to session {}: {}", sessionId, e.getMessage());
                    failedSessions.add(sessionId);
                }
            } else {
                failedSessions.add(sessionId);
            }
        }
        
        // Limpiar sesiones fallidas
        for (String sessionId : failedSessions) {
            String userEmail = sessionsByUser.entrySet().stream()
                .filter(e -> e.getValue().contains(sessionId))
                .map(Map.Entry::getKey)
                .findFirst()
                .orElse(null);
            if (userEmail != null) {
                cleanupSession(sessionId, userEmail);
            }
        }
        
        logger.info("📊 [WebSocket] Broadcast summary - total: {}, success: {}, failed: {}", 
                activeSessions.size(), successCount, failedSessions.size());
    }

    /**
     * Maneja mensaje PONG del cliente.
     */
    private void handlePong(String sessionId) {
        lastActivity.put(sessionId, System.currentTimeMillis());
        validateTokenIfNeeded(sessionId);
    }

    /**
     * Maneja ACK de mensaje.
     */
    private void handleAck(String sessionId, Long appointmentId) {
        logger.debug("ACK received for appointment {} from session {}", appointmentId, sessionId);
    }

    /**
     * Valida el token si está próximo a expirar.
     */
    private void validateTokenIfNeeded(String sessionId) {
        String token = tokensBySession.get(sessionId);
        if (token == null) {
            return;
        }

        try {
            securityValidator.validateToken(token);
        } catch (SecurityException e) {
            if (e.getMessage().contains("expired") || e.getMessage().contains("Invalid")) {
                sendTokenRefreshRequired(sessionId);
            }
        }
    }

    /**
     * Envía mensaje de requerimiento de refresh de token.
     */
    private void sendTokenRefreshRequired(String sessionId) {
        WebSocketSession wsSession = activeSessions.get(sessionId);
        if (wsSession == null || !wsSession.isOpen()) {
            return;
        }

        try {
            WebSocketMessage message = new WebSocketMessage();
            message.setType(WebSocketMessageType.TOKEN_REFRESH_REQUIRED);
            String json = objectMapper.writeValueAsString(message);
            wsSession.sendMessage(new TextMessage(json));
        } catch (Exception e) {
            logger.error("Error sending token refresh required: {}", e.getMessage());
        }
    }

    /**
     * Limpia una sesión (remueve de todos los mapas).
     */
    private void cleanupSession(String sessionId, String userEmail) {
        activeSessions.remove(sessionId);
        sessionsByUser.computeIfPresent(userEmail, (k, v) -> {
            v.remove(sessionId);
            return v.isEmpty() ? null : v;
        });
        lastActivity.remove(sessionId);
        tokensBySession.remove(sessionId);

        // Decrementar contador de conexiones activas
        securityValidator.decrementActiveConnections(userEmail);

        broadcastOnlineUsersCount();
    }

    /**
     * Retorna el número de usuarios únicos conectados vía WebSocket.
     */
    public int getOnlineUsersCount() {
        return sessionsByUser.size();
    }

    /**
     * Retorna los emails de usuarios actualmente conectados vía WebSocket.
     */
    public java.util.List<String> getOnlineUserEmails() {
        return new ArrayList<>(sessionsByUser.keySet());
    }

    /**
     * Emite el conteo y lista de usuarios en línea solo a sesiones con rol admin.
     */
    public void broadcastOnlineUsersCount() {
        WebSocketMessage message = new WebSocketMessage();
        message.setType(WebSocketMessageType.ONLINE_USERS_COUNT);
        message.setData(Map.of(
                "onlineUsersCount", getOnlineUsersCount(),
                "onlineUserEmails", getOnlineUserEmails()));
        broadcastToAdminsOnly(message);
    }

    /**
     * Envía un mensaje solo a las sesiones cuyo usuario tiene rol admin.
     */
    private void broadcastToAdminsOnly(WebSocketMessage message) {
        int successCount = 0;
        for (Map.Entry<String, WebSocketSession> entry : activeSessions.entrySet()) {
            Boolean isAdmin = (Boolean) entry.getValue().getAttributes().get("isAdmin");
            if (!Boolean.TRUE.equals(isAdmin)) {
                continue;
            }
            WebSocketSession wsSession = entry.getValue();
            if (wsSession != null && wsSession.isOpen()) {
                try {
                    String json = objectMapper.writeValueAsString(message);
                    wsSession.sendMessage(new TextMessage(json));
                    successCount++;
                } catch (Exception e) {
                    logger.error("Error sending to admin session {}: {}", entry.getKey(), e.getMessage());
                }
            }
        }
        logger.debug("📢 [WebSocket] ONLINE_USERS_COUNT sent to {} admin session(s)", successCount);
    }

    /**
     * Inicia el heartbeat (ping cada 30 segundos).
     */
    private void startHeartbeat() {
        scheduler.scheduleAtFixedRate(() -> {
            List<String> sessionsToClose = new ArrayList<>();

            for (Map.Entry<String, WebSocketSession> entry : activeSessions.entrySet()) {
                String sessionId = entry.getKey();
                WebSocketSession wsSession = entry.getValue();

                if (!wsSession.isOpen()) {
                    sessionsToClose.add(sessionId);
                    continue;
                }

                try {
                    WebSocketMessage ping = new WebSocketMessage();
                    ping.setType(WebSocketMessageType.PING);
                    String json = objectMapper.writeValueAsString(ping);
                    wsSession.sendMessage(new TextMessage(json));
                } catch (Exception e) {
                    logger.error("Error sending ping to session {}: {}", sessionId, e.getMessage());
                    sessionsToClose.add(sessionId);
                }
            }

            // Cerrar sesiones con errores
            for (String sessionId : sessionsToClose) {
                String userEmail = sessionsByUser.entrySet().stream()
                    .filter(e -> e.getValue().contains(sessionId))
                    .map(Map.Entry::getKey)
                    .findFirst()
                    .orElse(null);
                if (userEmail != null) {
                    cleanupSession(sessionId, userEmail);
                }
            }
        }, HEARTBEAT_INTERVAL_MS, HEARTBEAT_INTERVAL_MS, TimeUnit.MILLISECONDS);
    }

    /**
     * Inicia el checker de idle timeout.
     */
    private void startIdleTimeoutChecker() {
        scheduler.scheduleAtFixedRate(() -> {
            long currentTime = System.currentTimeMillis();
            List<String> sessionsToClose = new ArrayList<>();

            for (Map.Entry<String, Long> entry : lastActivity.entrySet()) {
                String sessionId = entry.getKey();
                long lastActivityTime = entry.getValue();
                long idleTime = currentTime - lastActivityTime;

                // Advertencia 5 minutos antes
                if (idleTime >= IDLE_WARNING_MS && idleTime < IDLE_TIMEOUT_MS) {
                    sendIdleTimeoutWarning(sessionId);
                }

                // Cerrar si excede timeout
                if (idleTime >= IDLE_TIMEOUT_MS) {
                    sessionsToClose.add(sessionId);
                }
            }

            // Cerrar sesiones inactivas
            for (String sessionId : sessionsToClose) {
                String userEmail = sessionsByUser.entrySet().stream()
                    .filter(e -> e.getValue().contains(sessionId))
                    .map(Map.Entry::getKey)
                    .findFirst()
                    .orElse(null);
                if (userEmail != null) {
                    closeSessionWithIdleTimeout(sessionId, userEmail);
                }
            }
        }, 60_000, 60_000, TimeUnit.MILLISECONDS); // Check cada minuto
    }

    /**
     * Envía advertencia de idle timeout.
     */
    private void sendIdleTimeoutWarning(String sessionId) {
        WebSocketSession wsSession = activeSessions.get(sessionId);
        if (wsSession == null || !wsSession.isOpen()) {
            return;
        }

        try {
            WebSocketMessage message = new WebSocketMessage();
            message.setType(WebSocketMessageType.IDLE_TIMEOUT);
            String json = objectMapper.writeValueAsString(message);
            wsSession.sendMessage(new TextMessage(json));
        } catch (Exception e) {
            logger.error("Error sending idle timeout warning: {}", e.getMessage());
        }
    }

    /**
     * Cierra sesión por idle timeout.
     */
    private void closeSessionWithIdleTimeout(String sessionId, String userEmail) {
        WebSocketSession wsSession = activeSessions.get(sessionId);
        if (wsSession != null && wsSession.isOpen()) {
            try {
                wsSession.close(CloseStatus.GOING_AWAY);
            } catch (IOException e) {
                logger.error("Error closing idle session: {}", e.getMessage());
            }
        }
        cleanupSession(sessionId, userEmail);
    }

    /**
     * Inicia tarea de limpieza periódica.
     */
    private void startCleanupTask() {
        scheduler.scheduleAtFixedRate(() -> {
            securityValidator.cleanupExpiredEntries();
        }, 5 * 60_000, 5 * 60_000, TimeUnit.MILLISECONDS); // Cada 5 minutos
    }

    /**
     * Cierra todas las conexiones (para graceful shutdown).
     */
    public void closeAllConnections() {
        for (Map.Entry<String, WebSocketSession> entry : activeSessions.entrySet()) {
            try {
                WebSocketMessage message = new WebSocketMessage();
                message.setType(WebSocketMessageType.SERVER_SHUTDOWN);
                message.setReconnectInSeconds(30);
                String json = objectMapper.writeValueAsString(message);
                entry.getValue().sendMessage(new TextMessage(json));
                Thread.sleep(100); // Pequeña pausa entre mensajes
            } catch (Exception e) {
                logger.error("Error sending shutdown message: {}", e.getMessage());
            }
        }

        // Cerrar todas las conexiones después de 2 segundos
        scheduler.schedule(() -> {
            for (WebSocketSession wsSession : activeSessions.values()) {
                try {
                    if (wsSession.isOpen()) {
                        wsSession.close(CloseStatus.SERVER_ERROR);
                    }
                } catch (Exception e) {
                    logger.error("Error closing session during shutdown: {}", e.getMessage());
                }
            }
        }, 2, TimeUnit.SECONDS);
    }
}

