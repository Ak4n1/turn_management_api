package com.ak4n1.turn_management.shared.websocket.util;

/**
 * Tipos de mensajes WebSocket.
 * 
 * <p>Define todos los tipos de mensajes que se pueden enviar y recibir
 * a través de WebSocket entre el cliente y el servidor.
 * 
 * @author ak4n1
 * @since 1.0
 */
public enum WebSocketMessageType {
    // Mensajes del servidor al cliente
    APPOINTMENT_CREATED,        // Nuevo turno creado
    APPOINTMENT_CONFIRMED,      // Turno confirmado
    APPOINTMENT_CANCELLED,      // Turno cancelado
    APPOINTMENT_RESCHEDULED,    // Turno reprogramado
    APPOINTMENT_EXPIRED,        // Turno expirado
    AVAILABILITY_UPDATED,       // Disponibilidad actualizada
    NOTIFICATION_COUNT_UPDATED, // Contador de notificaciones no leídas actualizado
    ONLINE_USERS_COUNT,         // Usuarios conectados vía WebSocket (para admins)
    RESCHEDULE_REQUEST_CREATED,  // Solicitud de reprogramación creada
    RESCHEDULE_REQUEST_APPROVED, // Solicitud de reprogramación aprobada
    RESCHEDULE_REQUEST_REJECTED, // Solicitud de reprogramación rechazada
    PING,                       // Heartbeat ping
    TOKEN_REFRESH_REQUIRED,     // Token próximo a expirar
    IDLE_TIMEOUT,               // Advertencia de timeout inactivo
    SERVER_SHUTDOWN,            // Servidor se está cerrando
    
    // Mensajes del cliente al servidor
    PONG,                       // Respuesta a ping
    ACK                         // Acknowledgment de mensaje recibido
}

