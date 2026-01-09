package com.ak4n1.turn_management.shared.websocket.controller;

import com.ak4n1.turn_management.shared.websocket.AppointmentWebSocketHandler;
import com.ak4n1.turn_management.shared.websocket.dto.WebSocketMessage;
import com.ak4n1.turn_management.shared.websocket.util.WebSocketMessageType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Controller de testeo para WebSocket.
 * 
 * <p>Endpoints para probar el funcionamiento de WebSocket.
 * Solo disponible en desarrollo.
 * 
 * @author ak4n1
 * @since 1.0
 */
@RestController
@RequestMapping("/api/test/websocket")
public class WebSocketTestController {

    private final AppointmentWebSocketHandler webSocketHandler;

    public WebSocketTestController(AppointmentWebSocketHandler webSocketHandler) {
        this.webSocketHandler = webSocketHandler;
    }

    /**
     * Envía un mensaje de test a un usuario específico.
     * 
     * @param userEmail email del usuario destinatario
     * @param messageType tipo de mensaje (opcional, default: APPOINTMENT_CREATED)
     * @return respuesta con el resultado
     */
    @PostMapping("/send-to-user")
    public ResponseEntity<Map<String, Object>> sendToUser(
            @RequestParam String userEmail,
            @RequestParam(required = false, defaultValue = "APPOINTMENT_CREATED") String messageType) {
        
        WebSocketMessage message = new WebSocketMessage();
        
        try {
            WebSocketMessageType type = WebSocketMessageType.valueOf(messageType);
            message.setType(type);
        } catch (IllegalArgumentException e) {
            message.setType(WebSocketMessageType.APPOINTMENT_CREATED);
        }
        
        message.setTitle("Mensaje de Test");
        message.setMessage("Este es un mensaje de prueba enviado desde el endpoint de testeo");
        
        Map<String, Object> data = new HashMap<>();
        data.put("testId", System.currentTimeMillis());
        data.put("source", "test-endpoint");
        data.put("messageType", messageType);
        message.setData(data);
        
        webSocketHandler.sendMessageToUser(userEmail, message);
        
        Map<String, Object> response = new HashMap<>();
        response.put("success", true);
        response.put("message", "Mensaje enviado a usuario: " + userEmail);
        response.put("messageType", message.getType().name());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Envía un mensaje de test a todos los usuarios conectados (broadcast).
     * 
     * @param messageType tipo de mensaje (opcional, default: APPOINTMENT_CREATED)
     * @return respuesta con el resultado
     */
    @PostMapping("/broadcast")
    public ResponseEntity<Map<String, Object>> broadcast(
            @RequestParam(required = false, defaultValue = "APPOINTMENT_CREATED") String messageType) {
        
        WebSocketMessage message = new WebSocketMessage();
        
        try {
            WebSocketMessageType type = WebSocketMessageType.valueOf(messageType);
            message.setType(type);
        } catch (IllegalArgumentException e) {
            message.setType(WebSocketMessageType.APPOINTMENT_CREATED);
        }
        
        message.setTitle("Broadcast de Test");
        message.setMessage("Este es un mensaje de broadcast enviado desde el endpoint de testeo");
        
        Map<String, Object> data = new HashMap<>();
        data.put("testId", System.currentTimeMillis());
        data.put("source", "test-endpoint");
        data.put("messageType", messageType);
        message.setData(data);
        
        webSocketHandler.broadcastMessage(message);
        
        Map<String, Object> response = new HashMap<>();
        response.put("success", true);
        response.put("message", "Mensaje broadcast enviado a todos los usuarios conectados");
        response.put("messageType", message.getType().name());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Envía un mensaje de test con datos de turno simulado.
     * 
     * @param userEmail email del usuario destinatario
     * @return respuesta con el resultado
     */
    @PostMapping("/send-appointment-test")
    public ResponseEntity<Map<String, Object>> sendAppointmentTest(@RequestParam String userEmail) {
        
        WebSocketMessage message = new WebSocketMessage();
        message.setType(WebSocketMessageType.APPOINTMENT_CREATED);
        message.setTitle("Nuevo Turno Creado");
        message.setMessage("Se ha creado un nuevo turno para el día 25 de abril a las 14:00");
        
        Map<String, Object> data = new HashMap<>();
        data.put("appointmentId", 123L);
        data.put("date", "2024-04-25");
        data.put("startTime", "14:00");
        data.put("duration", 30);
        data.put("state", "CREATED");
        message.setData(data);
        message.setAppointmentId(123L);
        
        webSocketHandler.sendMessageToUser(userEmail, message);
        
        Map<String, Object> response = new HashMap<>();
        response.put("success", true);
        response.put("message", "Mensaje de turno simulado enviado a: " + userEmail);
        
        return ResponseEntity.ok(response);
    }
}

