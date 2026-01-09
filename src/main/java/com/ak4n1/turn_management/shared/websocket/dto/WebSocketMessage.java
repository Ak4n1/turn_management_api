package com.ak4n1.turn_management.shared.websocket.dto;

import com.ak4n1.turn_management.shared.websocket.util.WebSocketMessageType;
import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.Map;

/**
 * DTO para mensajes WebSocket.
 * 
 * <p>Representa los mensajes que se envían y reciben a través de WebSocket.
 * El campo type determina el tipo de mensaje y qué otros campos están presentes.
 * 
 * @author ak4n1
 * @since 1.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WebSocketMessage {

    private WebSocketMessageType type;
    private Long appointmentId;
    private Long rescheduleRequestId;
    private String title;
    private String message;
    private Map<String, Object> data;
    private Long timestamp;
    private Integer reconnectInSeconds;

    // Constructor por defecto
    public WebSocketMessage() {
        this.timestamp = System.currentTimeMillis();
    }

    // Constructor para mensajes simples
    public WebSocketMessage(WebSocketMessageType type) {
        this.type = type;
        this.timestamp = System.currentTimeMillis();
    }

    // Getters y Setters
    public WebSocketMessageType getType() {
        return type;
    }

    public void setType(WebSocketMessageType type) {
        this.type = type;
    }

    public Long getAppointmentId() {
        return appointmentId;
    }

    public void setAppointmentId(Long appointmentId) {
        this.appointmentId = appointmentId;
    }

    public Long getRescheduleRequestId() {
        return rescheduleRequestId;
    }

    public void setRescheduleRequestId(Long rescheduleRequestId) {
        this.rescheduleRequestId = rescheduleRequestId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Map<String, Object> getData() {
        return data;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

    public Integer getReconnectInSeconds() {
        return reconnectInSeconds;
    }

    public void setReconnectInSeconds(Integer reconnectInSeconds) {
        this.reconnectInSeconds = reconnectInSeconds;
    }
}

