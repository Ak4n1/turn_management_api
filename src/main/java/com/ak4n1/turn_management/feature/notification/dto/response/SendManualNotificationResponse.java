package com.ak4n1.turn_management.feature.notification.dto.response;

import java.util.List;

/**
 * DTO para respuesta de envío manual de notificaciones.
 * 
 * Implementa US-N011.
 */
public class SendManualNotificationResponse {

    private boolean success;
    private int totalRecipients;
    private int sentImmediately;
    private int pendingDelivery;
    private int excludedByPreferences;
    private List<Long> notificationIds;
    private String message;

    public SendManualNotificationResponse() {
    }

    public SendManualNotificationResponse(boolean success, int totalRecipients, 
                                         int sentImmediately, int pendingDelivery,
                                         int excludedByPreferences, List<Long> notificationIds,
                                         String message) {
        this.success = success;
        this.totalRecipients = totalRecipients;
        this.sentImmediately = sentImmediately;
        this.pendingDelivery = pendingDelivery;
        this.excludedByPreferences = excludedByPreferences;
        this.notificationIds = notificationIds;
        this.message = message;
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public int getTotalRecipients() {
        return totalRecipients;
    }

    public void setTotalRecipients(int totalRecipients) {
        this.totalRecipients = totalRecipients;
    }

    public int getSentImmediately() {
        return sentImmediately;
    }

    public void setSentImmediately(int sentImmediately) {
        this.sentImmediately = sentImmediately;
    }

    public int getPendingDelivery() {
        return pendingDelivery;
    }

    public void setPendingDelivery(int pendingDelivery) {
        this.pendingDelivery = pendingDelivery;
    }

    public int getExcludedByPreferences() {
        return excludedByPreferences;
    }

    public void setExcludedByPreferences(int excludedByPreferences) {
        this.excludedByPreferences = excludedByPreferences;
    }

    public List<Long> getNotificationIds() {
        return notificationIds;
    }

    public void setNotificationIds(List<Long> notificationIds) {
        this.notificationIds = notificationIds;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}

