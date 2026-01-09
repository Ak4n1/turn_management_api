package com.ak4n1.turn_management.feature.notification.dto.response;

import java.util.List;

/**
 * DTO de respuesta para lista paginada de notificaciones del sistema.
 * 
 * Implementa US-T025.5.
 */
public class SystemNotificationsResponse {

    private List<SystemNotificationResponse> notifications;
    private long totalElements;
    private int totalPages;
    private int page;
    private int size;
    private long unreadCount;

    public SystemNotificationsResponse() {
    }

    public SystemNotificationsResponse(List<SystemNotificationResponse> notifications,
                                       long totalElements, int totalPages, int page, int size,
                                       long unreadCount) {
        this.notifications = notifications;
        this.totalElements = totalElements;
        this.totalPages = totalPages;
        this.page = page;
        this.size = size;
        this.unreadCount = unreadCount;
    }

    // Getters and Setters
    public List<SystemNotificationResponse> getNotifications() {
        return notifications;
    }

    public void setNotifications(List<SystemNotificationResponse> notifications) {
        this.notifications = notifications;
    }

    public long getTotalElements() {
        return totalElements;
    }

    public void setTotalElements(long totalElements) {
        this.totalElements = totalElements;
    }

    public int getTotalPages() {
        return totalPages;
    }

    public void setTotalPages(int totalPages) {
        this.totalPages = totalPages;
    }

    public int getPage() {
        return page;
    }

    public void setPage(int page) {
        this.page = page;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public long getUnreadCount() {
        return unreadCount;
    }

    public void setUnreadCount(long unreadCount) {
        this.unreadCount = unreadCount;
    }
}

