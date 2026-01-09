package com.ak4n1.turn_management.shared.audit.dto;

import java.util.List;

/**
 * DTO de respuesta paginada para logs de auditoría.
 * 
 * Implementa US-T018.
 */
public class AuditLogsResponse {
    
    private List<AuditLogResponse> logs;
    private long totalElements;
    private int totalPages;
    private int page;
    private int size;

    public AuditLogsResponse() {
    }

    public AuditLogsResponse(List<AuditLogResponse> logs, long totalElements, int totalPages, int page, int size) {
        this.logs = logs;
        this.totalElements = totalElements;
        this.totalPages = totalPages;
        this.page = page;
        this.size = size;
    }

    public List<AuditLogResponse> getLogs() {
        return logs;
    }

    public void setLogs(List<AuditLogResponse> logs) {
        this.logs = logs;
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
}

