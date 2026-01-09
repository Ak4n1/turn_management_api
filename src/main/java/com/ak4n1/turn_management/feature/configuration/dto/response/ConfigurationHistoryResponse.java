package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.util.List;

/**
 * DTO de respuesta para el historial de configuraciones del calendario.
 * 
 * Implementa US-T018.1.
 */
public class ConfigurationHistoryResponse {
    
    private List<ConfigurationVersionResponse> versions;
    private int totalVersions;

    public ConfigurationHistoryResponse() {
    }

    public ConfigurationHistoryResponse(List<ConfigurationVersionResponse> versions, int totalVersions) {
        this.versions = versions;
        this.totalVersions = totalVersions;
    }

    public List<ConfigurationVersionResponse> getVersions() {
        return versions;
    }

    public void setVersions(List<ConfigurationVersionResponse> versions) {
        this.versions = versions;
    }

    public int getTotalVersions() {
        return totalVersions;
    }

    public void setTotalVersions(int totalVersions) {
        this.totalVersions = totalVersions;
    }
}

