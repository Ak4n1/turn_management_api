package com.ak4n1.turn_management.feature.notification.dto.response;

import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;

import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO para respuesta de plantillas de email.
 */
public class EmailTemplateResponse {

    private Long id;
    private EmailTemplateType type;
    private Integer version;
    private String subject;
    private String body;
    private List<String> variables;
    private Boolean active;
    private String notes;
    private Long createdByUserId;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public EmailTemplateResponse() {
    }

    public EmailTemplateResponse(Long id, EmailTemplateType type, Integer version,
                                 String subject, String body, List<String> variables,
                                 Boolean active, String notes, Long createdByUserId,
                                 LocalDateTime createdAt, LocalDateTime updatedAt) {
        this.id = id;
        this.type = type;
        this.version = version;
        this.subject = subject;
        this.body = body;
        this.variables = variables;
        this.active = active;
        this.notes = notes;
        this.createdByUserId = createdByUserId;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
    }

    // Getters and Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public EmailTemplateType getType() {
        return type;
    }

    public void setType(EmailTemplateType type) {
        this.type = type;
    }

    public Integer getVersion() {
        return version;
    }

    public void setVersion(Integer version) {
        this.version = version;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
    }

    public List<String> getVariables() {
        return variables;
    }

    public void setVariables(List<String> variables) {
        this.variables = variables;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
}

