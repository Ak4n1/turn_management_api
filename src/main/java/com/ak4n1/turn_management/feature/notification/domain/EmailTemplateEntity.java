package com.ak4n1.turn_management.feature.notification.domain;

import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;
import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Entidad para almacenar plantillas de email en la base de datos.
 * 
 * Permite a los administradores gestionar plantillas personalizadas
 * con variables dinámicas y versionado.
 * Implementa US-T034.
 */
@Entity
@Table(name = "email_templates", indexes = {
    @Index(name = "idx_template_type_version", columnList = "template_type, version"),
    @Index(name = "idx_template_active", columnList = "active")
})
public class EmailTemplateEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Tipo de plantilla (ej: APPOINTMENT_CREATED).
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "template_type", nullable = false, length = 100)
    private EmailTemplateType templateType;

    /**
     * Versión de la plantilla (incrementa automáticamente).
     */
    @Column(name = "version", nullable = false)
    private Integer version = 1;

    /**
     * Asunto del email (puede contener variables como {{variableName}}).
     */
    @Column(name = "subject", nullable = false, length = 500)
    private String subject;

    /**
     * Cuerpo del email en HTML (puede contener variables como {{variableName}}).
     */
    @Column(name = "body", columnDefinition = "TEXT", nullable = false)
    private String body;

    /**
     * Lista de variables disponibles en la plantilla.
     */
    @ElementCollection
    @CollectionTable(name = "email_template_variables", joinColumns = @JoinColumn(name = "template_id"))
    @Column(name = "variable_name")
    private List<String> variables = new ArrayList<>();

    /**
     * Indica si la plantilla está activa.
     */
    @Column(name = "active", nullable = false)
    private Boolean active = true;

    /**
     * Notas o descripción de la plantilla.
     */
    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;

    /**
     * ID del usuario que creó/actualizó la plantilla.
     */
    @Column(name = "created_by_user_id", nullable = false)
    private Long createdByUserId;

    /**
     * Fecha y hora de creación.
     */
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * Fecha y hora de última actualización.
     */
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public EmailTemplateEntity() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    public EmailTemplateEntity(EmailTemplateType templateType, String subject, String body,
                               List<String> variables, Long createdByUserId) {
        this();
        this.templateType = templateType;
        this.subject = subject;
        this.body = body;
        this.variables = variables != null ? new ArrayList<>(variables) : new ArrayList<>();
        this.createdByUserId = createdByUserId;
    }

    /**
     * Incrementa la versión de la plantilla.
     */
    public void incrementVersion() {
        this.version++;
        this.updatedAt = LocalDateTime.now();
    }

    // Getters and Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public EmailTemplateType getTemplateType() {
        return templateType;
    }

    public void setTemplateType(EmailTemplateType templateType) {
        this.templateType = templateType;
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
        this.variables = variables != null ? new ArrayList<>(variables) : new ArrayList<>();
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

