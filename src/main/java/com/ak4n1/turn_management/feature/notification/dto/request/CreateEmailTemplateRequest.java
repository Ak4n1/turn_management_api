package com.ak4n1.turn_management.feature.notification.dto.request;

import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;

/**
 * DTO para crear/actualizar una plantilla de email.
 */
public class CreateEmailTemplateRequest {

    @NotNull(message = "El tipo de plantilla es requerido")
    private EmailTemplateType type;

    @NotBlank(message = "El asunto es requerido")
    @Size(max = 500, message = "El asunto no puede exceder 500 caracteres")
    private String subject;

    @NotBlank(message = "El cuerpo del email es requerido")
    @Size(max = 10000, message = "El cuerpo del email no puede exceder 10000 caracteres")
    private String body;

    private List<@NotBlank(message = "Las variables no pueden estar vacías") String> variables;

    @NotNull(message = "El estado activo es requerido")
    private Boolean active;

    @Size(max = 1000, message = "Las notas no pueden exceder 1000 caracteres")
    private String notes;

    public CreateEmailTemplateRequest() {
    }

    // Getters and Setters

    public EmailTemplateType getType() {
        return type;
    }

    public void setType(EmailTemplateType type) {
        this.type = type;
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
}

