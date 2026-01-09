package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.util.List;

/**
 * DTO de respuesta para evaluación de disponibilidad de una fecha.
 * Indica si la fecha está disponible y qué regla aplica.
 */
public class AvailabilityResponse {

    /**
     * Fecha evaluada.
     */
    private LocalDate date;

    /**
     * Indica si la fecha está disponible para reservar turnos.
     */
    private Boolean isAvailable;

    /**
     * Tipo de regla que aplica a esta fecha.
     * - BLOCK: Bloqueo operativo (prioridad máxima) - fecha NO disponible
     * - EXCEPTION: Excepción por fecha específica
     * - BASE: Regla base del calendario semanal
     */
    private String ruleApplied; // BLOCK, EXCEPTION, BASE

    /**
     * Rangos horarios disponibles para esta fecha.
     * Solo presente si isAvailable = true.
     * Vacío si isAvailable = false.
     */
    private List<TimeRangeResponse> timeRanges;

    /**
     * Descripción explicativa de por qué la fecha está disponible o no.
     */
    private String description;

    /**
     * Indica si hay turnos existentes en un día cerrado.
     * Solo presente cuando isAvailable = false.
     */
    private Boolean hasExistingAppointments;

    /**
     * Cantidad de turnos existentes en estados activos (CREATED, CONFIRMED).
     * Solo presente cuando hasExistingAppointments = true.
     */
    private Integer existingAppointmentsCount;

    /**
     * Mensaje descriptivo sobre la disponibilidad del día.
     * Proporciona contexto adicional cuando el día está cerrado con turnos existentes.
     */
    private String message;

    public AvailabilityResponse() {
    }

    public AvailabilityResponse(LocalDate date, Boolean isAvailable, String ruleApplied,
                                List<TimeRangeResponse> timeRanges, String description) {
        this.date = date;
        this.isAvailable = isAvailable;
        this.ruleApplied = ruleApplied;
        this.timeRanges = timeRanges;
        this.description = description;
    }

    // Getters and Setters
    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void setIsAvailable(Boolean isAvailable) {
        this.isAvailable = isAvailable;
    }

    public String getRuleApplied() {
        return ruleApplied;
    }

    public void setRuleApplied(String ruleApplied) {
        this.ruleApplied = ruleApplied;
    }

    public List<TimeRangeResponse> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRangeResponse> timeRanges) {
        this.timeRanges = timeRanges;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Boolean getHasExistingAppointments() {
        return hasExistingAppointments;
    }

    public void setHasExistingAppointments(Boolean hasExistingAppointments) {
        this.hasExistingAppointments = hasExistingAppointments;
    }

    public Integer getExistingAppointmentsCount() {
        return existingAppointmentsCount;
    }

    public void setExistingAppointmentsCount(Integer existingAppointmentsCount) {
        this.existingAppointmentsCount = existingAppointmentsCount;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}

