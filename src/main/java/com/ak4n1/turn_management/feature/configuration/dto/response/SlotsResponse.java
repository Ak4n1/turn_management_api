package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.util.List;

/**
 * DTO de respuesta para slots disponibles de una fecha.
 */
public class SlotsResponse {

    /**
     * Fecha para la cual se generaron los slots.
     */
    private LocalDate date;

    /**
     * Lista de slots disponibles para esta fecha.
     * Los slots están ordenados cronológicamente.
     */
    private List<SlotResponse> slots;

    /**
     * Cantidad total de slots generados.
     */
    private Integer totalSlots;

    /**
     * Cantidad de slots disponibles (no ocupados ni bloqueados).
     */
    private Integer availableSlots;

    /**
     * Indica si hay turnos existentes en un día cerrado.
     * Solo presente cuando el día está cerrado pero tiene turnos existentes.
     */
    private Boolean hasExistingAppointments;

    /**
     * Cantidad de turnos existentes en estados activos.
     * Solo presente cuando hasExistingAppointments = true.
     */
    private Integer existingAppointmentsCount;

    /**
     * Mensaje descriptivo.
     * Proporciona contexto sobre por qué no hay slots disponibles o información sobre turnos existentes.
     */
    private String message;

    public SlotsResponse() {
    }

    public SlotsResponse(LocalDate date, List<SlotResponse> slots, Integer totalSlots, Integer availableSlots) {
        this.date = date;
        this.slots = slots;
        this.totalSlots = totalSlots;
        this.availableSlots = availableSlots;
    }

    // Getters and Setters
    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public List<SlotResponse> getSlots() {
        return slots;
    }

    public void setSlots(List<SlotResponse> slots) {
        this.slots = slots;
    }

    public Integer getTotalSlots() {
        return totalSlots;
    }

    public void setTotalSlots(Integer totalSlots) {
        this.totalSlots = totalSlots;
    }

    public Integer getAvailableSlots() {
        return availableSlots;
    }

    public void setAvailableSlots(Integer availableSlots) {
        this.availableSlots = availableSlots;
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

