package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;

/**
 * DTO que representa la disponibilidad de un día específico.
 */
public class DayAvailabilityResponse {

    /**
     * Fecha del día evaluado.
     */
    private LocalDate date;

    /**
     * Estado de disponibilidad del día:
     * - FULL: Todos los slots están disponibles
     * - PARTIAL: Algunos slots están disponibles (hay bloqueos o ocupados)
     * - CLOSED: No hay slots disponibles (día cerrado o completamente bloqueado)
     */
    private String status; // FULL, PARTIAL, CLOSED

    /**
     * Cantidad de slots disponibles (no bloqueados ni ocupados).
     */
    private Integer availableSlots;

    /**
     * Cantidad total de slots generados para este día.
     */
    private Integer totalSlots;

    public DayAvailabilityResponse() {
    }

    public DayAvailabilityResponse(LocalDate date, String status, Integer availableSlots, Integer totalSlots) {
        this.date = date;
        this.status = status;
        this.availableSlots = availableSlots;
        this.totalSlots = totalSlots;
    }

    // Getters and Setters
    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Integer getAvailableSlots() {
        return availableSlots;
    }

    public void setAvailableSlots(Integer availableSlots) {
        this.availableSlots = availableSlots;
    }

    public Integer getTotalSlots() {
        return totalSlots;
    }

    public void setTotalSlots(Integer totalSlots) {
        this.totalSlots = totalSlots;
    }
}

