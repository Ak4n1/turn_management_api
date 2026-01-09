package com.ak4n1.turn_management.feature.configuration.dto.response;

/**
 * DTO que representa un slot (intervalo de tiempo) disponible para reservar.
 */
public class SlotResponse {

    /**
     * Hora de inicio del slot en formato HH:mm.
     */
    private String start;

    /**
     * Hora de fin del slot en formato HH:mm.
     */
    private String end;

    /**
     * Indica si el slot está disponible para reservar.
     * Por ahora siempre será true ya que no existe el dominio Appointment.
     * Se calculará en el futuro cuando existan turnos.
     */
    private Boolean available;

    public SlotResponse() {
    }

    public SlotResponse(String start, String end, Boolean available) {
        this.start = start;
        this.end = end;
        this.available = available;
    }

    // Getters and Setters
    public String getStart() {
        return start;
    }

    public void setStart(String start) {
        this.start = start;
    }

    public String getEnd() {
        return end;
    }

    public void setEnd(String end) {
        this.end = end;
    }

    public Boolean getAvailable() {
        return available;
    }

    public void setAvailable(Boolean available) {
        this.available = available;
    }
}

