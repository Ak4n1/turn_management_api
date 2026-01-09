package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.util.List;

/**
 * DTO de respuesta para un día del calendario consolidado.
 * Muestra qué regla aplica y el estado del día.
 */
public class ConsolidatedDayResponse {

    /**
     * Fecha del día.
     */
    private LocalDate date;

    /**
     * Estado del día.
     * - OPEN: Día abierto (tiene rangos horarios disponibles)
     * - CLOSED: Día cerrado (sin disponibilidad)
     * - PARTIAL: Día parcialmente disponible (algunos rangos disponibles)
     */
    private String state; // OPEN, CLOSED, PARTIAL

    /**
     * Tipo de regla que aplica a este día.
     * - BLOCK: Bloqueo operativo (prioridad máxima)
     * - EXCEPTION: Excepción por fecha específica
     * - BASE: Regla base del calendario semanal
     */
    private String ruleType; // BLOCK, EXCEPTION, BASE

    /**
     * Descripción explicativa de la regla aplicada.
     * Ejemplos:
     * - "Mantenimiento programado - Bloqueo operativo"
     * - "Sábado especial - Excepción"
     * - "Lunes laboral - Configuración base"
     */
    private String ruleDescription;

    /**
     * Rangos horarios disponibles para este día (solo si state = OPEN o PARTIAL).
     * Si state = CLOSED, esta lista estará vacía.
     */
    private List<TimeRangeResponse> timeRanges;

    /**
     * Indica si hay turnos existentes en este día.
     * - Si state = CLOSED y hasExistingAppointments = true: El día está cerrado según la configuración actual, 
     *   pero tiene turnos creados con una versión anterior donde el día estaba abierto.
     * - Si state = OPEN o PARTIAL: Indica si hay turnos agendados para ese día.
     */
    private Boolean hasExistingAppointments;

    /**
     * Cantidad de turnos existentes en estados activos (CREATED, CONFIRMED) para este día.
     * Solo se cuentan turnos en estados activos, no se incluyen cancelados, completados, etc.
     */
    private Integer appointmentsCount;

    public ConsolidatedDayResponse() {
    }

    public ConsolidatedDayResponse(LocalDate date, String state, String ruleType, 
                                   String ruleDescription, List<TimeRangeResponse> timeRanges,
                                   Boolean hasExistingAppointments, Integer appointmentsCount) {
        this.date = date;
        this.state = state;
        this.ruleType = ruleType;
        this.ruleDescription = ruleDescription;
        this.timeRanges = timeRanges;
        this.hasExistingAppointments = hasExistingAppointments;
        this.appointmentsCount = appointmentsCount;
    }

    // Getters and Setters
    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getRuleType() {
        return ruleType;
    }

    public void setRuleType(String ruleType) {
        this.ruleType = ruleType;
    }

    public String getRuleDescription() {
        return ruleDescription;
    }

    public void setRuleDescription(String ruleDescription) {
        this.ruleDescription = ruleDescription;
    }

    public List<TimeRangeResponse> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRangeResponse> timeRanges) {
        this.timeRanges = timeRanges;
    }

    public Boolean getHasExistingAppointments() {
        return hasExistingAppointments;
    }

    public void setHasExistingAppointments(Boolean hasExistingAppointments) {
        this.hasExistingAppointments = hasExistingAppointments;
    }

    public Integer getAppointmentsCount() {
        return appointmentsCount;
    }

    public void setAppointmentsCount(Integer appointmentsCount) {
        this.appointmentsCount = appointmentsCount;
    }
}

