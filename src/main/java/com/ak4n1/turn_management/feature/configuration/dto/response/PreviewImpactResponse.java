package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.util.List;

/**
 * DTO de respuesta para la previsualización del impacto de cambios propuestos.
 * Muestra qué días se verían afectados, cuántos slots desaparecerían y qué turnos existentes se impactarían.
 */
public class PreviewImpactResponse {

    /**
     * Cantidad de días afectados por el cambio propuesto.
     * Un día se considera afectado si cambia su disponibilidad (de abierto a cerrado, o viceversa)
     * o si cambian sus rangos horarios disponibles.
     */
    private Integer affectedDays;

    /**
     * Cantidad de slots que desaparecerían con el cambio propuesto.
     * Un slot es un intervalo de tiempo disponible para reservar un turno.
     * Se calcula comparando los slots actuales vs los slots propuestos.
     */
    private Integer slotsLost;

    /**
     * Cantidad de turnos existentes que se verían impactados por el cambio.
     * Por ahora siempre será 0 ya que no existe el dominio Appointment.
     * Se calculará en el futuro cuando existan turnos.
     */
    private Integer existingAppointmentsAffected;

    /**
     * Lista de turnos afectados con información detallada.
     * Por ahora siempre estará vacía ya que no existe el dominio Appointment.
     */
    private List<AffectedAppointmentInfo> appointments;

    /**
     * Mensaje descriptivo del cambio propuesto.
     */
    private String changeDescription;

    public PreviewImpactResponse() {
    }

    public PreviewImpactResponse(Integer affectedDays, Integer slotsLost, 
                                 Integer existingAppointmentsAffected,
                                 List<AffectedAppointmentInfo> appointments,
                                 String changeDescription) {
        this.affectedDays = affectedDays;
        this.slotsLost = slotsLost;
        this.existingAppointmentsAffected = existingAppointmentsAffected;
        this.appointments = appointments;
        this.changeDescription = changeDescription;
    }

    // Getters and Setters
    public Integer getAffectedDays() {
        return affectedDays;
    }

    public void setAffectedDays(Integer affectedDays) {
        this.affectedDays = affectedDays;
    }

    public Integer getSlotsLost() {
        return slotsLost;
    }

    public void setSlotsLost(Integer slotsLost) {
        this.slotsLost = slotsLost;
    }

    public Integer getExistingAppointmentsAffected() {
        return existingAppointmentsAffected;
    }

    public void setExistingAppointmentsAffected(Integer existingAppointmentsAffected) {
        this.existingAppointmentsAffected = existingAppointmentsAffected;
    }

    public List<AffectedAppointmentInfo> getAppointments() {
        return appointments;
    }

    public void setAppointments(List<AffectedAppointmentInfo> appointments) {
        this.appointments = appointments;
    }

    public String getChangeDescription() {
        return changeDescription;
    }

    public void setChangeDescription(String changeDescription) {
        this.changeDescription = changeDescription;
    }
}

