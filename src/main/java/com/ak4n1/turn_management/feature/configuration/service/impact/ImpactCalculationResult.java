package com.ak4n1.turn_management.feature.configuration.service.impact;

import com.ak4n1.turn_management.feature.configuration.dto.response.AffectedAppointmentInfo;

import java.util.List;

/**
 * Resultado del cálculo de impacto de un cambio de configuración.
 */
public class ImpactCalculationResult {
    private Integer affectedDays;
    private Integer slotsLost;
    private Integer existingAppointmentsAffected;
    private List<AffectedAppointmentInfo> appointments;
    private String changeDescription;

    public ImpactCalculationResult(Integer affectedDays, Integer slotsLost,
                                   Integer existingAppointmentsAffected,
                                   List<AffectedAppointmentInfo> appointments,
                                   String changeDescription) {
        this.affectedDays = affectedDays;
        this.slotsLost = slotsLost;
        this.existingAppointmentsAffected = existingAppointmentsAffected;
        this.appointments = appointments;
        this.changeDescription = changeDescription;
    }

    // Getters
    public Integer getAffectedDays() {
        return affectedDays;
    }

    public Integer getSlotsLost() {
        return slotsLost;
    }

    public Integer getExistingAppointmentsAffected() {
        return existingAppointmentsAffected;
    }

    public List<AffectedAppointmentInfo> getAppointments() {
        return appointments;
    }

    public String getChangeDescription() {
        return changeDescription;
    }
}
