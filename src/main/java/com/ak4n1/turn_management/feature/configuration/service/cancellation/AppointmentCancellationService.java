package com.ak4n1.turn_management.feature.configuration.service.cancellation;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;

import java.util.List;

/**
 * Servicio para cancelar turnos afectados por cambios de configuración.
 */
public interface AppointmentCancellationService {

    /**
     * Cancela turnos afectados por el cierre de días.
     * 
     * @param appointments Lista de turnos a cancelar
     * @param adminUserId ID del administrador que realiza la cancelación
     * @param reason Razón de la cancelación
     */
    void cancelAffectedAppointmentsByDayClosure(List<Appointment> appointments, Long adminUserId, String reason);
}
