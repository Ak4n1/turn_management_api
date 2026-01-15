package com.ak4n1.turn_management.feature.configuration.service.impact;

import com.ak4n1.turn_management.feature.configuration.dto.request.PreviewImpactRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.AffectedAppointmentInfo;
import com.ak4n1.turn_management.feature.configuration.dto.response.PreviewImpactResponse;

import java.time.LocalDate;
import java.util.List;

/**
 * Servicio para calcular el impacto de cambios de configuración en turnos existentes.
 */
public interface ImpactCalculationService {

    /**
     * Previsualiza el impacto de un cambio de configuración propuesto.
     * 
     * @param request Request con el tipo de cambio y parámetros
     * @return Respuesta con información del impacto calculado
     */
    PreviewImpactResponse previewImpact(PreviewImpactRequest request);

    /**
     * Calcula turnos afectados en un rango de fechas.
     * 
     * @param startDate Fecha de inicio
     * @param endDate Fecha de fin
     * @return Lista de turnos afectados
     */
    List<AffectedAppointmentInfo> calculateAffectedAppointments(LocalDate startDate, LocalDate endDate);

    /**
     * Calcula turnos afectados para fechas específicas.
     * 
     * @param dates Lista de fechas específicas
     * @return Lista de turnos afectados en esas fechas
     */
    List<AffectedAppointmentInfo> calculateAffectedAppointmentsForDates(List<LocalDate> dates);
}
