package com.ak4n1.turn_management.feature.configuration.service.evaluation;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedDayResponse;

import java.time.LocalDate;
import java.util.List;

/**
 * Servicio para evaluar la disponibilidad de días aplicando reglas de precedencia.
 * 
 * Orden de precedencia:
 * 1. Bloqueos operativos (prioridad máxima)
 * 2. Excepciones por fecha
 * 3. Configuración base del calendario semanal
 */
public interface DayEvaluationService {

    /**
     * Evalúa un día específico aplicando el orden de precedencia.
     * 
     * @param date Fecha a evaluar
     * @param config Configuración activa
     * @param exceptions Excepciones en el rango consultado (para optimizar)
     * @param blocks Bloqueos en el rango consultado (para optimizar)
     * @return Información consolidada del día
     */
    ConsolidatedDayResponse evaluateDay(LocalDate date, CalendarConfiguration config,
                                       List<CalendarException> exceptions,
                                       List<ManualBlock> blocks);

    /**
     * Verifica si hay turnos confirmados para una fecha.
     * Solo considera turnos futuros (fecha >= hoy).
     * 
     * @param date Fecha a verificar
     * @return true si hay turnos confirmados futuros, false en caso contrario
     */
    Boolean hasExistingAppointments(LocalDate date);

    /**
     * Cuenta los turnos confirmados para una fecha.
     * Solo considera turnos futuros (fecha >= hoy).
     * 
     * @param date Fecha a verificar
     * @return Cantidad de turnos confirmados futuros para esa fecha
     */
    Integer countExistingAppointments(LocalDate date);
}
