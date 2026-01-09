package com.ak4n1.turn_management.shared.metrics.service;

import com.ak4n1.turn_management.shared.metrics.dto.SystemMetricsResponse;

import java.time.LocalDate;

/**
 * Servicio para consultar métricas del sistema.
 * 
 * Implementa US-T022.
 */
public interface MetricsService {

    /**
     * Obtiene métricas del sistema para un rango de fechas.
     * 
     * Implementa US-T022:
     * - Solo accesible por admin
     * - Calcula métricas de turnos (creados, confirmados, cancelados, no-shows)
     * - Calcula tasa de cancelación
     * - Calcula slots ofrecidos vs usados
     * - Calcula tasa de uso de slots
     * - Identifica días más saturados
     * - Rango de fechas válido (startDate <= endDate, máximo 1 año)
     * 
     * @param startDate Fecha de inicio (inclusive)
     * @param endDate Fecha de fin (inclusive)
     * @return Métricas del sistema para el período especificado
     */
    SystemMetricsResponse getSystemMetrics(LocalDate startDate, LocalDate endDate);
}

