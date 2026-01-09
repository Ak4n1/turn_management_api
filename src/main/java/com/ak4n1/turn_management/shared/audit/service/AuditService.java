package com.ak4n1.turn_management.shared.audit.service;

import com.ak4n1.turn_management.shared.audit.dto.AuditLogsResponse;

import java.time.LocalDate;

/**
 * Servicio para consultar logs de auditoría.
 * 
 * Implementa US-T018.
 */
public interface AuditService {

    /**
     * Obtiene logs de auditoría con filtros opcionales.
     * 
     * Implementa US-T018:
     * - Solo accesible por admin
     * - Consolida información de múltiples fuentes:
     *   - AppointmentHistory (cambios de turnos)
     *   - CalendarConfiguration (cambios de configuraciones)
     *   - CalendarException (excepciones)
     *   - ManualBlock (bloqueos)
     *   - RescheduleRequest (reprogramaciones)
     * - Filtros opcionales por tipo de acción, fecha, usuario
     * - Resultados paginados
     * - Ordenados por fecha descendente (más recientes primero)
     * 
     * @param actionType Filtro opcional por tipo de acción (CANCELLATION, CONFIGURATION_CHANGE, etc.)
     * @param userId Filtro opcional por ID de usuario
     * @param startDate Filtro opcional por fecha desde
     * @param endDate Filtro opcional por fecha hasta
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20, máximo: 100)
     * @return Lista paginada de logs de auditoría
     */
    AuditLogsResponse getAuditLogs(
            String actionType,
            Long userId,
            LocalDate startDate,
            LocalDate endDate,
            int page,
            int size);
}

