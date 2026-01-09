package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.dto.request.ManualBlockRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.ManualBlockResponse;

/**
 * Servicio para gestión de bloqueos operativos del calendario.
 * Los bloqueos operativos tienen prioridad máxima sobre todas las reglas.
 */
public interface ManualBlockService {

    /**
     * Crea un nuevo bloqueo operativo.
     * 
     * Reglas de negocio:
     * - La fecha no puede ser pasada
     * - Si isFullDay = false, debe tener timeRange
     * - Si isFullDay = true, no debe tener timeRange
     * - El motivo es obligatorio (mínimo 10 caracteres)
     * - Se detectan turnos existentes afectados
     * - Si affectsExistingAppointments = false y hay turnos afectados, se rechaza (409)
     * - Si affectsExistingAppointments = true, se permite aunque haya turnos afectados
     * 
     * @param request DTO con los datos del bloqueo
     * @param userId ID del usuario que crea el bloqueo (para auditoría)
     * @return Bloqueo creado con lista de turnos afectados
     * @throws IllegalArgumentException si el bloqueo es inválido
     */
    ManualBlockResponse createBlock(ManualBlockRequest request, Long userId);
}

