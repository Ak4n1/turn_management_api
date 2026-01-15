package com.ak4n1.turn_management.feature.configuration.service.history;

import com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationHistoryResponse;

/**
 * Servicio para gestionar el historial de configuraciones del calendario.
 */
public interface ConfigurationHistoryService {

    /**
     * Obtiene el historial completo de configuraciones ordenado por versión descendente.
     * Incluye información de cada versión, cambios detectados, y turnos asociados.
     * 
     * @return Historial de configuraciones
     */
    ConfigurationHistoryResponse getConfigurationHistory();
}
