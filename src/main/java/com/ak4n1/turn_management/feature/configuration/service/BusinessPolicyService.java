package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.dto.request.BusinessPolicyRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.BusinessPolicyResponse;
import com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy;

/**
 * Servicio para gestión de políticas de negocio.
 * 
 * Implementa US-T019.
 */
public interface BusinessPolicyService {

    /**
     * Crea o actualiza una política de negocio.
     * 
     * Implementa US-T019:
     * - Solo accesible por admin
     * - Valida que los valores sean positivos y coherentes
     * - Asegura que solo una política esté activa a la vez
     * - Desactiva la política anterior si existe
     * - Crea una nueva política activa
     * 
     * @param request Request con los valores de la política
     * @param userId ID del usuario que crea la política (para auditoría)
     * @return Política creada o actualizada
     */
    BusinessPolicyResponse createOrUpdatePolicy(BusinessPolicyRequest request, Long userId);

    /**
     * Obtiene la política activa actual.
     * 
     * @return Política activa si existe, null en caso contrario
     */
    BusinessPolicyResponse getActivePolicy();

    /**
     * Obtiene la política activa como entidad (para uso interno).
     * 
     * @return Política activa si existe, null en caso contrario
     */
    BusinessPolicy getActivePolicyEntity();
}

