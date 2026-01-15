package com.ak4n1.turn_management.feature.configuration.service.validation;

/**
 * Interfaz genérica para validadores de configuración.
 * 
 * @param <T> Tipo del objeto a validar
 */
public interface ConfigurationValidator<T> {
    
    /**
     * Valida el objeto de configuración.
     * 
     * @param request Objeto a validar
     * @throws com.ak4n1.turn_management.shared.exception.ApiException si la validación falla
     */
    void validate(T request);
}
