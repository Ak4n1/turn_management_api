package com.ak4n1.turn_management.feature.configuration.service.configuration;

/**
 * Servicio para gestionar el versionado de configuraciones del calendario.
 */
public interface ConfigurationVersionService {

    /**
     * Calcula la siguiente versión disponible.
     * Si no hay configuraciones previas, retorna 1.
     * 
     * @return Siguiente número de versión
     */
    Integer calculateNextVersion();

    /**
     * Desactiva la configuración activa anterior (si existe).
     * Solo debe haber una configuración activa a la vez.
     */
    void deactivatePreviousConfiguration();
}
