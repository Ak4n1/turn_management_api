package com.ak4n1.turn_management.feature.configuration.repository;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CalendarConfigurationRepository extends JpaRepository<CalendarConfiguration, Long> {

    /**
     * Busca la configuración activa.
     * Solo debe haber una configuración activa a la vez.
     */
    Optional<CalendarConfiguration> findByActiveTrue();

    /**
     * Busca la configuración por versión.
     */
    Optional<CalendarConfiguration> findByVersion(Integer version);

    /**
     * Obtiene la última versión (mayor número de versión).
     */
    @Query("SELECT MAX(c.version) FROM CalendarConfiguration c")
    Optional<Integer> findMaxVersion();

    /**
     * Cuenta cuántas configuraciones activas existen.
     * Debe ser 0 o 1.
     */
    long countByActiveTrue();

    /**
     * Obtiene todas las configuraciones ordenadas por versión descendente (más recientes primero).
     * 
     * @return Lista de todas las configuraciones ordenadas por versión
     */
    @Query("SELECT c FROM CalendarConfiguration c ORDER BY c.version DESC")
    List<CalendarConfiguration> findAllByOrderByVersionDesc();
}

