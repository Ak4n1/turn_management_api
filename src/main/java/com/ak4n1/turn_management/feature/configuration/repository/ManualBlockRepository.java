package com.ak4n1.turn_management.feature.configuration.repository;

import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;

@Repository
public interface ManualBlockRepository extends JpaRepository<ManualBlock, Long> {

    /**
     * Busca bloqueos activos para una fecha específica.
     * 
     * @param date Fecha a consultar
     * @return Lista de bloqueos activos para esa fecha
     */
    @Query("SELECT b FROM ManualBlock b WHERE b.active = true AND b.blockDate = :date")
    List<ManualBlock> findActiveBlocksByDate(@Param("date") LocalDate date);

    /**
     * Busca bloqueos activos que cubren un rango horario específico.
     * 
     * @param date Fecha del bloqueo
     * @param startTime Hora de inicio del rango
     * @param endTime Hora de fin del rango
     * @return Lista de bloqueos que afectan el rango
     */
    @Query("SELECT b FROM ManualBlock b WHERE b.active = true AND b.blockDate = :date " +
           "AND (b.isFullDay = true OR " +
           "(b.timeRange.start <= :endTime AND b.timeRange.end >= :startTime))")
    List<ManualBlock> findActiveBlocksOverlappingTimeRange(
        @Param("date") LocalDate date,
        @Param("startTime") String startTime,
        @Param("endTime") String endTime);

    /**
     * Busca todos los bloqueos activos.
     * 
     * @return Lista de bloqueos activos
     */
    List<ManualBlock> findByActiveTrue();

    /**
     * Busca bloqueos activos dentro de un rango de fechas.
     * 
     * @param startDate Fecha de inicio
     * @param endDate Fecha de fin
     * @return Lista de bloqueos en el rango
     */
    List<ManualBlock> findByActiveTrueAndBlockDateBetween(LocalDate startDate, LocalDate endDate);
}

