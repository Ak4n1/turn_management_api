package com.ak4n1.turn_management.feature.configuration.repository;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface CalendarExceptionRepository extends JpaRepository<CalendarException, Long> {

    /**
     * Busca una excepción por fecha.
     * 
     * @param date Fecha de la excepción
     * @return Excepción si existe
     */
    Optional<CalendarException> findByExceptionDate(LocalDate date);

    /**
     * Verifica si existe una excepción para una fecha.
     * 
     * @param date Fecha a verificar
     * @return true si existe, false en caso contrario
     */
    boolean existsByExceptionDate(LocalDate date);

    /**
     * Busca todas las excepciones activas.
     * 
     * @return Lista de excepciones activas
     */
    List<CalendarException> findByActiveTrue();

    /**
     * Busca excepciones activas dentro de un rango de fechas.
     * 
     * @param startDate Fecha de inicio
     * @param endDate Fecha de fin
     * @return Lista de excepciones en el rango
     */
    List<CalendarException> findByActiveTrueAndExceptionDateBetween(LocalDate startDate, LocalDate endDate);
}

