package com.ak4n1.turn_management.feature.appointment.repository;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repositorio para gestión del historial de turnos (auditoría).
 */
@Repository
public interface AppointmentHistoryRepository extends JpaRepository<AppointmentHistory, Long> {

    /**
     * Busca el historial completo de un turno.
     * 
     * @param appointmentId ID del turno
     * @return Lista de eventos del historial, ordenados cronológicamente
     */
    List<AppointmentHistory> findByAppointmentIdOrderByCreatedAtAsc(Long appointmentId);

    /**
     * Busca el historial completo de un turno ordenado por fecha descendente (más recientes primero).
     * 
     * @param appointmentId ID del turno
     * @return Lista de eventos del historial, ordenados por fecha descendente
     */
    @Query("SELECT h FROM AppointmentHistory h WHERE h.appointmentId = :appointmentId " +
           "ORDER BY h.createdAt DESC")
    List<AppointmentHistory> findByAppointmentIdOrderByCreatedAtDesc(@Param("appointmentId") Long appointmentId);

    /**
     * Busca el historial de acciones de un usuario.
     * 
     * @param userId ID del usuario
     * @param startDate Fecha de inicio (inclusive)
     * @param endDate Fecha de fin (inclusive)
     * @return Lista de eventos del historial del usuario
     */
    @Query("SELECT h FROM AppointmentHistory h WHERE h.userId = :userId " +
           "AND h.createdAt BETWEEN :startDate AND :endDate " +
           "ORDER BY h.createdAt DESC")
    List<AppointmentHistory> findByUserIdAndDateRange(
        @Param("userId") Long userId,
        @Param("startDate") LocalDateTime startDate,
        @Param("endDate") LocalDateTime endDate);

    /**
     * Busca eventos del historial por tipo de acción.
     * 
     * @param action Tipo de acción (CREATED, CONFIRMED, CANCELLED, etc.)
     * @param startDate Fecha de inicio (inclusive)
     * @param endDate Fecha de fin (inclusive)
     * @return Lista de eventos del tipo especificado
     */
    @Query("SELECT h FROM AppointmentHistory h WHERE h.action = :action " +
           "AND h.createdAt BETWEEN :startDate AND :endDate " +
           "ORDER BY h.createdAt DESC")
    List<AppointmentHistory> findByActionAndDateRange(
        @Param("action") String action,
        @Param("startDate") LocalDateTime startDate,
        @Param("endDate") LocalDateTime endDate);
}

