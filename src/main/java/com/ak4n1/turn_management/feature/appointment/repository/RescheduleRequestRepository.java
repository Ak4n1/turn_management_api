package com.ak4n1.turn_management.feature.appointment.repository;

import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequest;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import jakarta.persistence.LockModeType;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repositorio para gestión de solicitudes de reprogramación.
 */
@Repository
public interface RescheduleRequestRepository extends JpaRepository<RescheduleRequest, Long> {

    /**
     * Busca solicitudes pendientes para un turno específico.
     * 
     * @param appointmentId ID del turno
     * @param state Estado pendiente
     * @return Lista de solicitudes pendientes para el turno
     */
    List<RescheduleRequest> findByAppointmentIdAndState(Long appointmentId, RescheduleRequestState state);

    /**
     * Busca todas las solicitudes de reprogramación para un turno específico.
     * 
     * @param appointmentId ID del turno
     * @return Lista de todas las solicitudes del turno, ordenadas por fecha de creación
     */
    List<RescheduleRequest> findByAppointmentIdOrderByCreatedAtDesc(Long appointmentId);

    /**
     * Busca solicitudes por usuario.
     * 
     * @param userId ID del usuario
     * @return Lista de solicitudes del usuario, ordenadas por fecha de creación
     */
    List<RescheduleRequest> findByUserIdOrderByCreatedAtDesc(Long userId);

    /**
     * Busca solicitudes por estado (para admin).
     * 
     * @param state Estado de la solicitud
     * @return Lista de solicitudes con el estado especificado
     */
    List<RescheduleRequest> findByStateOrderByCreatedAtAsc(RescheduleRequestState state);

    /**
     * Busca solicitudes pendientes más antiguas que un threshold (para expiración).
     * 
     * @param state Estado pendiente
     * @param threshold Fecha y hora threshold
     * @return Lista de solicitudes pendientes antiguas
     */
    @Query("SELECT r FROM RescheduleRequest r WHERE r.state = :state " +
           "AND r.createdAt <= :threshold " +
           "ORDER BY r.createdAt ASC")
    List<RescheduleRequest> findPendingRequestsOlderThan(
        @Param("state") RescheduleRequestState state,
        @Param("threshold") LocalDateTime threshold);

    /**
     * Busca una solicitud por ID y estado.
     * 
     * @param id ID de la solicitud
     * @param state Estado esperado
     * @return Solicitud si existe y tiene el estado esperado
     */
    Optional<RescheduleRequest> findByIdAndState(Long id, RescheduleRequestState state);

    /**
     * Busca solicitudes con filtros para admin (con paginación).
     */
    @Query("SELECT r FROM RescheduleRequest r WHERE " +
           "(:state IS NULL OR r.state = :state) AND " +
           "(:userId IS NULL OR r.userId = :userId) AND " +
           "(:fromDate IS NULL OR r.createdAt >= :fromDate) AND " +
           "(:toDate IS NULL OR r.createdAt <= :toDate) " +
           "ORDER BY r.createdAt DESC")
    Page<RescheduleRequest> findAllWithFilters(
        @Param("state") RescheduleRequestState state,
        @Param("userId") Long userId,
        @Param("fromDate") LocalDateTime fromDate,
        @Param("toDate") LocalDateTime toDate,
        Pageable pageable);

    /**
     * Busca una solicitud por ID con lock pesimista.
     * Usado para evitar condiciones de carrera al aprobar/rechazar solicitudes.
     * 
     * @param id ID de la solicitud
     * @return Solicitud con lock pesimista
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT r FROM RescheduleRequest r WHERE r.id = :id")
    Optional<RescheduleRequest> findByIdWithPessimisticLock(@Param("id") Long id);
}

