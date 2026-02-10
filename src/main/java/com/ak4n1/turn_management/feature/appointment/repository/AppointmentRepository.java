package com.ak4n1.turn_management.feature.appointment.repository;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import jakarta.persistence.LockModeType;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;

/**
 * Repositorio para gestión de turnos (appointments).
 */
@Repository
public interface AppointmentRepository extends JpaRepository<Appointment, Long> {

    /**
     * Busca un turno por clave de idempotencia.
     * 
     * @param idempotencyKey Clave de idempotencia
     * @return Turno existente si existe, empty si no
     */
    Optional<Appointment> findByIdempotencyKey(String idempotencyKey);

    /**
     * Busca turnos por usuario y rango de fechas.
     * 
     * @param userId ID del usuario
     * @param startDate Fecha de inicio (inclusive)
     * @param endDate Fecha de fin (inclusive)
     * @return Lista de turnos del usuario en el rango
     */
    List<Appointment> findByUserIdAndAppointmentDateBetween(
        Long userId, LocalDate startDate, LocalDate endDate);

    /**
     * Busca turnos por usuario y estado.
     * 
     * @param userId ID del usuario
     * @param state Estado del turno
     * @return Lista de turnos del usuario con el estado especificado
     */
    List<Appointment> findByUserIdAndState(Long userId, AppointmentState state);

    /**
     * Busca turnos por usuario, fecha y hora de inicio.
     * Usado para validar disponibilidad y evitar duplicados.
     * 
     * @param userId ID del usuario
     * @param appointmentDate Fecha del turno
     * @param startTime Hora de inicio
     * @return Turno existente si existe, empty si no
     */
    Optional<Appointment> findByUserIdAndAppointmentDateAndStartTime(
        Long userId, LocalDate appointmentDate, LocalTime startTime);

    /**
     * Busca turnos por fecha, hora de inicio y estado.
     * Usado para validar disponibilidad (verificar si el slot está ocupado).
     * 
     * @param appointmentDate Fecha del turno
     * @param startTime Hora de inicio
     * @param states Estados a considerar (solo turnos confirmados o creados ocupan el slot)
     * @return Lista de turnos que ocupan el slot
     */
    @Query("SELECT a FROM Appointment a WHERE a.appointmentDate = :date " +
           "AND a.startTime = :startTime " +
           "AND a.state IN :states")
    List<Appointment> findByDateAndStartTimeAndStateIn(
        @Param("date") LocalDate appointmentDate,
        @Param("startTime") LocalTime startTime,
        @Param("states") List<AppointmentState> states);

    /**
     * Busca turnos por fecha y hora de inicio con lock pesimista.
     * Usado para control de concurrencia al crear turnos.
     * 
     * @param appointmentDate Fecha del turno
     * @param startTime Hora de inicio
     * @param states Estados a considerar
     * @return Lista de turnos que ocupan el slot (con lock)
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT a FROM Appointment a WHERE a.appointmentDate = :date " +
           "AND a.startTime = :startTime " +
           "AND a.state IN :states")
    List<Appointment> findByDateAndStartTimeAndStateInWithLock(
        @Param("date") LocalDate appointmentDate,
        @Param("startTime") LocalTime startTime,
        @Param("states") List<AppointmentState> states);

    /**
     * Cuenta turnos por usuario y estado en un rango de fechas.
     * Usado para validar límites de turnos por usuario.
     * 
     * @param userId ID del usuario
     * @param states Estados a considerar
     * @param startDate Fecha de inicio (inclusive)
     * @param endDate Fecha de fin (inclusive)
     * @return Cantidad de turnos
     */
    @Query("SELECT COUNT(a) FROM Appointment a WHERE a.userId = :userId " +
           "AND a.state IN :states " +
           "AND a.appointmentDate BETWEEN :startDate AND :endDate")
    long countByUserIdAndStateInAndDateRange(
        @Param("userId") Long userId,
        @Param("states") List<AppointmentState> states,
        @Param("startDate") LocalDate startDate,
        @Param("endDate") LocalDate endDate);

    /**
     * Busca turnos en estado CREATED que han expirado.
     * Usado por el scheduler de expiración automática.
     * 
     * @param now Fecha y hora actual
     * @return Lista de turnos expirados
     */
    @Query("SELECT a FROM Appointment a WHERE a.state = 'CREATED' " +
           "AND a.expiresAt IS NOT NULL " +
           "AND a.expiresAt <= :now")
    List<Appointment> findExpiredAppointments(@Param("now") LocalDateTime now);

    /**
     * Busca turnos por fecha.
     * 
     * @param appointmentDate Fecha del turno
     * @param states Estados a considerar
     * @return Lista de turnos en la fecha especificada
     */
    @Query("SELECT a FROM Appointment a WHERE a.appointmentDate = :date " +
           "AND a.state IN :states " +
           "ORDER BY a.startTime")
    List<Appointment> findByDateAndStateIn(
        @Param("date") LocalDate appointmentDate,
        @Param("states") List<AppointmentState> states);

    /**
     * NUEVO: Busca turnos por fecha y estados, con paginación.
     * 
     * @param appointmentDate Fecha del turno
     * @param states Estados a considerar
     * @param pageable Paginación
     * @return Página de turnos en la fecha especificada
     */
    @Query("SELECT a FROM Appointment a WHERE a.appointmentDate = :date " +
           "AND a.state IN :states " +
           "ORDER BY a.startTime ASC")
    Page<Appointment> findByDateAndStateIn(
        @Param("date") LocalDate appointmentDate,
        @Param("states") List<AppointmentState> states,
        Pageable pageable);

    /**
     * Busca turnos por usuario.
     * 
     * @param userId ID del usuario
     * @return Lista de todos los turnos del usuario, ordenados por fecha
     */
    List<Appointment> findByUserIdOrderByAppointmentDateAscStartTimeAsc(Long userId);

    /**
     * Busca TODOS los turnos por fecha, independientemente de su estado.
     * Usado para contar turnos afectados cuando se cierra un día.
     * 
     * @param appointmentDate Fecha del turno
     * @return Lista de todos los turnos en la fecha (incluye todos los estados)
     */
    @Query("SELECT a FROM Appointment a WHERE a.appointmentDate = :date")
    List<Appointment> findAllByAppointmentDate(@Param("date") LocalDate appointmentDate);

    /**
     * Busca turnos del usuario con paginación y filtros opcionales.
     * 
     * @param userId ID del usuario
     * @param state Estado opcional (null para todos)
     * @param fromDate Fecha desde opcional (null para sin límite)
     * @param toDate Fecha hasta opcional (null para sin límite)
     * @param daysOfWeek Lista de días de la semana opcional (null para todos, 1=Lunes, 7=Domingo)
     * @param daysOfWeekCount Cantidad de días de la semana seleccionados (0 si no hay filtro)
     * @param pageable Paginación
     * @return Página de turnos del usuario
     */
    @Query(value = "SELECT * FROM appointments a WHERE a.user_id = :userId " +
           "AND (:state IS NULL OR a.state = :state) " +
           "AND (:fromDate IS NULL OR a.appointment_date >= :fromDate) " +
           "AND (:toDate IS NULL OR a.appointment_date <= :toDate) " +
           "AND (:daysOfWeekCount = 0 OR a.day_of_week IN (:daysOfWeek)) " +
           "ORDER BY a.appointment_date ASC, a.start_time ASC",
           countQuery = "SELECT COUNT(*) FROM appointments a WHERE a.user_id = :userId " +
           "AND (:state IS NULL OR a.state = :state) " +
           "AND (:fromDate IS NULL OR a.appointment_date >= :fromDate) " +
           "AND (:toDate IS NULL OR a.appointment_date <= :toDate) " +
           "AND (:daysOfWeekCount = 0 OR a.day_of_week IN (:daysOfWeek))",
           nativeQuery = true)
    Page<Appointment> findByUserIdWithFilters(
            @Param("userId") Long userId,
            @Param("state") String state,
            @Param("fromDate") LocalDate fromDate,
            @Param("toDate") LocalDate toDate,
            @Param("daysOfWeek") java.util.List<Integer> daysOfWeek,
            @Param("daysOfWeekCount") int daysOfWeekCount,
            Pageable pageable);

    /**
     * Busca turnos con filtros para administrador (todos los turnos).
     * 
     * @param state Estado opcional (null para todos)
     * @param userId ID del usuario opcional (null para todos)
     * @param fromDate Fecha desde opcional (null para sin límite)
     * @param toDate Fecha hasta opcional (null para sin límite)
     * @param date Fecha específica opcional (null si no se busca por fecha específica)
     * @param daysOfWeek Lista de días de la semana opcional (null para todos, 1=Lunes, 7=Domingo)
     * @param pageable Paginación
     * @return Página de turnos
     */
    @Query(value = "SELECT * FROM appointments a WHERE " +
           "(:state IS NULL OR a.state = :state) " +
           "AND (:userId IS NULL OR a.user_id = :userId) " +
           "AND (:fromDate IS NULL OR a.appointment_date >= :fromDate) " +
           "AND (:toDate IS NULL OR a.appointment_date <= :toDate) " +
           "AND (:date IS NULL OR a.appointment_date = :date) " +
           "AND (:daysOfWeekCount = 0 OR a.day_of_week IN (:daysOfWeek)) " +
           "ORDER BY a.appointment_date DESC, a.start_time ASC",
           countQuery = "SELECT COUNT(*) FROM appointments a WHERE " +
           "(:state IS NULL OR a.state = :state) " +
           "AND (:userId IS NULL OR a.user_id = :userId) " +
           "AND (:fromDate IS NULL OR a.appointment_date >= :fromDate) " +
           "AND (:toDate IS NULL OR a.appointment_date <= :toDate) " +
           "AND (:date IS NULL OR a.appointment_date = :date) " +
           "AND (:daysOfWeekCount = 0 OR a.day_of_week IN (:daysOfWeek))",
           nativeQuery = true)
    Page<Appointment> findAllWithFilters(
            @Param("state") String state,
            @Param("userId") Long userId,
            @Param("fromDate") java.time.LocalDate fromDate,
            @Param("toDate") java.time.LocalDate toDate,
            @Param("date") java.time.LocalDate date,
            @Param("daysOfWeek") java.util.List<Integer> daysOfWeek,
            @Param("daysOfWeekCount") int daysOfWeekCount,
            Pageable pageable);

    /**
     * Busca turnos por lista de IDs de usuario (para búsqueda por email/nombre).
     * 
     * @param userIds Lista de IDs de usuarios
     * @param state Estado opcional (null para todos)
     * @param fromDate Fecha desde opcional (null para sin límite)
     * @param toDate Fecha hasta opcional (null para sin límite)
     * @param date Fecha específica opcional (null si no se busca por fecha específica)
     * @param pageable Paginación
     * @return Página de turnos
     */
    @Query(value = "SELECT * FROM appointments a WHERE a.user_id IN :userIds " +
           "AND (:state IS NULL OR a.state = :state) " +
           "AND (:fromDate IS NULL OR a.appointment_date >= :fromDate) " +
           "AND (:toDate IS NULL OR a.appointment_date <= :toDate) " +
           "AND (:date IS NULL OR a.appointment_date = :date) " +
           "AND (:daysOfWeekCount = 0 OR a.day_of_week IN (:daysOfWeek)) " +
           "ORDER BY a.appointment_date DESC, a.start_time ASC",
           countQuery = "SELECT COUNT(*) FROM appointments a WHERE a.user_id IN :userIds " +
           "AND (:state IS NULL OR a.state = :state) " +
           "AND (:fromDate IS NULL OR a.appointment_date >= :fromDate) " +
           "AND (:toDate IS NULL OR a.appointment_date <= :toDate) " +
           "AND (:date IS NULL OR a.appointment_date = :date) " +
           "AND (:daysOfWeekCount = 0 OR a.day_of_week IN (:daysOfWeek))",
           nativeQuery = true)
    Page<Appointment> findByUserIdsWithFilters(
            @Param("userIds") java.util.List<Long> userIds,
            @Param("state") String state,
            @Param("fromDate") java.time.LocalDate fromDate,
            @Param("toDate") java.time.LocalDate toDate,
            @Param("date") java.time.LocalDate date,
            @Param("daysOfWeek") java.util.List<Integer> daysOfWeek,
            @Param("daysOfWeekCount") int daysOfWeekCount,
            Pageable pageable);

    /**
     * Cuenta turnos asociados a una versión específica de configuración.
     * 
     * @param calendarConfigVersion Versión de configuración
     * @return Cantidad de turnos asociados a esa versión
     */
    @Query("SELECT COUNT(a) FROM Appointment a WHERE a.calendarConfigVersion = :version")
    long countByCalendarConfigVersion(@Param("version") Integer calendarConfigVersion);

    /**
     * Busca todos los turnos en un rango de fechas.
     * 
     * @param startDate Fecha de inicio (inclusive)
     * @param endDate Fecha de fin (inclusive)
     * @return Lista de turnos en el rango
     */
    List<Appointment> findAllByAppointmentDateBetween(LocalDate startDate, LocalDate endDate);

    /**
     * Busca un turno por ID con lock pesimista.
     * Usado para evitar condiciones de carrera al aprobar/rechazar solicitudes de reprogramación.
     * 
     * @param id ID del turno
     * @return Turno con lock pesimista
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT a FROM Appointment a WHERE a.id = :id")
    Optional<Appointment> findByIdWithPessimisticLock(@Param("id") Long id);

}

