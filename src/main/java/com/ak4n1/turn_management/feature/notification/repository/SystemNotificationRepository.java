package com.ak4n1.turn_management.feature.notification.repository;

import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.SystemNotification;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repositorio para notificaciones del sistema.
 * 
 * Implementa US-T025.5.
 */
@Repository
public interface SystemNotificationRepository extends JpaRepository<SystemNotification, Long> {

    /**
     * Encuentra todas las notificaciones de un usuario, ordenadas por fecha de creación descendente.
     */
    Page<SystemNotification> 
    findByRecipientIdOrderByCreatedAtDesc(Long recipientId, Pageable pageable);

    /**
     * Encuentra todas las notificaciones no leídas de un usuario.
     */
    List<SystemNotification> findByRecipientIdAndReadFalseOrderByCreatedAtDesc(Long recipientId);

    /**
     * Cuenta las notificaciones no leídas de un usuario.
     */
    long countByRecipientIdAndReadFalse(Long recipientId);

    /**
     * Cuenta las notificaciones no leídas de un usuario excluyendo un tipo
     * (ej. para no contar APPOINTMENT_CREATED en el badge de la campanita).
     */
    @Query("SELECT COUNT(n) FROM SystemNotification n WHERE n.recipientId = :recipientId AND n.read = false AND n.type <> :excludeType")
    long countByRecipientIdAndReadFalseAndTypeNot(@Param("recipientId") Long recipientId, @Param("excludeType") NotificationType excludeType);

    /**
     * Cuenta todas las notificaciones no leídas del sistema (para admin).
     * Implementa US-N006.
     */
    @Query("SELECT COUNT(n) FROM SystemNotification n WHERE n.read = false")
    long countUnreadNotifications();

    /**
     * Encuentra notificaciones por tipo y entidad relacionada.
     */
    List<SystemNotification> findByTypeAndRelatedEntityTypeAndRelatedEntityId(
            NotificationType type, 
            com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType relatedEntityType,
            Long relatedEntityId);

    /**
     * Encuentra notificaciones por usuario, tipo y estado de lectura.
     */
    Page<SystemNotification> findByRecipientIdAndTypeAndRead(Long recipientId, NotificationType type, Boolean read, Pageable pageable);

    /**
     * Encuentra notificaciones por usuario y tipo.
     */
    Page<SystemNotification> findByRecipientIdAndType(Long recipientId, NotificationType type, Pageable pageable);

    /**
     * Encuentra notificaciones por usuario y estado de lectura.
     */
    Page<SystemNotification> findByRecipientIdAndRead(Long recipientId, Boolean read, Pageable pageable);

    /**
     * Encuentra notificaciones por usuario y búsqueda en título o mensaje.
     */
    @org.springframework.data.jpa.repository.Query(
        "SELECT n FROM SystemNotification n WHERE n.recipientId = :recipientId " +
        "AND (LOWER(n.title) LIKE LOWER(CONCAT('%', :search, '%')) " +
        "OR LOWER(n.message) LIKE LOWER(CONCAT('%', :search, '%')))")
    Page<SystemNotification> findByRecipientIdAndSearch(
            @org.springframework.data.repository.query.Param("recipientId") Long recipientId,
            @org.springframework.data.repository.query.Param("search") String search,
            Pageable pageable);

    /**
     * Encuentra notificaciones por usuario, tipo, estado de lectura y búsqueda.
     */
    @Query("SELECT n FROM SystemNotification n WHERE n.recipientId = :recipientId " +
           "AND n.type = :type AND n.read = :read " +
           "AND (LOWER(n.title) LIKE LOWER(CONCAT('%', :search, '%')) " +
           "OR LOWER(n.message) LIKE LOWER(CONCAT('%', :search, '%')))")
    Page<SystemNotification> findByRecipientIdAndTypeAndReadAndSearch(
            @Param("recipientId") Long recipientId,
            @Param("type") NotificationType type,
            @Param("read") Boolean read,
            @Param("search") String search,
            Pageable pageable);

    /**
     * Encuentra todas las notificaciones no leídas de un usuario.
     */
    List<SystemNotification> findByRecipientIdAndReadFalse(Long recipientId);

    /**
     * Marca todas las notificaciones no leídas de un usuario como leídas (bulk update, una sola query).
     *
     * @param recipientId ID del destinatario
     * @return Número de notificaciones actualizadas
     */
    @Modifying
    @Transactional
    @Query("UPDATE SystemNotification n SET n.read = true, n.readAt = :now, n.updatedAt = :now WHERE n.recipientId = :recipientId AND n.read = false")
    int markAllAsReadByRecipientId(@Param("recipientId") Long recipientId, @Param("now") LocalDateTime now);

    /**
     * Cuenta notificaciones no leídas por múltiples destinatarios en una sola query (batch).
     *
     * @param recipientIds Lista de IDs de destinatarios
     * @return Pares [recipientId, count] para cada destinatario con notificaciones no leídas
     */
    @Query("SELECT n.recipientId, COUNT(n) FROM SystemNotification n WHERE n.recipientId IN :recipientIds AND n.read = false GROUP BY n.recipientId")
    List<Object[]> countUnreadByRecipientIds(@Param("recipientIds") List<Long> recipientIds);

    /**
     * Elimina notificaciones leídas más antiguas que el threshold.
     */
    @Modifying
    @Transactional
    @Query("DELETE FROM SystemNotification n WHERE n.read = true AND n.createdAt < :threshold")
    int deleteByReadTrueAndCreatedAtBefore(@Param("threshold") LocalDateTime threshold);

    /**
     * Elimina notificaciones no leídas más antiguas que el threshold.
     */
    @Modifying
    @Transactional
    @Query("DELETE FROM SystemNotification n WHERE n.read = false AND n.createdAt < :threshold")
    int deleteByReadFalseAndCreatedAtBefore(@Param("threshold") LocalDateTime threshold);

    /**
     * Encuentra todas las notificaciones del sistema (para admin), ordenadas por fecha de creación descendente.
     * Implementa US-N006.
     */
    Page<SystemNotification> findAllByOrderByCreatedAtDesc(Pageable pageable);

    /**
     * Encuentra notificaciones del sistema por tipo (para admin).
     * Implementa US-N006.
     */
    Page<SystemNotification> findByTypeOrderByCreatedAtDesc(NotificationType type, Pageable pageable);

    /**
     * Encuentra notificaciones del sistema por usuario y tipo (para admin).
     * Implementa US-N006.
     */
    Page<SystemNotification> findByRecipientIdAndTypeOrderByCreatedAtDesc(Long recipientId, NotificationType type, Pageable pageable);

    /**
     * Encuentra notificaciones del sistema por tipo y estado de lectura (para admin).
     * Implementa US-N006.
     */
    Page<SystemNotification> findByTypeAndReadOrderByCreatedAtDesc(NotificationType type, Boolean read, Pageable pageable);

    /**
     * Encuentra notificaciones del sistema por usuario y estado de lectura (para admin).
     * Implementa US-N006.
     */
    Page<SystemNotification> findByRecipientIdAndReadOrderByCreatedAtDesc(Long recipientId, Boolean read, Pageable pageable);

    /**
     * Encuentra notificaciones del sistema por usuario, tipo y estado de lectura (para admin).
     * Implementa US-N006.
     */
    Page<SystemNotification> findByRecipientIdAndTypeAndReadOrderByCreatedAtDesc(
            Long recipientId, NotificationType type, Boolean read, Pageable pageable);

    /**
     * Encuentra notificaciones del sistema por rango de fechas (para admin).
     * Implementa US-N006.
     */
    @Query("SELECT n FROM SystemNotification n WHERE n.createdAt >= :dateFrom AND n.createdAt <= :dateTo ORDER BY n.createdAt DESC")
    Page<SystemNotification> findByCreatedAtBetweenOrderByCreatedAtDesc(
            @Param("dateFrom") LocalDateTime dateFrom,
            @Param("dateTo") LocalDateTime dateTo,
            Pageable pageable);

    /**
     * Encuentra notificaciones del sistema con múltiples filtros (para admin).
     * Implementa US-N006.
     */
    @Query("SELECT n FROM SystemNotification n WHERE " +
           "(:recipientId IS NULL OR n.recipientId = :recipientId) AND " +
           "(:type IS NULL OR n.type = :type) AND " +
           "(:read IS NULL OR n.read = :read) AND " +
           "(:dateFrom IS NULL OR n.createdAt >= :dateFrom) AND " +
           "(:dateTo IS NULL OR n.createdAt <= :dateTo) AND " +
           "(:search IS NULL OR LOWER(n.title) LIKE LOWER(CONCAT('%', :search, '%')) OR LOWER(n.message) LIKE LOWER(CONCAT('%', :search, '%'))) " +
           "ORDER BY n.createdAt DESC")
    Page<SystemNotification> findAdminNotificationsWithFilters(
            @Param("recipientId") Long recipientId,
            @Param("type") NotificationType type,
            @Param("read") Boolean read,
            @Param("dateFrom") LocalDateTime dateFrom,
            @Param("dateTo") LocalDateTime dateTo,
            @Param("search") String search,
            Pageable pageable);
}

