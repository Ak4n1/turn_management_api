package com.ak4n1.turn_management.feature.notification.repository;

import com.ak4n1.turn_management.feature.notification.domain.EmailNotificationType;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailNotification;
import com.ak4n1.turn_management.feature.notification.domain.FailedEmailStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repositorio para gestión de emails fallidos.
 */
@Repository
public interface FailedEmailNotificationRepository extends JpaRepository<FailedEmailNotification, Long> {

    /**
     * Busca emails fallidos por estado.
     * 
     * @param status Estado del email fallido
     * @param pageable Paginación
     * @return Página de emails fallidos
     */
    Page<FailedEmailNotification> findByStatusOrderByCreatedAtDesc(FailedEmailStatus status, Pageable pageable);

    /**
     * Busca emails fallidos por tipo de notificación.
     * 
     * @param notificationType Tipo de notificación
     * @param pageable Paginación
     * @return Página de emails fallidos
     */
    Page<FailedEmailNotification> findByNotificationTypeOrderByCreatedAtDesc(EmailNotificationType notificationType, Pageable pageable);

    /**
     * Busca emails fallidos con filtros múltiples.
     * 
     * @param status Estado opcional (null para todos)
     * @param notificationType Tipo opcional (null para todos)
     * @param pageable Paginación
     * @return Página de emails fallidos
     */
    @Query("SELECT f FROM FailedEmailNotification f WHERE " +
           "(:status IS NULL OR f.status = :status) " +
           "AND (:notificationType IS NULL OR f.notificationType = :notificationType) " +
           "ORDER BY f.createdAt DESC")
    Page<FailedEmailNotification> findByFilters(
        @Param("status") FailedEmailStatus status,
        @Param("notificationType") EmailNotificationType notificationType,
        Pageable pageable);


    /**
     * Cuenta emails fallidos por estado.
     * 
     * @param status Estado del email fallido
     * @return Cantidad de emails fallidos
     */
    long countByStatus(FailedEmailStatus status);

    /**
     * Busca emails fallidos que necesitan reintento automático.
     * Emails que:
     * - Están en estado FAILED
     * - Tienen menos de 5 intentos
     * - No se han intentado reenviar en la última hora
     * 
     * @param oneHourAgo Hora hace una hora
     * @return Lista de emails que pueden reintentarse
     */
    @Query("SELECT f FROM FailedEmailNotification f WHERE " +
           "f.status = 'FAILED' " +
           "AND f.retryCount < 5 " +
           "AND (f.lastRetryAt IS NULL OR f.lastRetryAt < :oneHourAgo) " +
           "ORDER BY f.createdAt ASC")
    List<FailedEmailNotification> findFailedEmailsForRetry(@Param("oneHourAgo") LocalDateTime oneHourAgo);
}

