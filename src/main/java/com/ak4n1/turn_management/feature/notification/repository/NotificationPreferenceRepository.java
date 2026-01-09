package com.ak4n1.turn_management.feature.notification.repository;

import com.ak4n1.turn_management.feature.notification.domain.NotificationPreference;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * Repositorio para gestión de preferencias de notificación.
 */
@Repository
public interface NotificationPreferenceRepository extends JpaRepository<NotificationPreference, Long> {

    /**
     * Busca las preferencias de un usuario específico.
     * 
     * @param userId ID del usuario
     * @return Preferencias del usuario o vacío si no existen
     */
    Optional<NotificationPreference> findByUserId(Long userId);
}

