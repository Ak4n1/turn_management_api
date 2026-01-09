package com.ak4n1.turn_management.shared.scheduling;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.Optional;

/**
 * Repositorio para gestión de locks distribuidos.
 */
@Repository
public interface DistributedLockRepository extends JpaRepository<DistributedLock, Long> {

    /**
     * Busca un lock por su clave.
     * 
     * @param lockKey Clave del lock
     * @return Lock encontrado o vacío
     */
    Optional<DistributedLock> findByLockKey(String lockKey);


    /**
     * Libera un lock si es propiedad de la instancia especificada.
     * 
     * @param lockKey Clave del lock
     * @param lockedBy Identificador de la instancia que intenta liberar el lock
     * @return Número de filas afectadas (1 si se liberó, 0 si no)
     */
    @Modifying
    @Query("DELETE FROM DistributedLock d WHERE d.lockKey = :lockKey AND d.lockedBy = :lockedBy")
    int releaseLock(@Param("lockKey") String lockKey, @Param("lockedBy") String lockedBy);

    /**
     * Limpia locks expirados.
     * 
     * @param now Fecha/hora actual
     * @return Número de locks eliminados
     */
    @Modifying
    @Query("DELETE FROM DistributedLock d WHERE d.expiresAt < :now")
    int cleanupExpiredLocks(@Param("now") LocalDateTime now);
}

