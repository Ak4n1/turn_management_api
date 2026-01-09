package com.ak4n1.turn_management.shared.scheduling;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDateTime;
import java.util.Optional;

/**
 * Servicio para gestión de locks distribuidos.
 * 
 * Permite que solo una instancia de la aplicación ejecute un job
 * cuando hay múltiples instancias corriendo en un cluster.
 */
@Service
public class DistributedLockService {

    private static final Logger logger = LoggerFactory.getLogger(DistributedLockService.class);
    private static final int DEFAULT_LOCK_TTL_MINUTES = 10; // TTL por defecto: 10 minutos

    private final DistributedLockRepository lockRepository;
    private final String instanceId;

    public DistributedLockService(DistributedLockRepository lockRepository) {
        this.lockRepository = lockRepository;
        this.instanceId = generateInstanceId();
    }

    /**
     * Genera un identificador único para esta instancia.
     * 
     * @return Identificador único (hostname + timestamp)
     */
    private String generateInstanceId() {
        try {
            String hostname = InetAddress.getLocalHost().getHostName();
            return hostname + "-" + System.currentTimeMillis();
        } catch (UnknownHostException e) {
            return "unknown-" + System.currentTimeMillis();
        }
    }

    /**
     * Intenta adquirir un lock distribuido.
     * 
     * @param lockKey Clave única del lock
     * @param ttlMinutes Tiempo de vida del lock en minutos (por defecto: 10 minutos)
     * @return true si se adquirió el lock, false si ya está adquirido por otra instancia
     */
    @Transactional
    public boolean tryAcquireLock(String lockKey, int ttlMinutes) {
        try {
            // Primero, limpiar locks expirados
            int cleaned = lockRepository.cleanupExpiredLocks(LocalDateTime.now());
            if (cleaned > 0) {
                logger.debug("Limpieza de locks expirados: {} locks eliminados", cleaned);
            }

            // Intentar adquirir el lock
            LocalDateTime now = LocalDateTime.now();
            LocalDateTime expiresAt = now.plusMinutes(ttlMinutes);

            // Verificar si el lock existe y no ha expirado
            Optional<DistributedLock> existingLock = lockRepository.findByLockKey(lockKey);
            if (existingLock.isPresent()) {
                DistributedLock lock = existingLock.get();
                if (!lock.isExpired() && !lock.getLockedBy().equals(instanceId)) {
                    logger.debug("Lock '{}' ya está adquirido por otra instancia: {} (expira: {})", 
                        lockKey, lock.getLockedBy(), lock.getExpiresAt());
                    return false;
                }
                // Si el lock expiró o es nuestro, podemos actualizarlo
                if (lock.isExpired()) {
                    logger.debug("Lock '{}' expirado, adquiriendo nuevo lock", lockKey);
                    lockRepository.delete(lock); // Eliminar lock expirado
                } else if (lock.getLockedBy().equals(instanceId)) {
                    // Ya es nuestro, actualizar expiración
                    lock.setExpiresAt(expiresAt);
                    lock.setLockedAt(now);
                    lockRepository.save(lock);
                    logger.debug("Lock '{}' renovado por instancia: {}", lockKey, instanceId);
                    return true;
                }
            }

            // Crear nuevo lock
            DistributedLock lock = new DistributedLock(lockKey, instanceId, expiresAt);
            lockRepository.save(lock);

            logger.debug("Lock '{}' adquirido exitosamente por instancia: {} (expira: {})", 
                lockKey, instanceId, expiresAt);
            return true;

        } catch (Exception e) {
            logger.error("Error al intentar adquirir lock '{}': {}", lockKey, e.getMessage(), e);
            return false;
        }
    }

    /**
     * Intenta adquirir un lock distribuido con TTL por defecto (10 minutos).
     * 
     * @param lockKey Clave única del lock
     * @return true si se adquirió el lock, false si ya está adquirido por otra instancia
     */
    @Transactional
    public boolean tryAcquireLock(String lockKey) {
        return tryAcquireLock(lockKey, DEFAULT_LOCK_TTL_MINUTES);
    }

    /**
     * Libera un lock distribuido.
     * 
     * @param lockKey Clave única del lock
     * @return true si se liberó el lock, false si no era nuestro
     */
    @Transactional
    public boolean releaseLock(String lockKey) {
        try {
            int released = lockRepository.releaseLock(lockKey, instanceId);
            if (released > 0) {
                logger.debug("Lock '{}' liberado exitosamente por instancia: {}", lockKey, instanceId);
                return true;
            } else {
                logger.debug("Lock '{}' no pudo ser liberado (no era nuestro o no existe)", lockKey);
                return false;
            }
        } catch (Exception e) {
            logger.error("Error al liberar lock '{}': {}", lockKey, e.getMessage(), e);
            return false;
        }
    }

    /**
     * Verifica si un lock está adquirido por esta instancia.
     * 
     * @param lockKey Clave única del lock
     * @return true si el lock está adquirido por esta instancia, false en caso contrario
     */
    public boolean isLockedByMe(String lockKey) {
        Optional<DistributedLock> lock = lockRepository.findByLockKey(lockKey);
        return lock.isPresent() 
            && !lock.get().isExpired() 
            && lock.get().getLockedBy().equals(instanceId);
    }
}

