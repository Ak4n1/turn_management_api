package com.ak4n1.turn_management.shared.scheduling;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * Entidad para locks distribuidos en jobs programados.
 * 
 * Permite que solo una instancia de la aplicación ejecute un job
 * cuando hay múltiples instancias corriendo en un cluster.
 */
@Entity
@Table(name = "distributed_locks", indexes = {
    @Index(name = "idx_lock_key", columnList = "lock_key", unique = true)
})
public class DistributedLock {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Clave única del lock (ej: "appointment-reminders").
     */
    @Column(name = "lock_key", nullable = false, unique = true, length = 255)
    private String lockKey;

    /**
     * Instancia que tiene el lock (hostname o identificador único).
     */
    @Column(name = "locked_by", nullable = false, length = 255)
    private String lockedBy;

    /**
     * Fecha y hora en que se adquirió el lock.
     */
    @Column(name = "locked_at", nullable = false)
    private LocalDateTime lockedAt;

    /**
     * Fecha y hora de expiración del lock (TTL).
     * Si el lock no se libera antes de esta fecha, se considera expirado.
     */
    @Column(name = "expires_at", nullable = false)
    private LocalDateTime expiresAt;

    public DistributedLock() {
    }

    public DistributedLock(String lockKey, String lockedBy, LocalDateTime expiresAt) {
        this.lockKey = lockKey;
        this.lockedBy = lockedBy;
        this.lockedAt = LocalDateTime.now();
        this.expiresAt = expiresAt;
    }

    /**
     * Verifica si el lock ha expirado.
     * 
     * @return true si el lock ha expirado, false en caso contrario
     */
    public boolean isExpired() {
        return LocalDateTime.now().isAfter(expiresAt);
    }

    // Getters and Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getLockKey() {
        return lockKey;
    }

    public void setLockKey(String lockKey) {
        this.lockKey = lockKey;
    }

    public String getLockedBy() {
        return lockedBy;
    }

    public void setLockedBy(String lockedBy) {
        this.lockedBy = lockedBy;
    }

    public LocalDateTime getLockedAt() {
        return lockedAt;
    }

    public void setLockedAt(LocalDateTime lockedAt) {
        this.lockedAt = lockedAt;
    }

    public LocalDateTime getExpiresAt() {
        return expiresAt;
    }

    public void setExpiresAt(LocalDateTime expiresAt) {
        this.expiresAt = expiresAt;
    }
}

