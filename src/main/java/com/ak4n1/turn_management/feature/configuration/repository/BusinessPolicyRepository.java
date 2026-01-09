package com.ak4n1.turn_management.feature.configuration.repository;

import com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * Repositorio para gestión de políticas de negocio.
 * 
 * Implementa US-T019.
 */
@Repository
public interface BusinessPolicyRepository extends JpaRepository<BusinessPolicy, Long> {

    /**
     * Busca la política activa.
     * Solo debe haber una política activa a la vez.
     */
    Optional<BusinessPolicy> findByActiveTrue();

    /**
     * Cuenta cuántas políticas activas existen.
     * Debe ser 0 o 1.
     */
    long countByActiveTrue();
}

