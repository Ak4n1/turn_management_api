package com.ak4n1.turn_management.feature.notification.repository;

import com.ak4n1.turn_management.feature.notification.domain.EmailTemplateEntity;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repositorio para gestión de plantillas de email.
 */
@Repository
public interface EmailTemplateRepository extends JpaRepository<EmailTemplateEntity, Long> {

    /**
     * Busca la plantilla activa más reciente de un tipo específico.
     * 
     * @param templateType Tipo de plantilla
     * @return Plantilla activa más reciente o vacío
     */
    @Query("SELECT e FROM EmailTemplateEntity e WHERE e.templateType = :templateType AND e.active = true ORDER BY e.version DESC")
    Optional<EmailTemplateEntity> findActiveByType(@Param("templateType") EmailTemplateType templateType);

    /**
     * Busca todas las plantillas de un tipo específico, ordenadas por versión descendente.
     * 
     * @param templateType Tipo de plantilla
     * @return Lista de plantillas ordenadas por versión
     */
    List<EmailTemplateEntity> findByTemplateTypeOrderByVersionDesc(EmailTemplateType templateType);

    /**
     * Busca todas las plantillas activas.
     * 
     * @return Lista de plantillas activas
     */
    List<EmailTemplateEntity> findByActiveTrueOrderByTemplateTypeAscVersionDesc();

    /**
     * Cuenta cuántas plantillas activas hay de un tipo específico.
     * 
     * @param templateType Tipo de plantilla
     * @return Cantidad de plantillas activas
     */
    long countByTemplateTypeAndActiveTrue(EmailTemplateType templateType);
}

