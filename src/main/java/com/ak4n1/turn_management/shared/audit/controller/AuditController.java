package com.ak4n1.turn_management.shared.audit.controller;

import com.ak4n1.turn_management.shared.audit.dto.AuditLogsResponse;
import com.ak4n1.turn_management.shared.audit.service.AuditService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

/**
 * Controller para consultar logs de auditoría.
 * 
 * Implementa US-T018.
 */
@RestController
@RequestMapping("/api/admin/audit")
public class AuditController {

    private static final Logger logger = LoggerFactory.getLogger(AuditController.class);

    private final AuditService auditService;

    public AuditController(AuditService auditService) {
        this.auditService = auditService;
    }

    /**
     * Consulta logs de auditoría con filtros opcionales (solo admin).
     * 
     * GET /api/admin/audit
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T018.
     * 
     * @param actionType Filtro opcional por tipo de acción (CANCELLATION, CONFIGURATION_CHANGE, EXCEPTION, BLOCK, RESCHEDULE_REQUEST)
     * @param userId Filtro opcional por ID de usuario
     * @param startDate Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param endDate Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20, máximo: 100)
     * @return Lista paginada de logs de auditoría (200 OK)
     */
    @GetMapping
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<AuditLogsResponse> getAuditLogs(
            @RequestParam(required = false) String actionType,
            @RequestParam(required = false) Long userId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("Consultando logs de auditoría - Tipo: {}, Usuario: {}, Desde: {}, Hasta: {}, Página: {}, Tamaño: {}",
            actionType, userId, startDate, endDate, page, size);

        AuditLogsResponse response = auditService.getAuditLogs(actionType, userId, startDate, endDate, page, size);

        logger.info("Logs encontrados - Total: {}, Página: {}, Tamaño: {}", response.getTotalElements(), page, size);

        return ResponseEntity.ok(response);
    }
}

