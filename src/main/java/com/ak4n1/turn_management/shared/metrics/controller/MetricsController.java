package com.ak4n1.turn_management.shared.metrics.controller;

import com.ak4n1.turn_management.shared.metrics.dto.SystemMetricsResponse;
import com.ak4n1.turn_management.shared.metrics.service.MetricsService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

/**
 * Controller para consultar métricas del sistema.
 * 
 * Implementa US-T022.
 */
@RestController
@RequestMapping("/api/admin/metrics")
public class MetricsController {

    private static final Logger logger = LoggerFactory.getLogger(MetricsController.class);

    private final MetricsService metricsService;

    public MetricsController(MetricsService metricsService) {
        this.metricsService = metricsService;
    }

    /**
     * Consulta métricas del sistema para un rango de fechas (solo admin).
     * 
     * GET /api/admin/metrics
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T022.
     * 
     * @param startDate Fecha de inicio (formato: yyyy-MM-dd, requerido)
     * @param endDate Fecha de fin (formato: yyyy-MM-dd, requerido)
     * @return Métricas del sistema para el período especificado (200 OK)
     */
    @GetMapping
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<SystemMetricsResponse> getSystemMetrics(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {
        
        logger.info("Consultando métricas del sistema - Desde: {}, Hasta: {}", startDate, endDate);

        SystemMetricsResponse response = metricsService.getSystemMetrics(startDate, endDate);

        logger.info("Métricas calculadas - Turnos creados: {}, Cancelados: {}, No-shows: {}",
            response.getAppointments().getCreated(),
            response.getAppointments().getCancelled(),
            response.getAppointments().getNoShow());

        return ResponseEntity.ok(response);
    }
}

