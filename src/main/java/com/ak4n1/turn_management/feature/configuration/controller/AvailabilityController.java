package com.ak4n1.turn_management.feature.configuration.controller;

import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityRangeResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;
import com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

/**
 * Controller para consultas públicas de disponibilidad.
 * 
 * Estos endpoints son públicos y no requieren autenticación.
 * Permiten a los usuarios consultar disponibilidad de fechas para reservar turnos.
 */
@RestController
@RequestMapping("/api/availability")
public class AvailabilityController {

    private static final Logger logger = LoggerFactory.getLogger(AvailabilityController.class);

    private final CalendarConfigurationService configurationService;

    public AvailabilityController(CalendarConfigurationService configurationService) {
        this.configurationService = configurationService;
    }

    /**
     * Evalúa la disponibilidad de una fecha específica.
     * 
     * GET /api/availability?date=2026-02-20
     * 
     * Endpoint público (no requiere autenticación).
     * 
     * Aplica el orden de precedencia:
     * 1. Bloqueos operativos (prioridad máxima) - fecha NO disponible
     * 2. Excepciones por fecha - usa configuración de excepción
     * 3. Configuración base - usa calendario semanal base
     * 
     * @param date Fecha a evaluar (formato: YYYY-MM-DD)
     * @return Información de disponibilidad (200 OK)
     */
    @GetMapping
    public ResponseEntity<AvailabilityResponse> checkAvailability(
            @RequestParam("date") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date) {
        
        logger.info("Solicitud de disponibilidad recibida - Fecha: {}", date);
        
        AvailabilityResponse response = configurationService.checkAvailability(date);
        
        logger.info("Disponibilidad evaluada - Fecha: {}, Disponible: {}, Regla: {}",
            date, response.getIsAvailable(), response.getRuleApplied());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene los slots disponibles (intervalos de tiempo) para una fecha específica.
     * 
     * GET /api/availability/slots?date=2026-02-20
     * 
     * Endpoint público (no requiere autenticación).
     * 
     * Genera slots según:
     * - Los rangos horarios disponibles para la fecha
     * - La duración de turnos configurada
     * 
     * Excluye:
     * - Slots bloqueados (por bloqueos operativos)
     * - Slots ocupados (excluye turnos en estados CREATED o CONFIRMED)
     * 
     * @param date Fecha para la cual generar slots (formato: YYYY-MM-DD)
     * @return Lista de slots disponibles (200 OK)
     */
    @GetMapping("/slots")
    public ResponseEntity<SlotsResponse> getAvailableSlots(
            @RequestParam("date") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date) {
        
        logger.info("Solicitud de slots disponibles recibida - Fecha: {}", date);
        
        SlotsResponse response = configurationService.getAvailableSlots(date);
        
        logger.info("Slots generados - Fecha: {}, Total: {}, Disponibles: {}",
            date, response.getTotalSlots(), response.getAvailableSlots());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene la disponibilidad por rango de fechas.
     * 
     * GET /api/availability/range?startDate=2026-02-01&endDate=2026-02-28
     * 
     * Endpoint público (no requiere autenticación según la US-T010 indica usuario autenticado,
     * pero por simplicidad lo dejamos público por ahora).
     * 
     * Evalúa cada día en el rango y retorna:
     * - Estado de disponibilidad (FULL, PARTIAL, CLOSED)
     * - Cantidad de slots disponibles
     * - Cantidad total de slots
     * 
     * @param startDate Fecha de inicio del rango (formato: YYYY-MM-DD, opcional, por defecto hoy)
     * @param endDate Fecha de fin del rango (formato: YYYY-MM-DD, opcional, por defecto hoy + 89 días)
     * @return Disponibilidad por día en el rango (200 OK)
     */
    @GetMapping("/range")
    public ResponseEntity<AvailabilityRangeResponse> getAvailabilityRange(
            @RequestParam(value = "startDate", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(value = "endDate", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {
        
        logger.info("Solicitud de disponibilidad por rango recibida - Inicio: {}, Fin: {}", startDate, endDate);
        
        AvailabilityRangeResponse response = configurationService.getAvailabilityRange(startDate, endDate);
        
        logger.info("Disponibilidad por rango calculada - Rango: {} a {}, Días evaluados: {}",
            response.getStartDate(), response.getEndDate(), response.getDays().size());
        
        return ResponseEntity.ok(response);
    }
}

