package com.ak4n1.turn_management.feature.configuration.controller;

import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.configuration.dto.request.AppointmentDurationRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.ManualBlockRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.PreviewImpactRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.WeeklyConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarConfigurationResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarExceptionResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationHistoryResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedCalendarResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ManualBlockResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.PreviewImpactResponse;
import com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService;
import com.ak4n1.turn_management.feature.configuration.service.CalendarExceptionService;
import com.ak4n1.turn_management.feature.configuration.service.ManualBlockService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;

/**
 * Controller para gestión de configuraciones de calendario.
 * 
 * Endpoints protegidos con rol ADMIN.
 */
@RestController
@RequestMapping("/api/admin/calendar")
public class CalendarConfigurationController {

    private static final Logger logger = LoggerFactory.getLogger(CalendarConfigurationController.class);

    private final CalendarConfigurationService configurationService;
    private final CalendarExceptionService exceptionService;
    private final ManualBlockService manualBlockService;
    private final UserService userService;
    private final JwtTokenProvider jwtTokenProvider;

    public CalendarConfigurationController(
            CalendarConfigurationService configurationService,
            CalendarExceptionService exceptionService,
            ManualBlockService manualBlockService,
            UserService userService,
            JwtTokenProvider jwtTokenProvider) {
        this.configurationService = configurationService;
        this.exceptionService = exceptionService;
        this.manualBlockService = manualBlockService;
        this.userService = userService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * Crea una nueva configuración semanal base.
     * 
     * POST /api/admin/calendar/weekly-config
     * 
     * Requiere: Rol ADMIN
     * 
     * @param request DTO con la configuración semanal
     * @param httpRequest Request HTTP para obtener el token
     * @return Configuración creada (201 Created)
     */
    @PostMapping("/weekly-config")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<CalendarConfigurationResponse> createWeeklyConfig(
            @Valid @RequestBody WeeklyConfigRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud de creación de configuración semanal recibida");
        
        // Obtener usuario autenticado
        Long userId = getCurrentUserId(httpRequest);
        
        // Crear configuración
        CalendarConfigurationResponse response = configurationService.createWeeklyConfig(request, userId);
        
        logger.info("Configuración semanal creada exitosamente - Versión: {}", response.getVersion());
        
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Configura horarios diarios para la configuración activa.
     * 
     * POST /api/admin/calendar/daily-hours
     * 
     * Requiere: Rol ADMIN
     * 
     * Body ejemplo:
     * {
     *   "dailyHours": {
     *     "monday": [
     *       {"start": "09:00", "end": "12:00"},
     *       {"start": "14:00", "end": "18:00"}
     *     ],
     *     "friday": [
     *       {"start": "09:00", "end": "16:00"}
     *     ]
     *   },
     *   "notes": "Configuración de horarios"
     * }
     * 
     * @param request DTO con los horarios diarios
     * @param httpRequest Request HTTP para obtener el token
     * @return Configuración actualizada (200 OK)
     */
    @PostMapping("/daily-hours")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<CalendarConfigurationResponse> configureDailyHours(
            @Valid @RequestBody DailyHoursConfigRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud de configuración de horarios diarios recibida");
        
        // Obtener usuario autenticado
        Long userId = getCurrentUserId(httpRequest);
        
        // Configurar horarios
        CalendarConfigurationResponse response = configurationService.configureDailyHours(request, userId);
        
        logger.info("Horarios diarios configurados exitosamente - Versión: {}", response.getVersion());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Configura la duración de los turnos para la configuración activa.
     * 
     * POST /api/admin/calendar/appointment-duration
     * 
     * Requiere: Rol ADMIN
     * 
     * Body ejemplo:
     * {
     *   "durationMinutes": 30,
     *   "notes": "Turnos de 30 minutos"
     * }
     * 
     * Validaciones:
     * - Duración debe estar entre 15 y 240 minutos
     * - Duración debe ser divisible por 15
     * - Duración debe ser compatible con los rangos horarios configurados
     * 
     * @param request DTO con la duración en minutos
     * @param httpRequest Request HTTP para obtener el token
     * @return Configuración actualizada (200 OK)
     */
    @PostMapping("/appointment-duration")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<CalendarConfigurationResponse> configureAppointmentDuration(
            @Valid @RequestBody AppointmentDurationRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud de configuración de duración de turnos recibida - Duración: {} minutos", request.getDurationMinutes());
        
        // Obtener usuario autenticado
        Long userId = getCurrentUserId(httpRequest);
        
        // Configurar duración
        CalendarConfigurationResponse response = configurationService.configureAppointmentDuration(request, userId);
        
        logger.info("Duración de turnos configurada exitosamente - Versión: {}, Duración: {} minutos", 
            response.getVersion(), response.getAppointmentDurationMinutes());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Crea una excepción de calendario por fecha específica.
     * 
     * POST /api/admin/calendar/exceptions
     * 
     * Requiere: Rol ADMIN
     * 
     * Body ejemplo:
     * {
     *   "date": "2024-04-20",
     *   "isOpen": true,
     *   "timeRanges": [
     *     {"start": "10:00", "end": "14:00"}
     *   ],
     *   "reason": "Sábado especial"
     * }
     * 
     * @param request DTO con los datos de la excepción
     * @param httpRequest Request HTTP para obtener el token
     * @return Excepción creada con impacto calculado (201 Created)
     */
    @PostMapping("/exceptions")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<CalendarExceptionResponse> createException(
            @Valid @RequestBody CalendarExceptionRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud de creación de excepción de calendario recibida - Fecha: {}, IsOpen: {}", 
            request.getDate(), request.getIsOpen());
        
        // Obtener usuario autenticado
        Long userId = getCurrentUserId(httpRequest);
        
        // Crear excepción
        CalendarExceptionResponse response = exceptionService.createException(request, userId);
        
        logger.info("Excepción de calendario creada exitosamente - ID: {}, Fecha: {}, Turnos afectados: {}", 
            response.getId(), response.getExceptionDate(), response.getAffectedAppointmentsCount());
        
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Obtiene todas las excepciones activas del calendario.
     * 
     * GET /api/admin/calendar/exceptions
     * 
     * Requiere: Rol ADMIN
     * 
     * @return Lista de excepciones activas ordenadas por fecha (200 OK)
     */
    @GetMapping("/exceptions")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<List<CalendarExceptionResponse>> getAllActiveExceptions() {
        logger.info("Solicitud de obtención de excepciones activas");
        
        List<CalendarExceptionResponse> exceptions = exceptionService.getAllActiveExceptions();
        
        logger.info("Se retornan {} excepciones activas", exceptions.size());
        return ResponseEntity.ok(exceptions);
    }

    /**
     * Actualiza una excepción existente.
     * PUT /api/admin/calendar/exceptions/{id}
     */
    @PutMapping("/exceptions/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<CalendarExceptionResponse> updateException(
            @PathVariable Long id,
            @Valid @RequestBody CalendarExceptionRequest request,
            HttpServletRequest httpRequest) {
        logger.info("Solicitud de actualización de excepción - ID: {}", id);
        Long userId = getCurrentUserId(httpRequest);
        CalendarExceptionResponse response = exceptionService.updateException(id, request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Elimina (desactiva) una excepción.
     * DELETE /api/admin/calendar/exceptions/{id}
     */
    @DeleteMapping("/exceptions/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> deleteException(@PathVariable Long id) {
        logger.info("Solicitud de eliminación de excepción - ID: {}", id);
        exceptionService.deactivateException(id);
        return ResponseEntity.noContent().build();
    }

    /**
     * Obtiene todos los bloqueos activos.
     *
     * GET /api/admin/calendar/blocks
     *
     * Requiere: Rol ADMIN
     */
    @GetMapping("/blocks")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<List<ManualBlockResponse>> getActiveBlocks() {
        logger.info("Solicitud de obtención de bloqueos activos");
        List<ManualBlockResponse> blocks = manualBlockService.getActiveBlocks();
        logger.info("Se retornan {} bloqueos activos", blocks.size());
        return ResponseEntity.ok(blocks);
    }

    /**
     * Crea un bloqueo operativo del calendario.
     *
     * POST /api/admin/calendar/blocks
     * 
     * Requiere: Rol ADMIN
     * 
     * Body ejemplo (día completo):
     * {
     *   "date": "2024-04-25",
     *   "isFullDay": true,
     *   "timeRange": null,
     *   "reason": "Mantenimiento programado",
     *   "affectsExistingAppointments": false
     * }
     * 
     * Body ejemplo (rango horario):
     * {
     *   "date": "2024-04-25",
     *   "isFullDay": false,
     *   "timeRange": {"start": "14:00", "end": "16:00"},
     *   "reason": "Reunión interna importante",
     *   "affectsExistingAppointments": false
     * }
     * 
     * @param request DTO con los datos del bloqueo
     * @param httpRequest Request HTTP para obtener el token
     * @return Bloqueo creado con lista de turnos afectados (201 Created)
     */
    @PostMapping("/blocks")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ManualBlockResponse> createBlock(
            @Valid @RequestBody ManualBlockRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Solicitud de creación de bloqueo operativo recibida - Fecha: {}, IsFullDay: {}", 
            request.getDate(), request.getIsFullDay());
        
        // Obtener usuario autenticado
        Long userId = getCurrentUserId(httpRequest);
        
        // Crear bloqueo
        ManualBlockResponse response = manualBlockService.createBlock(request, userId);
        
        logger.info("Bloqueo operativo creado exitosamente - ID: {}, Fecha: {}, Turnos afectados: {}", 
            response.getId(), response.getBlockDate(), 
            response.getAffectedAppointments() != null ? response.getAffectedAppointments().size() : 0);
        
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Actualiza un bloqueo operativo existente.
     *
     * PUT /api/admin/calendar/blocks/{id}
     *
     * Requiere: Rol ADMIN
     *
     * @param id ID del bloqueo
     * @param request DTO con los datos actualizados
     * @param httpRequest Request HTTP para obtener el token
     * @return Bloqueo actualizado (200 OK)
     */
    @PutMapping("/blocks/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ManualBlockResponse> updateBlock(
            @PathVariable Long id,
            @Valid @RequestBody ManualBlockRequest request,
            HttpServletRequest httpRequest) {
        logger.info("Solicitud de actualización de bloqueo - ID: {}, Fecha: {}", id, request.getDate());
        Long userId = getCurrentUserId(httpRequest);
        ManualBlockResponse response = manualBlockService.updateBlock(id, request, userId);
        logger.info("Bloqueo actualizado exitosamente - ID: {}", id);
        return ResponseEntity.ok(response);
    }

    /**
     * Elimina (desactiva) un bloqueo operativo.
     *
     * DELETE /api/admin/calendar/blocks/{id}
     *
     * Requiere: Rol ADMIN
     *
     * @param id ID del bloqueo
     * @return 204 No Content
     */
    @DeleteMapping("/blocks/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> deleteBlock(@PathVariable Long id) {
        logger.info("Solicitud de eliminación de bloqueo - ID: {}", id);
        manualBlockService.deactivateBlock(id);
        return ResponseEntity.noContent().build();
    }

    /**
     * Previsualiza el impacto de cambios propuestos sin aplicarlos.
     * 
     * POST /api/admin/calendar/preview-impact
     * 
     * Requiere: Rol ADMIN
     * 
     * Calcula el impacto de cambios propuestos:
     * - Días afectados
     * - Slots que desaparecerían
     * - Turnos existentes impactados
     * 
     * Tipos de cambio soportados:
     * - WEEKLY_CONFIG: Cambio en configuración semanal
     * - DAILY_HOURS: Cambio en horarios diarios
     * - APPOINTMENT_DURATION: Cambio en duración de turnos
     * - EXCEPTION: Nueva excepción
     * - BLOCK: Nuevo bloqueo
     * 
     * @param request DTO con los cambios propuestos
     * @return Información del impacto calculado (200 OK)
     */
    @PostMapping("/preview-impact")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<PreviewImpactResponse> previewImpact(
            @Valid @RequestBody PreviewImpactRequest request) {
        
        logger.info("Solicitud de previsualización de impacto recibida - Tipo: {}", request.getChangeType());
        
        PreviewImpactResponse response = configurationService.previewImpact(request);
        
        logger.info("Previsualización de impacto completada - Días afectados: {}, Slots perdidos: {}",
            response.getAffectedDays(), response.getSlotsLost());
        
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el calendario consolidado para un rango de fechas.
     * 
     * GET /api/admin/calendar/consolidated?startDate=2024-04-01&endDate=2024-04-30
     * 
     * Requiere: Rol ADMIN
     * 
     * El calendario consolidado muestra para cada día:
     * - Qué regla aplica (base, excepción o bloqueo)
     * - Estado del día (abierto, cerrado, parcial)
     * - Descripción explicativa
     * - Rangos horarios disponibles (si aplica)
     * - Indicador de turnos existentes
     * 
     * @param startDate Fecha de inicio del rango (formato: YYYY-MM-DD)
     * @param endDate Fecha de fin del rango (formato: YYYY-MM-DD)
     * @return Calendario consolidado (200 OK)
     */
    @GetMapping("/consolidated")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ConsolidatedCalendarResponse> getConsolidatedCalendar(
            @RequestParam("startDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam("endDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {
        
        logger.info("Solicitud de calendario consolidado recibida - Rango: {} a {}", startDate, endDate);
        
        ConsolidatedCalendarResponse response = configurationService.getConsolidatedCalendar(startDate, endDate);
        
        logger.info("Calendario consolidado generado exitosamente - {} días", 
            response.getDays() != null ? response.getDays().size() : 0);
        
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene la configuración activa actual.
     * 
     * GET /api/admin/calendar/active
     * 
     * Requiere: Rol ADMIN
     * 
     * @return Configuración activa (200 OK) o 404 si no existe
     */
    @GetMapping("/active")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<CalendarConfigurationResponse> getActiveConfiguration() {
        logger.info("Solicitud de configuración activa recibida");
        
        CalendarConfigurationResponse response = configurationService.getActiveConfiguration();
        
        if (response == null) {
            logger.warn("No existe configuración activa");
            return ResponseEntity.notFound().build();
        }
        
        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el ID del usuario autenticado.
     * 
     * @param request Request HTTP para obtener el token
     * @return ID del usuario
     * @throws ApiException si no hay usuario autenticado o no se puede obtener el ID
     */
    private Long getCurrentUserId(HttpServletRequest request) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new ApiException("Usuario no autenticado", HttpStatus.UNAUTHORIZED);
        }

        // Intentar obtener userId del token JWT
        String token = getAccessTokenFromRequest(request);
        if (token != null) {
            try {
                Long userId = jwtTokenProvider.extractUserId(token);
                if (userId != null) {
                    return userId;
                }
            } catch (Exception e) {
                logger.warn("No se pudo extraer userId del token: {}", e.getMessage());
            }
        }

        // Fallback: obtener desde UserDetails
        Object principal = authentication.getPrincipal();
        if (principal instanceof UserDetails) {
            String email = ((UserDetails) principal).getUsername();
            try {
                return userService.findByEmail(email)
                    .orElseThrow(() -> new ApiException("Usuario no encontrado", HttpStatus.NOT_FOUND))
                    .getId();
            } catch (Exception e) {
                logger.warn("No se pudo obtener userId desde email: {}", e.getMessage());
            }
        }
        
        throw new ApiException(
            "No se pudo obtener el ID del usuario autenticado",
            HttpStatus.INTERNAL_SERVER_ERROR);
    }

    /**
     * Obtiene el token de acceso desde cookies o Authorization header.
     */
    private String getAccessTokenFromRequest(HttpServletRequest request) {
        // Intentar desde cookies
        jakarta.servlet.http.Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (jakarta.servlet.http.Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }
        
        // Fallback: Authorization header
        String authHeader = request.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }
        
        return null;
    }

    /**
     * Consulta el historial completo de cambios en la configuración del calendario (solo admin).
     * 
     * GET /api/admin/calendar/configuration/history
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T018.1.
     * 
     * @return Historial completo de configuraciones (200 OK)
     */
    @GetMapping("/configuration/history")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ConfigurationHistoryResponse> getConfigurationHistory() {
        logger.info("Consultando historial de configuraciones del calendario (admin)");

        ConfigurationHistoryResponse response = configurationService.getConfigurationHistory();

        logger.info("Historial encontrado - Total versiones: {}", response.getTotalVersions());

        return ResponseEntity.ok(response);
    }
}

