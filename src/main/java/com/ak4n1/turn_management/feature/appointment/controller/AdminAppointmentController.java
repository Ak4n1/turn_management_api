package com.ak4n1.turn_management.feature.appointment.controller;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminAppointmentsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.request.AdminRescheduleAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CancelAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateOverrideAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminRescheduleRequestsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentHistoryResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentsCalendarResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.RescheduleRequestResponse;
import com.ak4n1.turn_management.feature.appointment.service.AppointmentService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;

/**
 * Controller para gestión de turnos y solicitudes de reprogramación por administradores.
 * 
 * Endpoints protegidos con rol ADMIN.
 */
@RestController
@RequestMapping("/api/admin/appointments")
@PreAuthorize("hasRole('ADMIN')")
public class AdminAppointmentController {

    private static final Logger logger = LoggerFactory.getLogger(AdminAppointmentController.class);

    private final AppointmentService appointmentService;
    private final JwtTokenProvider jwtTokenProvider;
    private final com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService configurationService;

    public AdminAppointmentController(AppointmentService appointmentService,
                                    JwtTokenProvider jwtTokenProvider,
                                    com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService configurationService) {
        this.appointmentService = appointmentService;
        this.jwtTokenProvider = jwtTokenProvider;
        this.configurationService = configurationService;
    }

    /**
     * Consulta todos los turnos con filtros (solo admin).
     * 
     * GET /api/admin/appointments
     * GET /api/admin/appointments?state=CONFIRMED&page=0&size=20
     * GET /api/admin/appointments?dateFrom=2026-01-01&dateTo=2026-01-31&page=0&size=20
     * GET /api/admin/appointments?userId=6&page=0&size=20
     * GET /api/admin/appointments?search=encabo&page=0&size=20
     * GET /api/admin/appointments?date=2026-02-15&page=0&size=20
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T017.
     * 
     * @param state Filtro opcional por estado
     * @param userId Filtro opcional por ID de usuario
     * @param search Filtro opcional de búsqueda por email o nombre (parcial)
     * @param dateFrom Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param dateTo Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param date Filtro opcional por fecha específica (formato: yyyy-MM-dd, mutuamente excluyente con dateFrom/dateTo)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20, máximo: 100)
     * @return Lista paginada de turnos con información de usuario (200 OK)
     */
    @GetMapping
    public ResponseEntity<AdminAppointmentsResponse> getAllAppointments(
            @RequestParam(required = false) String state,
            @RequestParam(required = false) Long userId,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String date,
            @RequestParam(required = false) String daysOfWeek,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("Consultando turnos (admin) - Estado: {}, Usuario: {}, Búsqueda: {}, Desde: {}, Hasta: {}, Fecha: {}, Días: {}, Página: {}, Tamaño: {}",
            state, userId, search, dateFrom, dateTo, date, daysOfWeek, page, size);

        // Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }

        // Parsear estado si se proporcionó
        AppointmentState appointmentState = null;
        if (state != null && !state.isBlank()) {
            try {
                appointmentState = AppointmentState.valueOf(state.toUpperCase());
            } catch (IllegalArgumentException e) {
                logger.warn("Estado inválido proporcionado: {}", state);
                throw new ApiException(
                    String.format("Estado inválido: %s. Estados válidos: CREATED, CONFIRMED, CANCELLED, EXPIRED, NO_SHOW, COMPLETED, RESCHEDULED", state),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Parsear fechas si se proporcionaron
        java.time.LocalDate fromDateParsed = null;
        if (dateFrom != null && !dateFrom.isBlank()) {
            try {
                fromDateParsed = java.time.LocalDate.parse(dateFrom);
            } catch (java.time.format.DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'dateFrom': %s. Formato esperado: yyyy-MM-dd", dateFrom),
                    HttpStatus.BAD_REQUEST);
            }
        }

        java.time.LocalDate toDateParsed = null;
        if (dateTo != null && !dateTo.isBlank()) {
            try {
                toDateParsed = java.time.LocalDate.parse(dateTo);
            } catch (java.time.format.DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'dateTo': %s. Formato esperado: yyyy-MM-dd", dateTo),
                    HttpStatus.BAD_REQUEST);
            }
        }

        java.time.LocalDate dateParsed = null;
        if (date != null && !date.isBlank()) {
            try {
                dateParsed = java.time.LocalDate.parse(date);
            } catch (java.time.format.DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'date': %s. Formato esperado: yyyy-MM-dd", date),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Validar que fromDate <= toDate si ambas están presentes
        if (fromDateParsed != null && toDateParsed != null && fromDateParsed.isAfter(toDateParsed)) {
            throw new ApiException(
                "La fecha 'dateFrom' no puede ser posterior a 'dateTo'",
                HttpStatus.BAD_REQUEST);
        }

        // Parsear días de la semana (formato: "1,2,3" o "1,2,3,4,5")
        java.util.List<Integer> daysOfWeekList = null;
        if (daysOfWeek != null && !daysOfWeek.isBlank()) {
            try {
                daysOfWeekList = java.util.Arrays.stream(daysOfWeek.split(","))
                    .map(String::trim)
                    .map(Integer::parseInt)
                    .filter(day -> day >= 1 && day <= 7) // Validar que esté entre 1 (Lunes) y 7 (Domingo)
                    .collect(java.util.stream.Collectors.toList());
                
                if (daysOfWeekList.isEmpty()) {
                    throw new ApiException(
                        "Los días de la semana deben estar entre 1 (Lunes) y 7 (Domingo). Formato: 1,2,3",
                        HttpStatus.BAD_REQUEST);
                }
            } catch (NumberFormatException e) {
                throw new ApiException(
                    "Formato inválido en 'daysOfWeek'. Formato esperado: 1,2,3 (donde 1=Lunes, 7=Domingo)",
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Llamar al servicio
        AdminAppointmentsResponse response = appointmentService.getAllAppointments(
            appointmentState, userId, search, fromDateParsed, toDateParsed, dateParsed, daysOfWeekList, page, size);

        logger.info("Turnos encontrados (admin) - Total: {}, Página: {}, Tamaño: {}, Total páginas: {}",
            response.getTotalElements(), response.getPage(), response.getSize(), response.getTotalPages());

        return ResponseEntity.ok(response);
    }

    /**
     * Consulta el calendario de turnos agendados para un rango de fechas (solo admin).
     * 
     * GET /api/admin/appointments/calendar?startDate=2026-02-01&endDate=2026-02-28
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T017.1.
     * 
     * @param startDate Fecha de inicio del rango (formato: yyyy-MM-dd, requerido)
     * @param endDate Fecha de fin del rango (formato: yyyy-MM-dd, requerido)
     * @return Calendario de turnos agrupados por día con slots (200 OK)
     */
    @GetMapping("/calendar")
    public ResponseEntity<AppointmentsCalendarResponse> getAppointmentsCalendar(
            @RequestParam(required = true) String startDate,
            @RequestParam(required = true) String endDate) {
        
        logger.info("Consultando calendario de turnos (admin) - Desde: {}, Hasta: {}", startDate, endDate);

        // Parsear fechas
        LocalDate startDateParsed;
        try {
            startDateParsed = LocalDate.parse(startDate);
        } catch (DateTimeParseException e) {
            throw new ApiException(
                String.format("Fecha inválida en 'startDate': %s. Formato esperado: yyyy-MM-dd", startDate),
                HttpStatus.BAD_REQUEST);
        }

        LocalDate endDateParsed;
        try {
            endDateParsed = LocalDate.parse(endDate);
        } catch (DateTimeParseException e) {
            throw new ApiException(
                String.format("Fecha inválida en 'endDate': %s. Formato esperado: yyyy-MM-dd", endDate),
                HttpStatus.BAD_REQUEST);
        }

        // Llamar al servicio
        AppointmentsCalendarResponse response = appointmentService.getAppointmentsCalendar(
            startDateParsed, endDateParsed);

        logger.info("Calendario generado - Días: {}, Total slots: {}, Disponibles: {}, Ocupados: {}",
            response.getTotalDays(), response.getTotalSlots(), 
            response.getTotalAvailableSlots(), response.getTotalOccupiedSlots());

        return ResponseEntity.ok(response);
    }

    /**
     * Consulta todas las solicitudes de reprogramación con filtros (solo admin).
     * 
     * GET /api/admin/appointments/reschedule-requests
     * GET /api/admin/appointments/reschedule-requests?status=PENDING_ADMIN_APPROVAL&page=0&size=20
     * GET /api/admin/appointments/reschedule-requests?userId=6&fromDate=2026-01-01&toDate=2026-01-31
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T014.3.
     * 
     * @param status Filtro opcional por estado
     * @param userId Filtro opcional por ID de usuario
     * @param fromDate Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param toDate Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20, máximo: 100)
     * @return Lista paginada de solicitudes con información de usuario (200 OK)
     */
    @GetMapping("/reschedule-requests")
    public ResponseEntity<AdminRescheduleRequestsResponse> getAllRescheduleRequests(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long userId,
            @RequestParam(required = false) String fromDate,
            @RequestParam(required = false) String toDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("Consultando solicitudes de reprogramación (admin) - Estado: {}, Usuario: {}, Desde: {}, Hasta: {}, Página: {}, Tamaño: {}",
            status, userId, fromDate, toDate, page, size);

        // Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }

        // Parsear estado si se proporcionó
        RescheduleRequestState state = null;
        if (status != null && !status.isBlank()) {
            try {
                state = RescheduleRequestState.valueOf(status.toUpperCase());
            } catch (IllegalArgumentException e) {
                logger.warn("Estado inválido proporcionado: {}", status);
                throw new ApiException(
                    String.format("Estado inválido: %s. Estados válidos: PENDING_ADMIN_APPROVAL, APPROVED, REJECTED, EXPIRED, CANCELLED", status),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Parsear fechas si se proporcionaron
        LocalDate fromDateParsed = null;
        if (fromDate != null && !fromDate.isBlank()) {
            try {
                fromDateParsed = LocalDate.parse(fromDate);
            } catch (DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'fromDate': %s. Formato esperado: yyyy-MM-dd", fromDate),
                    HttpStatus.BAD_REQUEST);
            }
        }

        LocalDate toDateParsed = null;
        if (toDate != null && !toDate.isBlank()) {
            try {
                toDateParsed = LocalDate.parse(toDate);
            } catch (DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'toDate': %s. Formato esperado: yyyy-MM-dd", toDate),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Validar que fromDate <= toDate si ambas están presentes
        if (fromDateParsed != null && toDateParsed != null && fromDateParsed.isAfter(toDateParsed)) {
            throw new ApiException(
                "La fecha 'fromDate' no puede ser posterior a 'toDate'",
                HttpStatus.BAD_REQUEST);
        }

        // Crear Pageable con ordenamiento por fecha de creación descendente
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));

        AdminRescheduleRequestsResponse response = appointmentService.getAllRescheduleRequests(
            state, userId, fromDateParsed, toDateParsed, pageable);

        logger.info("Solicitudes encontradas - Total: {}, Página: {}, Tamaño: {}, Total páginas: {}",
            response.getTotal(), response.getPage(), response.getSize(), response.getTotalPages());

        return ResponseEntity.ok(response);
    }

    /**
     * Aprueba una solicitud de reprogramación (solo admin).
     * 
     * POST /api/admin/appointments/reschedule-requests/{id}/approve
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T014.
     * 
     * @param id ID de la solicitud de reprogramación a aprobar
     * @param request HttpServletRequest para obtener el usuario autenticado
     * @return Solicitud aprobada con información del nuevo turno
     */
    @PostMapping("/reschedule-requests/{id}/approve")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<RescheduleRequestResponse> approveRescheduleRequest(
            @PathVariable Long id,
            HttpServletRequest request) {
        
        logger.info("Aprobación de solicitud de reprogramación - Solicitud ID: {}", id);

        Long adminUserId = getCurrentUserId(request);
        String clientIp = getClientIp(request);

        RescheduleRequestResponse response = appointmentService.approveRescheduleRequest(id, adminUserId, clientIp);

        logger.info("Solicitud de reprogramación aprobada exitosamente - Solicitud ID: {}, Admin: {}", id, adminUserId);

        return ResponseEntity.ok(response);
    }

    /**
     * Rechaza una solicitud de reprogramación (solo admin).
     * 
     * POST /api/admin/appointments/reschedule-requests/{id}/reject
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T014.
     * 
     * @param id ID de la solicitud de reprogramación a rechazar
     * @param rejectRequest DTO con motivo de rechazo (opcional)
     * @param request HttpServletRequest para obtener el usuario autenticado
     * @return Solicitud rechazada
     */
    @PostMapping("/reschedule-requests/{id}/reject")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<RescheduleRequestResponse> rejectRescheduleRequest(
            @PathVariable Long id,
            @RequestBody(required = false) com.ak4n1.turn_management.feature.appointment.dto.request.RejectRescheduleRequestRequest rejectRequest,
            HttpServletRequest request) {
        
        logger.info("Rechazo de solicitud de reprogramación - Solicitud ID: {}, Motivo: {}", 
            id, rejectRequest != null ? rejectRequest.getRejectionReason() : "N/A");

        Long adminUserId = getCurrentUserId(request);
        String clientIp = getClientIp(request);

        // Si no se proporciona request, crear uno vacío
        if (rejectRequest == null) {
            rejectRequest = new com.ak4n1.turn_management.feature.appointment.dto.request.RejectRescheduleRequestRequest();
        }

        RescheduleRequestResponse response = appointmentService.rejectRescheduleRequest(id, rejectRequest, adminUserId, clientIp);

        logger.info("Solicitud de reprogramación rechazada exitosamente - Solicitud ID: {}, Admin: {}", id, adminUserId);

        return ResponseEntity.ok(response);
    }

    /**
     * GET /api/admin/appointments/affected-by-closed-day
     * Obtiene turnos activos que están en un día cerrado según la configuración actual.
     * 
     * @param date Fecha a consultar
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20)
     * @return Lista paginada de turnos afectados con información completa
     */
    @GetMapping("/affected-by-closed-day")
    public ResponseEntity<AdminAppointmentsResponse> getAppointmentsAffectedByClosedDay(
            @RequestParam @org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE) LocalDate date,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("Solicitud de turnos afectados por día cerrado: {}, Página: {}, Tamaño: {}", date, page, size);
        
        // Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }
        
        // 1. Validar que el día está cerrado según la configuración actual
        com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse availability = 
            configurationService.checkAvailability(date);
        
        if (Boolean.TRUE.equals(availability.getIsAvailable())) {
            throw new ApiException(
                String.format("El día %s no está cerrado según la configuración actual.", date),
                HttpStatus.BAD_REQUEST);
        }
        
        // 2. Obtener turnos activos para esa fecha
        AdminAppointmentsResponse response = appointmentService.getAppointmentsAffectedByClosedDay(
            date, page, size);
        
        logger.info("Turnos afectados encontrados: {} de {} total para fecha {}", 
            response.getContent().size(), response.getTotalElements(), date);
        
        return ResponseEntity.ok(response);
    }

    /**
     * Exporta turnos en formato CSV o Excel (solo admin).
     * 
     * GET /api/admin/appointments/export?format=CSV&state=CONFIRMED&dateFrom=2026-01-01&dateTo=2026-01-31
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T017.3.
     * 
     * @param format Formato de exportación (CSV o XLSX, por ahora solo CSV)
     * @param state Filtro opcional por estado
     * @param userId Filtro opcional por ID de usuario
     * @param search Filtro opcional de búsqueda por email o nombre (parcial)
     * @param dateFrom Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param dateTo Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param date Filtro opcional por fecha específica (formato: yyyy-MM-dd, mutuamente excluyente con dateFrom/dateTo)
     * @return Archivo CSV para descarga (200 OK)
     */
    @GetMapping("/export")
    public ResponseEntity<byte[]> exportAppointments(
            @RequestParam(required = true) String format,
            @RequestParam(required = false) String state,
            @RequestParam(required = false) Long userId,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String date) {
        
        logger.info("Exportando turnos (admin) - Formato: {}, Estado: {}, Usuario: {}, Búsqueda: {}, Desde: {}, Hasta: {}, Fecha: {}",
            format, state, userId, search, dateFrom, dateTo, date);

        // Parsear estado si se proporcionó
        AppointmentState appointmentState = null;
        if (state != null && !state.isBlank()) {
            try {
                appointmentState = AppointmentState.valueOf(state.toUpperCase());
            } catch (IllegalArgumentException e) {
                logger.warn("Estado inválido proporcionado: {}", state);
                throw new ApiException(
                    String.format("Estado inválido: %s. Estados válidos: CREATED, CONFIRMED, CANCELLED, EXPIRED, NO_SHOW, COMPLETED, RESCHEDULED", state),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Parsear fechas si se proporcionaron
        java.time.LocalDate fromDateParsed = null;
        if (dateFrom != null && !dateFrom.isBlank()) {
            try {
                fromDateParsed = java.time.LocalDate.parse(dateFrom);
            } catch (java.time.format.DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'dateFrom': %s. Formato esperado: yyyy-MM-dd", dateFrom),
                    HttpStatus.BAD_REQUEST);
            }
        }

        java.time.LocalDate toDateParsed = null;
        if (dateTo != null && !dateTo.isBlank()) {
            try {
                toDateParsed = java.time.LocalDate.parse(dateTo);
            } catch (java.time.format.DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'dateTo': %s. Formato esperado: yyyy-MM-dd", dateTo),
                    HttpStatus.BAD_REQUEST);
            }
        }

        java.time.LocalDate dateParsed = null;
        if (date != null && !date.isBlank()) {
            try {
                dateParsed = java.time.LocalDate.parse(date);
            } catch (java.time.format.DateTimeParseException e) {
                throw new ApiException(
                    String.format("Fecha inválida en 'date': %s. Formato esperado: yyyy-MM-dd", date),
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Validar que fromDate <= toDate si ambas están presentes
        if (fromDateParsed != null && toDateParsed != null && fromDateParsed.isAfter(toDateParsed)) {
            throw new ApiException(
                "La fecha 'dateFrom' no puede ser posterior a 'dateTo'",
                HttpStatus.BAD_REQUEST);
        }

        // Llamar al servicio
        return appointmentService.exportAppointments(
            format, appointmentState, userId, search, fromDateParsed, toDateParsed, dateParsed);
    }

    /**
     * Consulta el historial completo de un turno (solo admin).
     * 
     * GET /api/admin/appointments/{id}/history
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T017.2.
     * 
     * @param id ID del turno
     * @return Historial completo del turno (200 OK)
     */
    @GetMapping("/{id}/history")
    public ResponseEntity<AppointmentHistoryResponse> getAppointmentHistoryForAdmin(
            @PathVariable Long id) {
        
        logger.info("Consultando historial de turno (admin) - ID: {}", id);

        AppointmentHistoryResponse response = appointmentService.getAppointmentHistoryForAdmin(id);

        logger.info("Historial encontrado - Turno ID: {}, Eventos: {}", id, response.getHistory().size());

        return ResponseEntity.ok(response);
    }

    /**
     * Marca un turno como no-show (ausente) - Solo admin.
     * 
     * POST /api/admin/appointments/{id}/mark-no-show
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T016.
     * 
     * @param appointmentId ID del turno a marcar como no-show
     * @param httpRequest HttpServletRequest para obtener adminUserId e IP del cliente
     * @return Turno marcado como no-show (200 OK)
     */
    @PostMapping("/{id}/mark-no-show")
    public ResponseEntity<AppointmentResponse> markAsNoShow(
            @PathVariable("id") Long appointmentId,
            HttpServletRequest httpRequest) {
        
        Long adminUserId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        logger.info("Solicitud de marcar turno como no-show - ID: {}, Admin: {}", 
            appointmentId, adminUserId);

        AppointmentResponse response = appointmentService.markAsNoShow(
            appointmentId, adminUserId, clientIp);

        logger.info("Turno marcado como no-show exitosamente - ID: {}, Admin: {}", 
            appointmentId, adminUserId);

        return ResponseEntity.ok(response);
    }

    /**
     * Cancela un turno directamente como administrador (solo admin).
     * 
     * POST /api/admin/appointments/{id}/cancel
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T024.
     * 
     * @param id ID del turno a cancelar
     * @param request Request con motivo
     * @param httpRequest HttpServletRequest para obtener IP del cliente
     * @return Turno cancelado (en estado CANCELLED_BY_ADMIN)
     */
    @PostMapping("/{id}/cancel")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<AppointmentResponse> cancelAppointment(
            @PathVariable Long id,
            @Valid @RequestBody CancelAppointmentRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Cancelación por admin - Turno ID: {}, Motivo: {}",
            id, request != null ? request.getReason() : "N/A");

        Long adminUserId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        AppointmentResponse response = appointmentService.cancelAppointmentByAdmin(
            id, request, adminUserId, clientIp);

        logger.info("Cancelación completada - Turno ID: {}, Estado: {}", id, response.getState());

        return ResponseEntity.ok(response);
    }

    /**
     * Crea un turno forzando las reglas normales (solo admin).
     * 
     * POST /api/admin/appointments/override
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T025.
     * 
     * @param request Request con fecha/hora, usuario y justificación
     * @param httpRequest HttpServletRequest para obtener IP del cliente
     * @return Turno creado (en estado CONFIRMED, con overridden = true)
     */
    @PostMapping("/override")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<AppointmentResponse> createAppointmentOverride(
            @Valid @RequestBody CreateOverrideAppointmentRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Creación forzada de turno por admin - Usuario: {}, Fecha: {}, Hora: {}, Justificación: {}",
            request.getUserId(), request.getDate(), request.getStartTime(), request.getJustification());

        Long adminUserId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        AppointmentResponse response = appointmentService.createAppointmentOverride(
            request, adminUserId, clientIp);

        logger.info("Turno forzado creado exitosamente - ID: {}, Usuario: {}", 
            response.getId(), response.getUserId());

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Reprograma un turno directamente como administrador (solo admin).
     * 
     * POST /api/admin/appointments/{id}/reschedule
     * 
     * Endpoint protegido (requiere rol ADMIN).
     * Implementa US-T023.
     * 
     * @param id ID del turno a reprogramar
     * @param request Request con nueva fecha/hora y motivo
     * @param httpRequest HttpServletRequest para obtener IP del cliente
     * @return Nuevo turno creado (en estado CONFIRMED)
     */
    @PostMapping("/{id}/reschedule")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<AppointmentResponse> rescheduleAppointment(
            @PathVariable Long id,
            @Valid @RequestBody AdminRescheduleAppointmentRequest request,
            HttpServletRequest httpRequest) {
        
        logger.info("Reprogramación por admin - Turno ID: {}, Nueva fecha: {}, Nueva hora: {}",
            id, request.getNewDate(), request.getNewStartTime());

        Long adminUserId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        AppointmentResponse response = appointmentService.rescheduleAppointmentByAdmin(
            id, request, adminUserId, clientIp);

        logger.info("Reprogramación completada - Nuevo turno ID: {}", response.getId());

        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el ID del usuario actual desde el token JWT.
     */
    private Long getCurrentUserId(HttpServletRequest request) {
        // Intentar obtener userId del token JWT
        String token = extractTokenFromRequest(request);
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

        throw new ApiException(
            "No se pudo obtener el ID del usuario autenticado",
            org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
    }

    /**
     * Extrae el token JWT desde las cookies o el header Authorization.
     */
    private String extractTokenFromRequest(HttpServletRequest request) {
        // Intentar obtener desde cookies (nombre: accessToken)
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        // Intentar obtener desde header Authorization: Bearer <token>
        String authHeader = request.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }

        return null;
    }

    /**
     * Obtiene la IP del cliente desde HttpServletRequest.
     */
    private String getClientIp(HttpServletRequest request) {
        String ipAddress = request.getHeader("X-Forwarded-For");
        if (ipAddress == null || ipAddress.isEmpty() || "unknown".equalsIgnoreCase(ipAddress)) {
            ipAddress = request.getHeader("X-Real-IP");
        }
        if (ipAddress == null || ipAddress.isEmpty() || "unknown".equalsIgnoreCase(ipAddress)) {
            ipAddress = request.getRemoteAddr();
        }
        // Si hay múltiples IPs (proxies), tomar la primera
        if (ipAddress != null && ipAddress.contains(",")) {
            ipAddress = ipAddress.split(",")[0].trim();
        }
        return ipAddress;
    }
}

