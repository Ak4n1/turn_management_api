package com.ak4n1.turn_management.feature.appointment.controller;

import com.ak4n1.turn_management.feature.appointment.dto.request.CancelAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateRescheduleRequestRequest;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentHistoryResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.MyAppointmentsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.MyRescheduleRequestsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.RescheduleRequestResponse;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.appointment.service.AppointmentService;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.security.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para gestión de turnos (appointments) por usuarios.
 * 
 * Los endpoints requieren autenticación y permiten a los usuarios:
 * - Crear turnos
 * - Confirmar turnos
 * - Cancelar turnos
 * - Consultar sus propios turnos
 */
@RestController
@RequestMapping("/api/appointments")
public class AppointmentController {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentController.class);

    private final AppointmentService appointmentService;
    private final AppointmentRepository appointmentRepository;
    private final JwtTokenProvider jwtTokenProvider;
    private final UserService userService;

    public AppointmentController(AppointmentService appointmentService,
                                AppointmentRepository appointmentRepository,
                                JwtTokenProvider jwtTokenProvider,
                                UserService userService) {
        this.appointmentService = appointmentService;
        this.appointmentRepository = appointmentRepository;
        this.jwtTokenProvider = jwtTokenProvider;
        this.userService = userService;
    }

    /**
     * Crea un nuevo turno (reserva).
     * 
     * POST /api/appointments
     * 
     * Endpoint protegido (requiere autenticación).
     * 
     * Soporta idempotencia mediante header `Idempotency-Key`.
     * 
     * @param request DTO con la información del turno a crear
     * @param httpRequest HttpServletRequest para obtener token, IP del cliente e Idempotency-Key
     * @return Turno creado (201 Created) o existente si Idempotency-Key duplicado (200 OK)
     */
    @PostMapping
    public ResponseEntity<AppointmentResponse> createAppointment(
            @Valid @RequestBody CreateAppointmentRequest request,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);
        String idempotencyKey = httpRequest.getHeader("Idempotency-Key");
        String clientIp = getClientIp(httpRequest);

        logger.info("Solicitud de creación de turno - Usuario: {}, Fecha: {}, Hora: {}, Idempotency-Key: {}",
            userId, request.getDate(), request.getStartTime(), idempotencyKey);

        // Verificar si ya existe un turno con esta idempotency key (para determinar código HTTP)
        boolean wasAlreadyExisting = idempotencyKey != null && !idempotencyKey.isBlank() &&
            appointmentRepository.findByIdempotencyKey(idempotencyKey).isPresent();

        AppointmentResponse response = appointmentService.createAppointment(
            request, userId, idempotencyKey, clientIp);

        // Si el turno ya existía (por Idempotency-Key), retornar 200 OK, sino 201 Created
        if (wasAlreadyExisting) {
            logger.info("Turno existente retornado por Idempotency-Key: {}", idempotencyKey);
            return ResponseEntity.ok(response);
        }

        logger.info("Turno creado exitosamente - ID: {}, Usuario: {}", response.getId(), userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Confirma un turno (appointment), pasándolo de estado CREATED a CONFIRMED.
     * 
     * POST /api/appointments/{id}/confirm
     * 
     * Endpoint protegido (requiere autenticación).
     * 
     * @param appointmentId ID del turno a confirmar
     * @param httpRequest HttpServletRequest para obtener token e IP del cliente
     * @return Turno confirmado (200 OK)
     */
    @PostMapping("/{id}/confirm")
    public ResponseEntity<AppointmentResponse> confirmAppointment(
            @PathVariable("id") Long appointmentId,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        logger.info("Solicitud de confirmación de turno - ID: {}, Usuario: {}", appointmentId, userId);

        AppointmentResponse response = appointmentService.confirmAppointment(
            appointmentId, userId, clientIp);

        logger.info("Turno confirmado exitosamente - ID: {}, Usuario: {}", appointmentId, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Cancela un turno (appointment), pasándolo de estado CREATED o CONFIRMED a CANCELLED.
     * 
     * POST /api/appointments/{id}/cancel
     * 
     * Endpoint protegido (requiere autenticación).
     * 
     * @param appointmentId ID del turno a cancelar
     * @param request DTO con el motivo de cancelación (opcional)
     * @param httpRequest HttpServletRequest para obtener token e IP del cliente
     * @return Turno cancelado (200 OK)
     */
    @PostMapping("/{id}/cancel")
    public ResponseEntity<AppointmentResponse> cancelAppointment(
            @PathVariable("id") Long appointmentId,
            @RequestBody(required = false) CancelAppointmentRequest request,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        // Si no se envía request body, crear uno vacío
        if (request == null) {
            request = new CancelAppointmentRequest();
        }

        logger.info("Solicitud de cancelación de turno - ID: {}, Usuario: {}, Motivo: {}",
            appointmentId, userId, request.getReason() != null ? request.getReason() : "N/A");

        AppointmentResponse response = appointmentService.cancelAppointment(
            appointmentId, request, userId, clientIp);

        logger.info("Turno cancelado exitosamente - ID: {}, Usuario: {}", appointmentId, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Crea una solicitud de reprogramación de turno.
     * 
     * POST /api/appointments/{id}/request-reschedule
     * 
     * Endpoint protegido (requiere autenticación).
     * 
     * @param appointmentId ID del turno a reprogramar
     * @param request DTO con la nueva fecha/hora solicitada y motivo (opcional)
     * @param httpRequest HttpServletRequest para obtener token e IP del cliente
     * @return Solicitud de reprogramación creada (201 Created)
     */
    @PostMapping("/{id}/request-reschedule")
    public ResponseEntity<RescheduleRequestResponse> requestReschedule(
            @PathVariable("id") Long appointmentId,
            @Valid @RequestBody CreateRescheduleRequestRequest request,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        logger.info("Solicitud de reprogramación - Turno ID: {}, Usuario: {}, Nueva fecha: {}, Nueva hora: {}",
            appointmentId, userId, request.getNewDate(), request.getNewStartTime());

        RescheduleRequestResponse response = appointmentService.requestReschedule(
            appointmentId, request, userId, clientIp);

        logger.info("Solicitud de reprogramación creada exitosamente - ID: {}, Turno ID: {}, Usuario: {}",
            response.getId(), appointmentId, userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Consulta las solicitudes de reprogramación del usuario autenticado.
     * 
     * GET /api/appointments/my-reschedule-requests
     * GET /api/appointments/my-reschedule-requests?status=PENDING_ADMIN_APPROVAL
     * 
     * Endpoint protegido (requiere autenticación).
     * Implementa US-T014.1.
     * 
     * @param status Filtro opcional por estado (PENDING_ADMIN_APPROVAL, APPROVED, REJECTED, EXPIRED, CANCELLED)
     * @param httpRequest HttpServletRequest para obtener userId
     * @return Lista de solicitudes con resumen por estado (200 OK)
     */
    @GetMapping("/my-reschedule-requests")
    public ResponseEntity<MyRescheduleRequestsResponse> getMyRescheduleRequests(
            @RequestParam(required = false) String status,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);

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

        logger.info("Consultando solicitudes de reprogramación - Usuario: {}, Estado: {}", 
            userId, state != null ? state : "TODOS");

        MyRescheduleRequestsResponse response = appointmentService.getMyRescheduleRequests(userId, state);

        logger.info("Solicitudes encontradas - Total: {}, Pendientes: {}, Aprobadas: {}, Rechazadas: {}, Expiradas: {}, Canceladas: {}",
            response.getTotal(), response.getPending(), response.getApproved(), 
            response.getRejected(), response.getExpired(), response.getCancelled());

        return ResponseEntity.ok(response);
    }

    /**
     * Cancela una solicitud de reprogramación del usuario autenticado.
     * 
     * DELETE /api/appointments/reschedule-requests/{id}
     * 
     * Endpoint protegido (requiere autenticación).
     * Implementa US-T014.2.
     * 
     * @param rescheduleRequestId ID de la solicitud de reprogramación a cancelar
     * @param httpRequest HttpServletRequest para obtener userId e IP del cliente
     * @return Solicitud de reprogramación cancelada (200 OK)
     */
    @DeleteMapping("/reschedule-requests/{id}")
    @PreAuthorize("hasRole('USER') or hasRole('ADMIN')")
    public ResponseEntity<RescheduleRequestResponse> cancelMyRescheduleRequest(
            @PathVariable("id") Long rescheduleRequestId,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);
        String clientIp = getClientIp(httpRequest);

        logger.info("Solicitud de cancelación de reprogramación - ID: {}, Usuario: {}", 
            rescheduleRequestId, userId);

        RescheduleRequestResponse response = appointmentService.cancelMyRescheduleRequest(
            rescheduleRequestId, userId, clientIp);

        logger.info("Solicitud de reprogramación cancelada exitosamente - ID: {}, Usuario: {}", 
            rescheduleRequestId, userId);

        return ResponseEntity.ok(response);
    }

    /**
     * Obtiene el ID del usuario actual desde el token JWT o Authentication.
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

        // Fallback: obtener desde UserDetails
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof UserDetails) {
            String email = ((UserDetails) authentication.getPrincipal()).getUsername();
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
     * Consulta los turnos del usuario autenticado con filtros y paginación.
     * 
     * GET /api/appointments/my-appointments
     * GET /api/appointments/my-appointments?status=CONFIRMED&page=0&size=20
     * GET /api/appointments/my-appointments?fromDate=2026-01-01&toDate=2026-01-31&page=0&size=20
     * GET /api/appointments/my-appointments?upcoming=true&page=0&size=20
     * GET /api/appointments/my-appointments?past=true&page=0&size=20
     * 
     * Endpoint protegido (requiere autenticación).
     * Implementa US-T011.1.
     * 
     * @param status Filtro opcional por estado (CREATED, CONFIRMED, CANCELLED, etc.)
     * @param fromDate Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param toDate Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param upcoming Filtro opcional para solo turnos futuros (boolean, mutuamente excluyente con past)
     * @param past Filtro opcional para solo turnos pasados (boolean, mutuamente excluyente con upcoming)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20, máximo: 100)
     * @param httpRequest HttpServletRequest para obtener userId del token JWT
     * @return Lista paginada de turnos del usuario (200 OK)
     */
    @GetMapping("/my-appointments")
    @PreAuthorize("hasRole('USER') or hasRole('ADMIN')")
    public ResponseEntity<MyAppointmentsResponse> getMyAppointments(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String fromDate,
            @RequestParam(required = false) String toDate,
            @RequestParam(required = false) Boolean upcoming,
            @RequestParam(required = false) Boolean past,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);

        logger.info("Consultando turnos del usuario - ID: {}, Estado: {}, Desde: {}, Hasta: {}, Upcoming: {}, Past: {}, Página: {}, Tamaño: {}",
            userId, status, fromDate, toDate, upcoming, past, page, size);

        // Parsear estado si se proporcionó
        AppointmentState appointmentState = null;
        if (status != null && !status.isBlank()) {
            try {
                appointmentState = AppointmentState.valueOf(status.toUpperCase());
            } catch (IllegalArgumentException e) {
                logger.warn("Estado inválido proporcionado: {}", status);
                throw new com.ak4n1.turn_management.shared.exception.ApiException(
                    String.format("Estado inválido: %s. Estados válidos: CREATED, CONFIRMED, CANCELLED, EXPIRED, NO_SHOW, COMPLETED, RESCHEDULED", status),
                    org.springframework.http.HttpStatus.BAD_REQUEST);
            }
        }

        // Parsear fechas si se proporcionaron
        java.time.LocalDate fromDateParsed = null;
        if (fromDate != null && !fromDate.isBlank()) {
            try {
                fromDateParsed = java.time.LocalDate.parse(fromDate);
            } catch (java.time.format.DateTimeParseException e) {
                throw new com.ak4n1.turn_management.shared.exception.ApiException(
                    String.format("Fecha inválida en 'fromDate': %s. Formato esperado: yyyy-MM-dd", fromDate),
                    org.springframework.http.HttpStatus.BAD_REQUEST);
            }
        }

        java.time.LocalDate toDateParsed = null;
        if (toDate != null && !toDate.isBlank()) {
            try {
                toDateParsed = java.time.LocalDate.parse(toDate);
            } catch (java.time.format.DateTimeParseException e) {
                throw new com.ak4n1.turn_management.shared.exception.ApiException(
                    String.format("Fecha inválida en 'toDate': %s. Formato esperado: yyyy-MM-dd", toDate),
                    org.springframework.http.HttpStatus.BAD_REQUEST);
            }
        }

        // Llamar al servicio
        MyAppointmentsResponse response = appointmentService.getMyAppointments(
            userId, appointmentState, fromDateParsed, toDateParsed, upcoming, past, page, size);

        logger.info("Turnos encontrados - Total: {}, Página: {}, Tamaño: {}, Total páginas: {}",
            response.getTotal(), response.getPage(), response.getSize(), response.getTotalPages());

        return ResponseEntity.ok(response);
    }

    /**
     * Consulta el historial completo de un turno.
     * 
     * GET /api/appointments/{id}/history
     * 
     * Endpoint protegido (requiere autenticación).
     * Implementa US-T011.3.
     * 
     * @param appointmentId ID del turno
     * @param httpRequest HttpServletRequest para obtener userId del token JWT
     * @return Historial completo del turno (200 OK)
     */
    @GetMapping("/{id}/history")
    @PreAuthorize("hasRole('USER') or hasRole('ADMIN')")
    public ResponseEntity<AppointmentHistoryResponse> getAppointmentHistory(
            @PathVariable("id") Long appointmentId,
            HttpServletRequest httpRequest) {
        
        Long userId = getCurrentUserId(httpRequest);

        logger.info("Consultando historial de turno - ID: {}, Usuario: {}", appointmentId, userId);

        AppointmentHistoryResponse response = appointmentService.getAppointmentHistory(
            appointmentId, userId);

        logger.info("Historial encontrado - Turno ID: {}, Eventos: {}", 
            appointmentId, response.getHistory().size());

        return ResponseEntity.ok(response);
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

