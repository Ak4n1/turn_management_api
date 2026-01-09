package com.ak4n1.turn_management.shared.audit.service;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentHistoryRepository;
import com.ak4n1.turn_management.feature.appointment.repository.RescheduleRequestRepository;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequest;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarException;
import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarExceptionRepository;
import com.ak4n1.turn_management.feature.configuration.repository.ManualBlockRepository;
import com.ak4n1.turn_management.shared.audit.dto.AuditLogResponse;
import com.ak4n1.turn_management.shared.audit.dto.AuditLogsResponse;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Implementación del servicio de auditoría.
 * 
 * Consolida información de múltiples fuentes para proporcionar un historial completo.
 * Implementa US-T018.
 */
@Service
public class AuditServiceImpl implements AuditService {

    private static final Logger logger = LoggerFactory.getLogger(AuditServiceImpl.class);

    private final AppointmentHistoryRepository appointmentHistoryRepository;
    private final CalendarConfigurationRepository configurationRepository;
    private final CalendarExceptionRepository exceptionRepository;
    private final ManualBlockRepository blockRepository;
    private final RescheduleRequestRepository rescheduleRequestRepository;
    private final UserService userService;

    public AuditServiceImpl(
            AppointmentHistoryRepository appointmentHistoryRepository,
            CalendarConfigurationRepository configurationRepository,
            CalendarExceptionRepository exceptionRepository,
            ManualBlockRepository blockRepository,
            RescheduleRequestRepository rescheduleRequestRepository,
            UserService userService) {
        this.appointmentHistoryRepository = appointmentHistoryRepository;
        this.configurationRepository = configurationRepository;
        this.exceptionRepository = exceptionRepository;
        this.blockRepository = blockRepository;
        this.rescheduleRequestRepository = rescheduleRequestRepository;
        this.userService = userService;
    }

    /**
     * Obtiene logs de auditoría con filtros opcionales.
     * 
     * Consolida información de:
     * - AppointmentHistory (cambios de turnos)
     * - CalendarConfiguration (cambios de configuraciones)
     * - CalendarException (excepciones)
     * - ManualBlock (bloqueos)
     * - RescheduleRequest (reprogramaciones)
     */
    @Override
    @Transactional(readOnly = true)
    public AuditLogsResponse getAuditLogs(
            String actionType,
            Long userId,
            LocalDate startDate,
            LocalDate endDate,
            int page,
            int size) {
        
        logger.info("Consultando logs de auditoría - Tipo: {}, Usuario: {}, Desde: {}, Hasta: {}, Página: {}, Tamaño: {}",
            actionType, userId, startDate, endDate, page, size);

        // 1. Validar tamaño de página
        if (size < 1 || size > 100) {
            throw new ApiException(
                "El tamaño de página debe estar entre 1 y 100. Valor recibido: " + size,
                HttpStatus.BAD_REQUEST);
        }

        // 2. Validar número de página
        if (page < 0) {
            throw new ApiException(
                "El número de página debe ser mayor o igual a 0. Valor recibido: " + page,
                HttpStatus.BAD_REQUEST);
        }

        // 3. Validar rango de fechas
        if (startDate != null && endDate != null && startDate.isAfter(endDate)) {
            throw new ApiException(
                "La fecha 'startDate' no puede ser posterior a 'endDate'",
                HttpStatus.BAD_REQUEST);
        }

        // 4. Convertir fechas a LocalDateTime para comparaciones
        LocalDateTime startDateTime = startDate != null ? startDate.atStartOfDay() : null;
        LocalDateTime endDateTime = endDate != null ? endDate.atTime(23, 59, 59) : null;

        // 5. Obtener logs de todas las fuentes
        List<AuditLogResponse> allLogs = new ArrayList<>();

        // 5.1. Logs de AppointmentHistory (cambios de turnos)
        if (actionType == null || "CANCELLATION".equals(actionType) || "CONFIRMATION".equals(actionType) || 
            "CREATION".equals(actionType) || "RESCHEDULE".equals(actionType) || "NO_SHOW".equals(actionType)) {
            List<AppointmentHistory> appointmentHistories = appointmentHistoryRepository.findAll();
            allLogs.addAll(appointmentHistories.stream()
                .filter(h -> filterByActionType(h.getAction(), actionType))
                .filter(h -> filterByUserId(h.getUserId(), userId))
                .filter(h -> filterByDateRange(h.getCreatedAt(), startDateTime, endDateTime))
                .map(this::mapAppointmentHistoryToAuditLog)
                .toList());
        }

        // 5.2. Logs de CalendarConfiguration (cambios de configuraciones)
        if (actionType == null || "CONFIGURATION_CHANGE".equals(actionType)) {
            List<CalendarConfiguration> configurations = configurationRepository.findAllByOrderByVersionDesc();
            allLogs.addAll(configurations.stream()
                .filter(c -> filterByUserId(c.getCreatedByUserId(), userId))
                .filter(c -> filterByDateRange(c.getCreatedAt(), startDateTime, endDateTime))
                .map(this::mapConfigurationToAuditLog)
                .toList());
        }

        // 5.3. Logs de CalendarException (excepciones)
        if (actionType == null || "EXCEPTION".equals(actionType)) {
            List<CalendarException> exceptions = exceptionRepository.findAll();
            allLogs.addAll(exceptions.stream()
                .filter(e -> filterByUserId(e.getCreatedByUserId(), userId))
                .filter(e -> filterByDateRange(e.getCreatedAt(), startDateTime, endDateTime))
                .map(this::mapExceptionToAuditLog)
                .toList());
        }

        // 5.4. Logs de ManualBlock (bloqueos)
        if (actionType == null || "BLOCK".equals(actionType)) {
            List<ManualBlock> blocks = blockRepository.findAll();
            allLogs.addAll(blocks.stream()
                .filter(b -> filterByUserId(b.getCreatedByUserId(), userId))
                .filter(b -> filterByDateRange(b.getCreatedAt(), startDateTime, endDateTime))
                .map(this::mapBlockToAuditLog)
                .toList());
        }

        // 5.5. Logs de RescheduleRequest (reprogramaciones)
        if (actionType == null || "RESCHEDULE_REQUEST".equals(actionType)) {
            List<RescheduleRequest> rescheduleRequests = rescheduleRequestRepository.findAll();
            allLogs.addAll(rescheduleRequests.stream()
                .filter(r -> filterByUserId(r.getUserId(), userId))
                .filter(r -> filterByDateRange(r.getCreatedAt(), startDateTime, endDateTime))
                .map(this::mapRescheduleRequestToAuditLog)
                .toList());
        }

        // 6. Ordenar por fecha descendente (más recientes primero)
        allLogs.sort(Comparator.comparing(AuditLogResponse::getTimestamp).reversed());

        logger.info("Logs de auditoría encontrados - Total: {}", allLogs.size());

        // 7. Paginar
        int start = page * size;
        int end = Math.min(start + size, allLogs.size());
        List<AuditLogResponse> pagedLogs = start < allLogs.size() ? allLogs.subList(start, end) : new ArrayList<>();
        
        int totalPages = (int) Math.ceil((double) allLogs.size() / size);

        logger.info("Logs paginados - Página: {}, Tamaño: {}, Total páginas: {}", page, size, totalPages);

        return new AuditLogsResponse(pagedLogs, allLogs.size(), totalPages, page, size);
    }

    /**
     * Filtra por tipo de acción.
     */
    private boolean filterByActionType(String action, String actionType) {
        if (actionType == null || actionType.isBlank()) {
            return true;
        }
        
        String actionUpper = action != null ? action.toUpperCase() : "";
        String actionTypeUpper = actionType.toUpperCase();
        
        // Mapear tipos de acción a acciones específicas
        switch (actionTypeUpper) {
            case "CANCELLATION":
                return actionUpper.contains("CANCELL");
            case "CONFIRMATION":
                return actionUpper.contains("CONFIRM");
            case "CREATION":
                return actionUpper.contains("CREATED") || actionUpper.contains("CREATE");
            case "RESCHEDULE":
                return actionUpper.contains("RESCHEDULE");
            case "NO_SHOW":
                return actionUpper.contains("NO_SHOW");
            default:
                return actionUpper.contains(actionTypeUpper);
        }
    }

    /**
     * Filtra por ID de usuario.
     */
    private boolean filterByUserId(Long logUserId, Long filterUserId) {
        if (filterUserId == null) {
            return true;
        }
        return logUserId != null && logUserId.equals(filterUserId);
    }

    /**
     * Filtra por rango de fechas.
     */
    private boolean filterByDateRange(LocalDateTime timestamp, LocalDateTime start, LocalDateTime end) {
        if (timestamp == null) {
            return false;
        }
        if (start != null && timestamp.isBefore(start)) {
            return false;
        }
        if (end != null && timestamp.isAfter(end)) {
            return false;
        }
        return true;
    }

    /**
     * Mapea AppointmentHistory a AuditLogResponse.
     */
    private AuditLogResponse mapAppointmentHistoryToAuditLog(AppointmentHistory history) {
        AuditLogResponse log = new AuditLogResponse();
        log.setId(history.getId());
        log.setAction(history.getAction());
        log.setActionType(mapActionToActionType(history.getAction()));
        log.setUserId(history.getUserId());
        log.setAppointmentId(history.getAppointmentId());
        log.setPreviousState(history.getPreviousState() != null ? history.getPreviousState().toString() : null);
        log.setNewState(history.getNewState() != null ? history.getNewState().toString() : null);
        log.setReason(history.getReason());
        log.setTimestamp(history.getCreatedAt().toString());
        log.setIpAddress(history.getIpAddress());

        // Obtener email del usuario
        Optional<User> user = userService.findById(history.getUserId());
        if (user.isPresent()) {
            log.setUserEmail(user.get().getEmail());
        }

        return log;
    }

    /**
     * Mapea CalendarConfiguration a AuditLogResponse.
     */
    private AuditLogResponse mapConfigurationToAuditLog(CalendarConfiguration config) {
        AuditLogResponse log = new AuditLogResponse();
        log.setId(config.getId());
        log.setAction("CONFIGURATION_CREATED");
        log.setActionType("CONFIGURATION_CHANGE");
        log.setUserId(config.getCreatedByUserId());
        log.setConfigurationId(config.getId());
        log.setReason(config.getNotes());
        log.setTimestamp(config.getCreatedAt().toString());
        log.setNewState("VERSION_" + config.getVersion());

        // Obtener email del usuario
        Optional<User> user = userService.findById(config.getCreatedByUserId());
        if (user.isPresent()) {
            log.setUserEmail(user.get().getEmail());
        }

        return log;
    }

    /**
     * Mapea CalendarException a AuditLogResponse.
     */
    private AuditLogResponse mapExceptionToAuditLog(CalendarException exception) {
        AuditLogResponse log = new AuditLogResponse();
        log.setId(exception.getId());
        log.setAction("EXCEPTION_CREATED");
        log.setActionType("EXCEPTION");
        log.setUserId(exception.getCreatedByUserId());
        log.setExceptionId(exception.getId());
        log.setReason(exception.getReason());
        log.setTimestamp(exception.getCreatedAt().toString());
        log.setNewState(exception.getIsOpen() ? "OPEN" : "CLOSED");

        // Obtener email del usuario
        Optional<User> user = userService.findById(exception.getCreatedByUserId());
        if (user.isPresent()) {
            log.setUserEmail(user.get().getEmail());
        }

        return log;
    }

    /**
     * Mapea ManualBlock a AuditLogResponse.
     */
    private AuditLogResponse mapBlockToAuditLog(ManualBlock block) {
        AuditLogResponse log = new AuditLogResponse();
        log.setId(block.getId());
        log.setAction("BLOCK_CREATED");
        log.setActionType("BLOCK");
        log.setUserId(block.getCreatedByUserId());
        log.setBlockId(block.getId());
        log.setReason(block.getReason());
        log.setTimestamp(block.getCreatedAt().toString());
        log.setNewState(block.getIsFullDay() ? "FULL_DAY_BLOCK" : "PARTIAL_BLOCK");

        // Obtener email del usuario
        Optional<User> user = userService.findById(block.getCreatedByUserId());
        if (user.isPresent()) {
            log.setUserEmail(user.get().getEmail());
        }

        return log;
    }

    /**
     * Mapea RescheduleRequest a AuditLogResponse.
     */
    private AuditLogResponse mapRescheduleRequestToAuditLog(RescheduleRequest request) {
        AuditLogResponse log = new AuditLogResponse();
        log.setId(request.getId());
        log.setAction("RESCHEDULE_REQUEST_" + request.getState());
        log.setActionType("RESCHEDULE_REQUEST");
        log.setUserId(request.getUserId());
        log.setAppointmentId(request.getAppointmentId());
        log.setRescheduleRequestId(request.getId());
        log.setReason(request.getReason());
        log.setTimestamp(request.getCreatedAt().toString());
        log.setNewState(request.getState().toString());

        // Obtener email del usuario
        Optional<User> user = userService.findById(request.getUserId());
        if (user.isPresent()) {
            log.setUserEmail(user.get().getEmail());
        }

        return log;
    }

    /**
     * Mapea acción específica a tipo de acción general.
     */
    private String mapActionToActionType(String action) {
        if (action == null) {
            return "UNKNOWN";
        }
        String actionUpper = action.toUpperCase();
        if (actionUpper.contains("CANCELL")) {
            return "CANCELLATION";
        } else if (actionUpper.contains("CONFIRM")) {
            return "CONFIRMATION";
        } else if (actionUpper.contains("CREATED") || actionUpper.contains("CREATE")) {
            return "CREATION";
        } else if (actionUpper.contains("RESCHEDULE")) {
            return "RESCHEDULE";
        } else if (actionUpper.contains("NO_SHOW")) {
            return "NO_SHOW";
        } else if (actionUpper.contains("EXPIRED")) {
            return "EXPIRATION";
        } else {
            return "OTHER";
        }
    }
}

