package com.ak4n1.turn_management.feature.appointment.service;

import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.domain.RescheduleRequestState;
import com.ak4n1.turn_management.feature.appointment.dto.request.AdminRescheduleAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CancelAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateOverrideAppointmentRequest;
import com.ak4n1.turn_management.feature.appointment.dto.request.CreateRescheduleRequestRequest;
import com.ak4n1.turn_management.feature.appointment.dto.response.AdminRescheduleRequestsResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentResponse;
import com.ak4n1.turn_management.feature.appointment.dto.response.RescheduleRequestResponse;
import org.springframework.data.domain.Pageable;

/**
 * Servicio para gestión de turnos (appointments).
 */
public interface AppointmentService {

    /**
     * Crea un nuevo turno (appointment).
     * 
     * Implementa:
     * - Validación de disponibilidad real (re-validación atómica)
     * - Idempotencia mediante Idempotency-Key header
     * - Control de concurrencia (lock pesimista)
     * - Validación de anticipación mínima
     * - Validación de límites de turnos por usuario
     * - Cálculo de TTL (expiresAt)
     * - Auditoría completa
     * 
     * @param request DTO con la información del turno a crear
     * @param userId ID del usuario que crea el turno
     * @param idempotencyKey Clave de idempotencia (opcional, desde header)
     * @param clientIp IP del cliente (para auditoría)
     * @return Turno creado
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse createAppointment(CreateAppointmentRequest request, Long userId, 
                                          String idempotencyKey, String clientIp);

    /**
     * Confirma un turno (appointment), pasándolo de estado CREATED a CONFIRMED.
     * 
     * Implementa:
     * - Validación de que el turno esté en estado CREATED
     * - Validación de que el turno no haya expirado
     * - Re-validación de disponibilidad del slot
     * - Validación de que el usuario sea el dueño del turno
     * - Actualización de estado a CONFIRMED
     * - Remoción de TTL (expiresAt)
     * - Guardado de timestamp de confirmación (confirmedAt)
     * - Auditoría completa
     * 
     * @param appointmentId ID del turno a confirmar
     * @param userId ID del usuario que confirma el turno
     * @param clientIp IP del cliente (para auditoría)
     * @return Turno confirmado
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse confirmAppointment(Long appointmentId, Long userId, String clientIp);

    /**
     * Cancela un turno (appointment), pasándolo de estado CREATED o CONFIRMED a CANCELLED.
     * 
     * Implementa:
     * - Validación de que el turno esté en estado CREATED o CONFIRMED
     * - Validación de que el turno sea futuro
     * - Validación de ventana mínima de cancelación configurable
     * - Validación de que el usuario sea el dueño del turno (o admin)
     * - Actualización de estado a CANCELLED
     * - Liberación del slot (remover TTL si existe)
     * - Guardado de motivo de cancelación (opcional)
     * - Auditoría completa
     * 
     * @param appointmentId ID del turno a cancelar
     * @param request DTO con el motivo de cancelación (opcional)
     * @param userId ID del usuario que cancela el turno
     * @param clientIp IP del cliente (para auditoría)
     * @return Turno cancelado
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse cancelAppointment(Long appointmentId, CancelAppointmentRequest request, 
                                         Long userId, String clientIp);

    /**
     * Crea una solicitud de reprogramación de turno.
     * 
     * Implementa:
     * - Validación de que el turno esté en estado CONFIRMED
     * - Validación de que el usuario sea el dueño del turno
     * - Validación de que no exista otra solicitud pendiente para el mismo turno
     * - Validación de que el nuevo slot esté disponible
     * - Validación de que el nuevo slot cumpla todas las reglas
     * - Creación de solicitud en estado PENDING_ADMIN_APPROVAL
     * - Auditoría completa
     * - **Pendiente:** Notificación WebSocket al admin (FASE 9 - US-N001)
     * 
     * @param appointmentId ID del turno a reprogramar
     * @param request DTO con la nueva fecha/hora solicitada y motivo (opcional)
     * @param userId ID del usuario que solicita la reprogramación
     * @param clientIp IP del cliente (para auditoría)
     * @return Solicitud de reprogramación creada
     * @throws IllegalArgumentException si la validación falla
     */
    RescheduleRequestResponse requestReschedule(Long appointmentId, CreateRescheduleRequestRequest request,
                                               Long userId, String clientIp);

    /**
     * Obtiene todas las solicitudes de reprogramación del usuario autenticado.
     * 
     * Implementa US-T014.1:
     * - Filtrado por usuario (solo solicitudes propias)
     * - Filtrado opcional por estado
     * - Ordenamiento por fecha de creación (más recientes primero)
     * - Resumen con totales por estado
     * 
     * @param userId ID del usuario autenticado
     * @param state Filtro opcional por estado (null para todas)
     * @return Lista de solicitudes con resumen por estado
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.MyRescheduleRequestsResponse getMyRescheduleRequests(
            Long userId, RescheduleRequestState state);

    /**
     * Cancela una solicitud de reprogramación del usuario autenticado.
     * 
     * Implementa US-T014.2:
     * - Validación de que la solicitud esté en estado PENDING_ADMIN_APPROVAL
     * - Validación de que el usuario sea el dueño de la solicitud
     * - Actualización de estado a CANCELLED
     * - El turno original permanece sin cambios
     * - Auditoría completa
     * - **Pendiente:** Notificación WebSocket al admin (FASE 9 - US-N002)
     * 
     * @param rescheduleRequestId ID de la solicitud de reprogramación a cancelar
     * @param userId ID del usuario que cancela la solicitud
     * @param clientIp IP del cliente (para auditoría)
     * @return Solicitud de reprogramación cancelada
     * @throws IllegalArgumentException si la validación falla
     */
    RescheduleRequestResponse cancelMyRescheduleRequest(Long rescheduleRequestId, Long userId, String clientIp);

    /**
     * Obtiene todas las solicitudes de reprogramación con filtros (solo admin).
     * 
     * Implementa US-T014.3:
     * - Solo accesible por admin
     * - Filtrado por estado, usuario, rango de fechas
     * - Paginación obligatoria
     * - Ordenamiento por fecha de creación (más recientes primero)
     * - Incluye información del usuario (email, nombre)
     * 
     * @param state Filtro opcional por estado
     * @param userId Filtro opcional por ID de usuario
     * @param fromDate Filtro opcional por fecha desde (en createdAt)
     * @param toDate Filtro opcional por fecha hasta (en createdAt)
     * @param pageable Paginación (page, size)
     * @return Lista paginada de solicitudes con información de usuario
     */
    AdminRescheduleRequestsResponse getAllRescheduleRequests(
            RescheduleRequestState state,
            Long userId,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            Pageable pageable);

    /**
     * Aprueba una solicitud de reprogramación (solo admin).
     * 
     * Implementa US-T014:
     * - Solo accesible por admin
     * - Validación de que la solicitud esté en estado PENDING_ADMIN_APPROVAL
     * - Validación de que el turno original siga existiendo y esté en CONFIRMED
     * - Validación de que el slot solicitado siga disponible
     * - Turno original pasa a estado RESCHEDULED
     * - Nuevo turno creado en estado CONFIRMED
     * - Solicitud pasa a estado APPROVED
     * - Operación transaccional atómica
     * - Email y WebSocket al usuario
     * - Actualización en tiempo real de disponibilidad (ambas fechas)
     * 
     * @param rescheduleRequestId ID de la solicitud de reprogramación a aprobar
     * @param adminUserId ID del administrador que aprueba
     * @param clientIp IP del cliente (para auditoría)
     * @return Solicitud aprobada con información del nuevo turno
     * @throws IllegalArgumentException si la validación falla
     */
    RescheduleRequestResponse approveRescheduleRequest(Long rescheduleRequestId, Long adminUserId, String clientIp);

    /**
     * Rechaza una solicitud de reprogramación (solo admin).
     * 
     * Implementa US-T014:
     * - Solo accesible por admin
     * - Validación de que la solicitud esté en estado PENDING_ADMIN_APPROVAL
     * - Solicitud pasa a estado REJECTED
     * - Motivo de rechazo guardado
     * - Turno original permanece sin cambios
     * - Email y WebSocket al usuario
     * 
     * @param rescheduleRequestId ID de la solicitud de reprogramación a rechazar
     * @param request DTO con motivo de rechazo (opcional)
     * @param adminUserId ID del administrador que rechaza
     * @param clientIp IP del cliente (para auditoría)
     * @return Solicitud rechazada
     * @throws IllegalArgumentException si la validación falla
     */
    RescheduleRequestResponse rejectRescheduleRequest(Long rescheduleRequestId, 
                                                      com.ak4n1.turn_management.feature.appointment.dto.request.RejectRescheduleRequestRequest request,
                                                      Long adminUserId, String clientIp);

    /**
     * Marca un turno como no-show (ausente) - Solo admin.
     * 
     * Implementa US-T016:
     * - Solo accesible por admin
     * - Validación de que el turno esté en estado CONFIRMED
     * - Validación de que el horario del turno ya pasó
     * - Actualización de estado a NO_SHOW
     * - Registro en auditoría
     * - **Pendiente:** Notificación WebSocket al usuario (FASE 9 - US-N001)
     * 
     * @param appointmentId ID del turno a marcar como no-show
     * @param adminUserId ID del administrador que marca el no-show
     * @param clientIp IP del cliente (para auditoría)
     * @return Turno marcado como no-show
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse markAsNoShow(Long appointmentId, Long adminUserId, String clientIp);

    /**
     * Obtiene los turnos del usuario autenticado con filtros y paginación.
     * 
     * Implementa US-T011.1:
     * - Solo muestra turnos del usuario autenticado
     * - Filtros opcionales: estado, rango de fechas, upcoming, past
     * - Paginación obligatoria
     * - Ordenamiento por fecha y hora ascendente
     * - upcoming y past son mutuamente excluyentes
     * 
     * @param userId ID del usuario autenticado
     * @param status Filtro opcional por estado
     * @param fromDate Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param toDate Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param daysOfWeek Filtro opcional por días de la semana (1=Lunes, 7=Domingo)
     * @param upcoming Filtro opcional para solo turnos futuros (mutuamente excluyente con past)
     * @param past Filtro opcional para solo turnos pasados (mutuamente excluyente con upcoming)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20)
     * @return Lista paginada de turnos del usuario
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.MyAppointmentsResponse getMyAppointments(
            Long userId,
            AppointmentState status,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            java.util.List<Integer> daysOfWeek,
            Boolean upcoming,
            Boolean past,
            String sortOrder,
            int page,
            int size);

    /**
     * Obtiene el historial completo de un turno.
     * 
     * Implementa US-T011.3:
     * - Solo muestra historial de turnos del usuario autenticado
     * - Ordenado por timestamp descendente (más recientes primero)
     * - Incluye información del usuario que realizó cada acción
     * - Incluye motivos y detalles
     * 
     * @param appointmentId ID del turno
     * @param userId ID del usuario autenticado (para validar propiedad)
     * @return Historial completo del turno
     * @throws IllegalArgumentException si el turno no existe o no pertenece al usuario
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentHistoryResponse getAppointmentHistory(
            Long appointmentId, Long userId);

    /**
     * Obtiene todos los turnos con filtros para administrador.
     * 
     * Implementa US-T017:
     * - Solo accesible por admin
     * - Filtros opcionales: estado, usuario, rango de fechas, fecha específica
     * - Búsqueda por email o nombre de usuario
     * - Paginación obligatoria
     * - Ordenamiento por fecha descendente y hora ascendente
     * - Incluye información del usuario (email, nombre)
     * 
     * @param state Filtro opcional por estado
     * @param userId Filtro opcional por ID de usuario
     * @param search Filtro opcional de búsqueda por email o nombre (parcial)
     * @param fromDate Filtro opcional por fecha desde (formato: yyyy-MM-dd)
     * @param toDate Filtro opcional por fecha hasta (formato: yyyy-MM-dd)
     * @param date Filtro opcional por fecha específica (formato: yyyy-MM-dd)
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20, máximo: 100)
     * @return Lista paginada de turnos con información de usuario
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.AdminAppointmentsResponse getAllAppointments(
            AppointmentState state,
            Long userId,
            String search,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            java.time.LocalDate date,
            java.util.List<Integer> daysOfWeek,
            int page,
            int size);

    /**
     * NUEVO: Obtiene turnos activos que están en un día cerrado según la configuración actual.
     * 
     * @param date Fecha a consultar
     * @param page Número de página (default: 0)
     * @param size Tamaño de página (default: 20)
     * @return Lista paginada de turnos afectados con información completa
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.AdminAppointmentsResponse getAppointmentsAffectedByClosedDay(
        java.time.LocalDate date, int page, int size);

    /**
     * Obtiene el calendario de turnos agendados para un rango de fechas (solo admin).
     * 
     * Implementa US-T017.1:
     * - Solo accesible por admin
     * - Agrupa turnos por día
     * - Genera slots disponibles usando la configuración del calendario
     * - Marca slots ocupados con información del turno
     * - Incluye información del usuario en cada turno
     * - Muestra disponibilidad vs ocupación
     * 
     * @param startDate Fecha de inicio del rango (formato: yyyy-MM-dd)
     * @param endDate Fecha de fin del rango (formato: yyyy-MM-dd)
     * @return Calendario de turnos agrupados por día con slots
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentsCalendarResponse getAppointmentsCalendar(
            java.time.LocalDate startDate, java.time.LocalDate endDate);

    /**
     * Obtiene el historial completo de un turno para administrador.
     * 
     * Implementa US-T017.2:
     * - Solo accesible por admin
     * - No valida propiedad del turno (admin puede ver cualquier turno)
     * - Incluye todos los cambios de estado
     * - Incluye información de quién hizo cada cambio
     * - Incluye fechas y motivos
     * - Incluye relación con otros turnos (reprogramaciones)
     * - Incluye solicitudes de reprogramación asociadas
     * 
     * @param appointmentId ID del turno
     * @return Historial completo del turno
     * @throws IllegalArgumentException si el turno no existe
     */
    com.ak4n1.turn_management.feature.appointment.dto.response.AppointmentHistoryResponse getAppointmentHistoryForAdmin(
            Long appointmentId);

    /**
     * Exporta turnos en formato CSV o Excel para administrador.
     * 
     * Implementa US-T017.3:
     * - Solo accesible por admin
     * - Aplica los mismos filtros que getAllAppointments
     * - Formato CSV o XLSX
     * - Límite de 10,000 registros por request
     * - Para más registros, se debe usar exportación asíncrona (futuro)
     * 
     * @param format Formato de exportación (CSV o XLSX)
     * @param state Filtro opcional por estado
     * @param userId Filtro opcional por ID de usuario
     * @param search Filtro opcional de búsqueda por email o nombre (parcial)
     * @param fromDate Filtro opcional por fecha desde
     * @param toDate Filtro opcional por fecha hasta
     * @param date Filtro opcional por fecha específica
     * @return Bytes del archivo y nombre del archivo
     */
    org.springframework.http.ResponseEntity<byte[]> exportAppointments(
            String format,
            AppointmentState state,
            Long userId,
            String search,
            java.time.LocalDate fromDate,
            java.time.LocalDate toDate,
            java.time.LocalDate date);

    /**
     * Reprograma un turno directamente como administrador.
     * 
     * Implementa US-T023:
     * - Solo accesible por admin
     * - Puede reprogramar cualquier turno futuro (CONFIRMED, CREATED, RESCHEDULED)
     * - Motivo obligatorio (mínimo 10 caracteres)
     * - Validación de disponibilidad del nuevo slot
     * - Turno original pasa a estado RESCHEDULED
     * - Nuevo turno creado con previousAppointmentId apuntando al original
     * - Nuevo turno en estado CONFIRMED (sin necesidad de confirmación adicional)
     * - Operación transaccional atómica
     * - Registro en auditoría
     * - **Pendiente:** Notificación WebSocket al usuario (FASE 9 - US-N001)
     * 
     * @param appointmentId ID del turno a reprogramar
     * @param request DTO con nueva fecha/hora y motivo
     * @param adminUserId ID del administrador que realiza la reprogramación
     * @param clientIp IP del cliente (para auditoría)
     * @return Nuevo turno creado (en estado CONFIRMED)
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse rescheduleAppointmentByAdmin(Long appointmentId, 
                                                      AdminRescheduleAppointmentRequest request,
                                                      Long adminUserId, String clientIp);

    /**
     * Cancela un turno directamente como administrador.
     * 
     * Implementa US-T024:
     * - Solo accesible por admin
     * - Puede cancelar cualquier turno (sin validar ventana mínima)
     * - Motivo obligatorio (mínimo 10 caracteres)
     * - Turno pasa a estado CANCELLED_BY_ADMIN
     * - No se respetan ventanas de cancelación
     * - Operación transaccional atómica
     * - Registro en auditoría
     * - **Pendiente:** Notificación WebSocket al usuario (FASE 9 - US-N001)
     * 
     * @param appointmentId ID del turno a cancelar
     * @param request DTO con motivo
     * @param adminUserId ID del administrador que realiza la cancelación
     * @param clientIp IP del cliente (para auditoría)
     * @return Turno cancelado
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse cancelAppointmentByAdmin(Long appointmentId, 
                                                  CancelAppointmentRequest request,
                                                  Long adminUserId, String clientIp);

    /**
     * Crea un turno forzando las reglas normales (solo admin).
     * 
     * Implementa US-T025:
     * - Solo accesible por admin
     * - Permite crear turnos fuera de horario, en días cerrados, etc.
     * - Justificación obligatoria (mínimo 20 caracteres)
     * - Turno se marca como overridden = true
     * - Turno se crea directamente en estado CONFIRMED
     * - No se validan reglas normales (horarios, días, anticipación, límites, etc.)
     * - Operación transaccional atómica
     * - Registro en auditoría
     * - **Pendiente:** Notificación WebSocket al usuario (FASE 9 - US-N001)
     * 
     * @param request DTO con fecha/hora, usuario y justificación
     * @param adminUserId ID del administrador que crea el turno
     * @param clientIp IP del cliente (para auditoría)
     * @return Turno creado (en estado CONFIRMED, con overridden = true)
     * @throws IllegalArgumentException si la validación falla
     */
    AppointmentResponse createAppointmentOverride(CreateOverrideAppointmentRequest request,
                                                   Long adminUserId, String clientIp);
}

