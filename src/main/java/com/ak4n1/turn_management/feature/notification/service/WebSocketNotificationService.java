package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.feature.notification.domain.SystemNotification;
import com.ak4n1.turn_management.shared.websocket.AppointmentWebSocketHandler;
import com.ak4n1.turn_management.shared.websocket.dto.WebSocketMessage;
import com.ak4n1.turn_management.shared.websocket.util.WebSocketMessageType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Servicio para enviar notificaciones vía WebSocket.
 * 
 * Implementa US-N001: Integración de WebSocket con eventos de turnos.
 */
@Service
public class WebSocketNotificationService {

    private static final Logger logger = LoggerFactory.getLogger(WebSocketNotificationService.class);

    private final AppointmentWebSocketHandler webSocketHandler;
    private final SystemNotificationService notificationService;
    private final UserRepository userRepository;
    private final NotificationPreferenceService preferenceService;

    public WebSocketNotificationService(
            AppointmentWebSocketHandler webSocketHandler,
            SystemNotificationService notificationService,
            UserRepository userRepository,
            NotificationPreferenceService preferenceService) {
        this.webSocketHandler = webSocketHandler;
        this.notificationService = notificationService;
        this.userRepository = userRepository;
        this.preferenceService = preferenceService;
    }

    /**
     * Envía una notificación a un usuario específico vía WebSocket y la guarda en BD.
     * 
     * @param userId ID del usuario destinatario
     * @param type Tipo de notificación
     * @param title Título de la notificación
     * @param message Mensaje de la notificación
     * @param relatedEntityType Tipo de entidad relacionada
     * @param relatedEntityId ID de la entidad relacionada
     * @param appointmentId ID del turno (opcional)
     */
    @Async("emailTaskExecutor")
    public void sendNotificationToUser(Long userId, NotificationType type, String title, String message,
                                      RelatedEntityType relatedEntityType, Long relatedEntityId,
                                      Long appointmentId) {
        try {
            // RESCHEDULE_REQUEST_PENDING: notificación solo en plataforma para admins, siempre (no configurable por preferencias)
            boolean skipPreferenceCheck = (type == NotificationType.RESCHEDULE_REQUEST_PENDING);
            if (!skipPreferenceCheck) {
                boolean shouldSend = preferenceService.shouldReceiveNotification(userId, type);
                if (!shouldSend) {
                    logger.info("Notificación WebSocket omitida para usuario {} (tipo {}) debido a preferencias del usuario.",
                        userId, type);
                    return;
                }
            }

            // Obtener usuario para obtener su email
            Optional<User> userOpt = userRepository.findById(userId);
            if (userOpt.isEmpty()) {
                logger.warn("Usuario no encontrado para enviar notificación - ID: {}", userId);
                return;
            }

            User user = userOpt.get();
            String userEmail = user.getEmail();

            // RESCHEDULE_REQUEST_PENDING: la notificación ya se creó en BD en el flujo del appointment; solo enviamos WebSocket
            long unreadCount;
            Long notificationId = null;
            if (type == NotificationType.RESCHEDULE_REQUEST_PENDING) {
                unreadCount = notificationService.countUnreadNotifications(userId);
            } else {
                SystemNotification notification = notificationService.createNotification(
                    type, userId, title, message, relatedEntityType, relatedEntityId);
                notificationId = notification.getId();
                unreadCount = notificationService.countUnreadNotifications(userId);
            }

            // Enviar vía WebSocket
            WebSocketMessage wsMessage = new WebSocketMessage();
            wsMessage.setType(mapNotificationTypeToWebSocketType(type));
            wsMessage.setTitle(title);
            wsMessage.setMessage(message);
            if (appointmentId != null) {
                wsMessage.setAppointmentId(appointmentId);
            }
            Map<String, Object> data = new HashMap<>();
            if (notificationId != null) {
                data.put("notificationId", notificationId);
            }
            data.put("type", type.name());
            data.put("relatedEntityType", relatedEntityType != null ? relatedEntityType.name() : null);
            data.put("relatedEntityId", relatedEntityId);
            data.put("unreadCount", unreadCount);
            wsMessage.setData(data);

            webSocketHandler.sendMessageToUser(userEmail, wsMessage);

            logger.info("Notificación enviada vía WebSocket - Usuario: {}, Tipo: {}", userEmail, type);

        } catch (Exception e) {
            // No lanzar excepción - las notificaciones no deben bloquear operaciones principales
            logger.error("Error al enviar notificación WebSocket - Usuario: {}, Tipo: {} - Error: {}", 
                userId, type, e.getMessage(), e);
        }
    }

    /**
     * Envía una notificación a todos los administradores conectados.
     * 
     * @param type Tipo de notificación
     * @param title Título de la notificación
     * @param message Mensaje de la notificación
     * @param relatedEntityType Tipo de entidad relacionada
     * @param relatedEntityId ID de la entidad relacionada
     */
    @Transactional
    public void sendNotificationToAdmins(NotificationType type, String title, String message,
                                       RelatedEntityType relatedEntityType, Long relatedEntityId) {
        try {
            List<User> admins = userRepository.findAllAdmins();

            for (User admin : admins) {
                sendNotificationToUser(admin.getId(), type, title, message, 
                    relatedEntityType, relatedEntityId, null);
            }

            logger.info("Notificación enviada a {} administrador(es) - Tipo: {}", admins.size(), type);

        } catch (Exception e) {
            logger.error("Error al enviar notificación a admins - Tipo: {} - Error: {}", 
                type, e.getMessage(), e);
        }
    }

    /**
     * Envía una actualización de disponibilidad en tiempo real a todos los usuarios conectados.
     * 
     * Se usa cuando cambia la disponibilidad de slots (se crea/cancela un turno, cambia configuración, etc.)
     * 
     * @param date Fecha afectada
     */
    public void broadcastAvailabilityUpdate(java.time.LocalDate date) {
        try {
            WebSocketMessage wsMessage = new WebSocketMessage();
            wsMessage.setType(WebSocketMessageType.AVAILABILITY_UPDATED);
            wsMessage.setTitle("Disponibilidad Actualizada");
            wsMessage.setMessage(String.format("La disponibilidad para el %s ha cambiado. Por favor, actualiza la vista.", date));

            Map<String, Object> data = new HashMap<>();
            data.put("date", date.toString());
            data.put("timestamp", java.time.LocalDateTime.now().toString());
            wsMessage.setData(data);

            // Broadcast a todos los usuarios conectados
            webSocketHandler.broadcastMessage(wsMessage);

            logger.info("Actualización de disponibilidad enviada vía WebSocket - Fecha: {}", date);

        } catch (Exception e) {
            // No lanzar excepción - las actualizaciones no deben bloquear operaciones principales
            logger.error("Error al enviar actualización de disponibilidad WebSocket - Fecha: {} - Error: {}", 
                date, e.getMessage(), e);
        }
    }

    /**
     * Envía una actualización general de disponibilidad en tiempo real a todos los usuarios conectados.
     * 
     * Se usa cuando cambia la configuración del calendario (afecta múltiples fechas o todas las fechas futuras).
     * 
     * @param message Mensaje opcional personalizado
     * @param additionalData Datos adicionales opcionales (ej: configurationVersion)
     */
    public void broadcastGeneralAvailabilityUpdate(String message, Map<String, Object> additionalData) {
        try {
            WebSocketMessage wsMessage = new WebSocketMessage();
            wsMessage.setType(WebSocketMessageType.AVAILABILITY_UPDATED);
            wsMessage.setTitle("Configuración de Calendario Actualizada");
            wsMessage.setMessage(message != null ? message : 
                "La configuración del calendario ha cambiado. Por favor, actualiza la vista de disponibilidad.");

            Map<String, Object> data = new HashMap<>();
            data.put("timestamp", java.time.LocalDateTime.now().toString());
            if (additionalData != null) {
                data.putAll(additionalData);
            }
            wsMessage.setData(data);

            // Broadcast a todos los usuarios conectados
            webSocketHandler.broadcastMessage(wsMessage);

            logger.info("Actualización general de disponibilidad enviada vía WebSocket");

        } catch (Exception e) {
            // No lanzar excepción - las actualizaciones no deben bloquear operaciones principales
            logger.error("Error al enviar actualización general de disponibilidad WebSocket - Error: {}", 
                e.getMessage(), e);
        }
    }

    /**
     * Envía una notificación a todos los usuarios conectados (broadcast).
     * 
     * @param type Tipo de notificación
     * @param title Título de la notificación
     * @param message Mensaje de la notificación
     */
    @Transactional
    public void sendNotificationToAllUsers(NotificationType type, String title, String message) {
        try {
            // Crear mensaje WebSocket
            WebSocketMessage wsMessage = new WebSocketMessage();
            wsMessage.setType(mapNotificationTypeToWebSocketType(type));
            wsMessage.setTitle(title);
            wsMessage.setMessage(message);

            Map<String, Object> data = new HashMap<>();
            data.put("type", type.name());
            wsMessage.setData(data);

            // Broadcast a todos los usuarios conectados
            webSocketHandler.broadcastMessage(wsMessage);

            logger.info("Notificación broadcast enviada - Tipo: {}", type);

        } catch (Exception e) {
            logger.error("Error al enviar notificación broadcast - Tipo: {} - Error: {}", 
                type, e.getMessage(), e);
        }
    }

    /**
     * Envía una actualización del contador de notificaciones no leídas a un usuario específico.
     * 
     * Se usa cuando el usuario marca/elimina notificaciones y el contador cambia.
     * 
     * @param userId ID del usuario
     */
    public void sendUnreadCountUpdate(Long userId) {
        try {
            // Obtener usuario para obtener su email
            Optional<User> userOpt = userRepository.findById(userId);
            if (userOpt.isEmpty()) {
                logger.warn("Usuario no encontrado para enviar actualización de contador - ID: {}", userId);
                return;
            }

            User user = userOpt.get();
            String userEmail = user.getEmail();

            // Contar notificaciones no leídas
            long unreadCount = notificationService.countUnreadNotifications(userId);

            // Crear mensaje WebSocket
            WebSocketMessage wsMessage = new WebSocketMessage();
            wsMessage.setType(WebSocketMessageType.NOTIFICATION_COUNT_UPDATED);
            wsMessage.setTitle("Contador de Notificaciones Actualizado");
            wsMessage.setMessage("El contador de notificaciones no leídas ha sido actualizado");

            // Agregar datos adicionales
            Map<String, Object> data = new HashMap<>();
            data.put("unreadCount", unreadCount);
            data.put("timestamp", java.time.LocalDateTime.now().toString());
            wsMessage.setData(data);

            // Enviar a todas las sesiones del usuario
            webSocketHandler.sendMessageToUser(userEmail, wsMessage);

            logger.debug("Actualización de contador de notificaciones enviada vía WebSocket - Usuario: {}, Contador: {}", 
                userEmail, unreadCount);

        } catch (Exception e) {
            // No lanzar excepción - las actualizaciones no deben bloquear operaciones principales
            logger.error("Error al enviar actualización de contador de notificaciones WebSocket - Usuario: {} - Error: {}", 
                userId, e.getMessage(), e);
        }
    }

    /**
     * Mapea NotificationType a WebSocketMessageType.
     */
    private WebSocketMessageType mapNotificationTypeToWebSocketType(NotificationType type) {
        return switch (type) {
            case APPOINTMENT_CREATED -> WebSocketMessageType.APPOINTMENT_CREATED;
            case APPOINTMENT_CONFIRMED -> WebSocketMessageType.APPOINTMENT_CONFIRMED;
            case APPOINTMENT_CANCELLED, APPOINTMENT_CANCELLED_BY_ADMIN -> WebSocketMessageType.APPOINTMENT_CANCELLED;
            case APPOINTMENT_RESCHEDULED -> WebSocketMessageType.APPOINTMENT_RESCHEDULED;
            case APPOINTMENT_EXPIRED -> WebSocketMessageType.APPOINTMENT_EXPIRED;
            case APPOINTMENT_REMINDER -> WebSocketMessageType.APPOINTMENT_CREATED; // Usar tipo genérico
            case RESCHEDULE_REQUEST_PENDING -> WebSocketMessageType.RESCHEDULE_REQUEST_CREATED;
            case RESCHEDULE_REQUEST_APPROVED -> WebSocketMessageType.RESCHEDULE_REQUEST_APPROVED;
            case RESCHEDULE_REQUEST_REJECTED -> WebSocketMessageType.RESCHEDULE_REQUEST_REJECTED;
            case RESCHEDULE_REQUEST_CANCELLED -> WebSocketMessageType.RESCHEDULE_REQUEST_REJECTED; // Usar tipo similar
            default -> WebSocketMessageType.APPOINTMENT_CREATED; // Tipo por defecto
        };
    }
}

