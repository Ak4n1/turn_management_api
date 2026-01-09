package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.repository.UserRepository;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.domain.RelatedEntityType;
import com.ak4n1.turn_management.feature.notification.domain.SystemNotification;
import com.ak4n1.turn_management.feature.notification.dto.request.SendManualNotificationRequest;
import com.ak4n1.turn_management.feature.notification.dto.response.SendManualNotificationResponse;
import com.ak4n1.turn_management.feature.notification.dto.response.SystemNotificationResponse;
import com.ak4n1.turn_management.feature.notification.repository.SystemNotificationRepository;
import com.ak4n1.turn_management.shared.websocket.AppointmentWebSocketHandler;
import com.ak4n1.turn_management.shared.websocket.dto.WebSocketMessage;
import com.ak4n1.turn_management.shared.websocket.util.WebSocketMessageType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Servicio para gestión de notificaciones del sistema.
 * 
 * Implementa US-T025.5 y US-N011.
 */
@Service
public class SystemNotificationService {

    private static final Logger logger = LoggerFactory.getLogger(SystemNotificationService.class);

    private final SystemNotificationRepository notificationRepository;
    private final UserRepository userRepository;
    private final AppointmentWebSocketHandler webSocketHandler;
    private final NotificationPreferenceService preferenceService;

    public SystemNotificationService(SystemNotificationRepository notificationRepository,
                                     UserRepository userRepository,
                                     AppointmentWebSocketHandler webSocketHandler,
                                     NotificationPreferenceService preferenceService) {
        this.notificationRepository = notificationRepository;
        this.userRepository = userRepository;
        this.webSocketHandler = webSocketHandler;
        this.preferenceService = preferenceService;
    }

    /**
     * Crea una notificación en el sistema.
     */
    @Transactional
    public SystemNotification createNotification(NotificationType type, Long recipientId, 
                                                 String title, String message,
                                                 RelatedEntityType relatedEntityType, 
                                                 Long relatedEntityId) {
        logger.info("Creando notificación - Tipo: {}, Destinatario: {}, Título: {}",
            type, recipientId, title);

        SystemNotification notification = new SystemNotification(
            type, recipientId, title, message, relatedEntityType, relatedEntityId);

        SystemNotification saved = notificationRepository.save(notification);
        logger.info("Notificación creada - ID: {}, Destinatario: {}", saved.getId(), recipientId);

        return saved;
    }

    /**
     * Obtiene todas las notificaciones de un usuario con paginación.
     */
    @Transactional(readOnly = true)
    public Page<SystemNotification> getNotifications(Long recipientId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        return notificationRepository.findByRecipientIdOrderByCreatedAtDesc(recipientId, pageable);
    }

    /**
     * Obtiene todas las notificaciones no leídas de un usuario.
     */
    @Transactional(readOnly = true)
    public List<SystemNotification> getUnreadNotifications(Long recipientId) {
        return notificationRepository.findByRecipientIdAndReadFalseOrderByCreatedAtDesc(recipientId);
    }

    /**
     * Cuenta las notificaciones no leídas de un usuario.
     */
    @Transactional(readOnly = true)
    public long countUnreadNotifications(Long recipientId) {
        return notificationRepository.countByRecipientIdAndReadFalse(recipientId);
    }

    /**
     * Marca una notificación como leída.
     */
    @Transactional
    public SystemNotification markAsRead(Long notificationId, Long recipientId) {
        SystemNotification notification = notificationRepository.findById(notificationId)
            .orElseThrow(() -> new RuntimeException("Notificación no encontrada"));

        // Validar que la notificación pertenece al usuario
        if (!notification.getRecipientId().equals(recipientId)) {
            throw new RuntimeException("No tienes permiso para marcar esta notificación como leída");
        }

        notification.markAsRead();
        SystemNotification saved = notificationRepository.save(notification);
        logger.info("Notificación marcada como leída - ID: {}, Usuario: {}", notificationId, recipientId);

        return saved;
    }

    /**
     * Busca notificaciones por tipo y entidad relacionada.
     */
    @Transactional(readOnly = true)
    public List<SystemNotification> findByTypeAndRelatedEntity(NotificationType type,
                                                               RelatedEntityType relatedEntityType,
                                                               Long relatedEntityId) {
        return notificationRepository.findByTypeAndRelatedEntityTypeAndRelatedEntityId(
            type, relatedEntityType, relatedEntityId);
    }

    /**
     * Obtiene notificaciones con filtros avanzados.
     */
    @Transactional(readOnly = true)
    public Page<SystemNotification> getNotificationsWithFilters(Long recipientId, NotificationType type,
                                                                 Boolean read, String search,
                                                                 int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        
        if (type != null && read != null && search != null && !search.trim().isEmpty()) {
            return notificationRepository.findByRecipientIdAndTypeAndReadAndSearch(
                recipientId, type, read, search.trim(), pageable);
        } else if (type != null && read != null) {
            return notificationRepository.findByRecipientIdAndTypeAndRead(recipientId, type, read, pageable);
        } else if (type != null) {
            return notificationRepository.findByRecipientIdAndType(recipientId, type, pageable);
        } else if (read != null) {
            return notificationRepository.findByRecipientIdAndRead(recipientId, read, pageable);
        } else if (search != null && !search.trim().isEmpty()) {
            return notificationRepository.findByRecipientIdAndSearch(
                recipientId, search.trim(), pageable);
        } else {
            return notificationRepository.findByRecipientIdOrderByCreatedAtDesc(recipientId, pageable);
        }
    }

    /**
     * Marca todas las notificaciones de un usuario como leídas.
     */
    @Transactional
    public int markAllAsRead(Long recipientId) {
        List<SystemNotification> unreadNotifications = 
            notificationRepository.findByRecipientIdAndReadFalse(recipientId);
        
        for (SystemNotification notification : unreadNotifications) {
            notification.markAsRead();
        }
        
        notificationRepository.saveAll(unreadNotifications);
        logger.info("Marcadas {} notificaciones como leídas - Usuario: {}", unreadNotifications.size(), recipientId);
        
        return unreadNotifications.size();
    }

    /**
     * Elimina una notificación.
     */
    @Transactional
    public void deleteNotification(Long notificationId, Long recipientId) {
        SystemNotification notification = notificationRepository.findById(notificationId)
            .orElseThrow(() -> new RuntimeException("Notificación no encontrada"));

        if (!notification.getRecipientId().equals(recipientId)) {
            throw new RuntimeException("No tienes permiso para eliminar esta notificación");
        }

        notificationRepository.delete(notification);
        logger.info("Notificación eliminada - ID: {}, Usuario: {}", notificationId, recipientId);
    }

    /**
     * Elimina múltiples notificaciones.
     */
    @Transactional
    public int deleteNotifications(List<Long> notificationIds, Long recipientId) {
        List<SystemNotification> notifications = notificationRepository.findAllById(notificationIds);
        
        // Filtrar solo las que pertenecen al usuario
        List<SystemNotification> userNotifications = notifications.stream()
            .filter(n -> n.getRecipientId().equals(recipientId))
            .toList();
        
        notificationRepository.deleteAll(userNotifications);
        logger.info("Eliminadas {} notificaciones - Usuario: {}", userNotifications.size(), recipientId);
        
        return userNotifications.size();
    }

    /**
     * Mapea una entidad SystemNotification a un DTO SystemNotificationResponse.
     */
    public SystemNotificationResponse mapToResponse(SystemNotification notification) {
        SystemNotificationResponse response = new SystemNotificationResponse();
        response.setId(notification.getId());
        response.setType(notification.getType());
        response.setRecipientId(notification.getRecipientId());
        response.setTitle(notification.getTitle());
        response.setMessage(notification.getMessage());
        response.setRelatedEntityType(notification.getRelatedEntityType());
        response.setRelatedEntityId(notification.getRelatedEntityId());
        response.setRead(notification.getRead());
        response.setReadAt(notification.getReadAt());
        response.setCreatedAt(notification.getCreatedAt());
        response.setUpdatedAt(notification.getUpdatedAt());
        return response;
    }

    /**
     * Envía notificaciones manuales a todos los usuarios o a un usuario específico.
     * 
     * Implementa US-N011.
     * 
     * @param recipientType Tipo de destinatario (ALL_USERS o SPECIFIC_USER)
     * @param recipientEmail Email del usuario específico (requerido si recipientType es SPECIFIC_USER)
     * @param type Tipo de notificación
     * @param title Título de la notificación
     * @param message Mensaje de la notificación
     * @return Resumen del envío
     */
    @Transactional
    public SendManualNotificationResponse sendManualNotification(
            SendManualNotificationRequest.RecipientType recipientType,
            String recipientEmail,
            NotificationType type,
            String title,
            String message) {
        
        logger.info("Iniciando envío manual de notificación - Tipo: {}, Destinatario: {}, Tipo de notificación: {}",
            recipientType, recipientEmail != null ? recipientEmail : "ALL_USERS", type);

        List<User> recipients = new ArrayList<>();
        
        // Determinar destinatarios
        if (recipientType == SendManualNotificationRequest.RecipientType.ALL_USERS) {
            recipients = userRepository.findAllEnabled();
            logger.info("Enviando a todos los usuarios habilitados - Total: {}", recipients.size());
        } else if (recipientType == SendManualNotificationRequest.RecipientType.SPECIFIC_USER) {
            if (recipientEmail == null || recipientEmail.isBlank()) {
                throw new RuntimeException("Email del destinatario es requerido cuando recipientType es SPECIFIC_USER");
            }
            Optional<User> userOpt = userRepository.findByEmail(recipientEmail);
            if (userOpt.isEmpty()) {
                throw new RuntimeException("Usuario con email " + recipientEmail + " no encontrado");
            }
            User user = userOpt.get();
            if (!user.getEnabled()) {
                throw new RuntimeException("Usuario con email " + recipientEmail + " está deshabilitado");
            }
            recipients.add(user);
            logger.info("Enviando a usuario específico - Email: {}", recipientEmail);
        }

        int totalRecipients = recipients.size();
        int sentImmediately = 0;
        int pendingDelivery = 0;
        int excludedByPreferences = 0;
        List<Long> notificationIds = new ArrayList<>();

        // Crear mensaje WebSocket
        WebSocketMessage wsMessage = new WebSocketMessage();
        wsMessage.setType(mapNotificationTypeToWebSocketType(type));
        wsMessage.setTitle(title);
        wsMessage.setMessage(message);
        Map<String, Object> data = new HashMap<>();
        data.put("type", type.name());
        wsMessage.setData(data);

        // Procesar cada destinatario
        for (User recipient : recipients) {
            try {
                // Verificar preferencias de notificación
                boolean shouldSend = preferenceService.shouldReceiveNotification(recipient.getId(), type);
                
                if (!shouldSend) {
                    excludedByPreferences++;
                    logger.debug("Notificación omitida para usuario {} debido a preferencias", recipient.getEmail());
                    continue;
                }

                // Crear notificación en BD
                SystemNotification notification = createNotification(
                    type, recipient.getId(), title, message, null, null);
                notificationIds.add(notification.getId());

                // Intentar enviar vía WebSocket
                try {
                    webSocketHandler.sendMessageToUser(recipient.getEmail(), wsMessage);
                    sentImmediately++;
                    logger.debug("Notificación enviada inmediatamente vía WebSocket - Usuario: {}", recipient.getEmail());
                } catch (Exception e) {
                    // Usuario no conectado, notificación queda pendiente
                    pendingDelivery++;
                    logger.debug("Usuario no conectado, notificación guardada para entrega posterior - Usuario: {}", 
                        recipient.getEmail());
                }

            } catch (Exception e) {
                logger.error("Error al procesar notificación para usuario {}: {}", 
                    recipient.getEmail(), e.getMessage(), e);
                // Continuar con los demás usuarios aunque uno falle
            }
        }

        // Si es ALL_USERS, también hacer broadcast para usuarios conectados que no están en la lista
        if (recipientType == SendManualNotificationRequest.RecipientType.ALL_USERS) {
            try {
                webSocketHandler.broadcastMessage(wsMessage);
                logger.info("Notificación broadcast enviada a todos los usuarios conectados");
            } catch (Exception e) {
                logger.error("Error al enviar broadcast: {}", e.getMessage(), e);
            }
        }

        String responseMessage = String.format(
            "Notificación enviada a %d destinatario(s). Enviadas inmediatamente: %d, Pendientes: %d, Excluidas por preferencias: %d",
            totalRecipients, sentImmediately, pendingDelivery, excludedByPreferences);

        logger.info("Envío manual de notificación completado - {}", responseMessage);

        return new SendManualNotificationResponse(
            true, totalRecipients, sentImmediately, pendingDelivery, excludedByPreferences,
            notificationIds, responseMessage);
    }

    /**
     * Obtiene todas las notificaciones del sistema con filtros avanzados (para admin).
     * 
     * Implementa US-N006.
     * 
     * @param recipientId ID del usuario (opcional)
     * @param type Tipo de notificación (opcional)
     * @param read Estado de lectura (opcional)
     * @param dateFrom Fecha desde (opcional)
     * @param dateTo Fecha hasta (opcional)
     * @param search Búsqueda por texto (opcional)
     * @param page Número de página
     * @param size Tamaño de página
     * @return Página de notificaciones
     */
    @Transactional(readOnly = true)
    public Page<SystemNotification> getAdminNotifications(Long recipientId, NotificationType type, Boolean read,
                                                          LocalDateTime dateFrom, LocalDateTime dateTo, String search,
                                                          int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        
        return notificationRepository.findAdminNotificationsWithFilters(
            recipientId, type, read, dateFrom, dateTo, search != null && !search.trim().isEmpty() ? search.trim() : null,
            pageable);
    }

    /**
     * Obtiene estadísticas de notificaciones del sistema (para admin).
     * 
     * Implementa US-N006.
     * 
     * @return Mapa con estadísticas
     */
    @Transactional(readOnly = true)
    public Map<String, Object> getAdminNotificationStats() {
        long totalNotifications = notificationRepository.count();
        long unreadNotifications = notificationRepository.countUnreadNotifications();
        long readNotifications = totalNotifications - unreadNotifications;
        
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalNotifications", totalNotifications);
        stats.put("unreadNotifications", unreadNotifications);
        stats.put("readNotifications", readNotifications);
        stats.put("readPercentage", totalNotifications > 0 ? (readNotifications * 100.0 / totalNotifications) : 0.0);
        
        return stats;
    }

    /**
     * Mapea NotificationType a WebSocketMessageType.
     */
    private WebSocketMessageType mapNotificationTypeToWebSocketType(NotificationType type) {
        return switch (type) {
            case ADMIN_ANNOUNCEMENT, SYSTEM_MAINTENANCE, IMPORTANT_UPDATE, PROMOTION, REMINDER -> 
                WebSocketMessageType.AVAILABILITY_UPDATED; // Tipo genérico para notificaciones manuales
            case APPOINTMENT_CREATED -> WebSocketMessageType.APPOINTMENT_CREATED;
            case APPOINTMENT_CONFIRMED -> WebSocketMessageType.APPOINTMENT_CONFIRMED;
            case APPOINTMENT_CANCELLED, APPOINTMENT_CANCELLED_BY_ADMIN -> WebSocketMessageType.APPOINTMENT_CANCELLED;
            case APPOINTMENT_RESCHEDULED -> WebSocketMessageType.APPOINTMENT_RESCHEDULED;
            case APPOINTMENT_EXPIRED -> WebSocketMessageType.APPOINTMENT_EXPIRED;
            case APPOINTMENT_REMINDER -> WebSocketMessageType.APPOINTMENT_CREATED;
            case RESCHEDULE_REQUEST_PENDING -> WebSocketMessageType.RESCHEDULE_REQUEST_CREATED;
            case RESCHEDULE_REQUEST_APPROVED -> WebSocketMessageType.RESCHEDULE_REQUEST_APPROVED;
            case RESCHEDULE_REQUEST_REJECTED -> WebSocketMessageType.RESCHEDULE_REQUEST_REJECTED;
            case RESCHEDULE_REQUEST_CANCELLED -> WebSocketMessageType.RESCHEDULE_REQUEST_REJECTED;
            default -> WebSocketMessageType.AVAILABILITY_UPDATED;
        };
    }
}

