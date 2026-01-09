package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.notification.repository.SystemNotificationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.ZoneId;

/**
 * Servicio para limpieza automática de notificaciones antiguas.
 * 
 * Implementa US-N009: Limpieza automática de notificaciones antiguas.
 */
@Service
public class NotificationCleanupService {

    private static final Logger logger = LoggerFactory.getLogger(NotificationCleanupService.class);

    private final SystemNotificationRepository notificationRepository;

    /**
     * Días de retención de notificaciones leídas (default: 30 días).
     * Configurable desde application.properties.
     */
    @Value("${notification.retention-days-read:30}")
    private int retentionDaysRead;

    /**
     * Días de retención de notificaciones no leídas (default: 90 días).
     * Configurable desde application.properties.
     */
    @Value("${notification.retention-days-unread:90}")
    private int retentionDaysUnread;

    public NotificationCleanupService(SystemNotificationRepository notificationRepository) {
        this.notificationRepository = notificationRepository;
    }

    /**
     * Job programado que limpia notificaciones antiguas.
     * 
     * Se ejecuta diariamente a las 3:00 AM (GMT-3).
     * 
     * Implementa US-N009:
     * - Elimina notificaciones leídas más antiguas que retentionDaysRead
     * - Elimina notificaciones no leídas más antiguas que retentionDaysUnread
     * - Mantiene un registro de cuántas notificaciones se eliminaron
     */
    @Scheduled(cron = "0 0 3 * * ?") // Diariamente a las 3 AM
    @Transactional
    public void cleanupOldNotifications() {
        logger.info("Iniciando limpieza automática de notificaciones antiguas");

        LocalDateTime now = getNowGMT3();
        LocalDateTime thresholdRead = now.minusDays(retentionDaysRead);
        LocalDateTime thresholdUnread = now.minusDays(retentionDaysUnread);

        logger.info("Threshold para notificaciones leídas: {} (retención: {} días)", 
            thresholdRead, retentionDaysRead);
        logger.info("Threshold para notificaciones no leídas: {} (retención: {} días)", 
            thresholdUnread, retentionDaysUnread);

        // Eliminar notificaciones leídas antiguas
        int deletedRead = notificationRepository.deleteByReadTrueAndCreatedAtBefore(thresholdRead);
        logger.info("Notificaciones leídas eliminadas: {}", deletedRead);

        // Eliminar notificaciones no leídas antiguas
        int deletedUnread = notificationRepository.deleteByReadFalseAndCreatedAtBefore(thresholdUnread);
        logger.info("Notificaciones no leídas eliminadas: {}", deletedUnread);

        int totalDeleted = deletedRead + deletedUnread;
        logger.info("Limpieza completada - Total eliminadas: {} (Leídas: {}, No leídas: {})", 
            totalDeleted, deletedRead, deletedUnread);
    }

    /**
     * Obtiene la fecha y hora actual en GMT-3 (Argentina).
     */
    private LocalDateTime getNowGMT3() {
        return LocalDateTime.now(ZoneId.of("America/Argentina/Buenos_Aires"));
    }
}

