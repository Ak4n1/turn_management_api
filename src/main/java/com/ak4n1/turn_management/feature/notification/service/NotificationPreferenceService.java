package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.notification.domain.NotificationPreference;
import com.ak4n1.turn_management.feature.notification.domain.NotificationType;
import com.ak4n1.turn_management.feature.notification.dto.request.UpdateNotificationPreferenceRequest;
import com.ak4n1.turn_management.feature.notification.dto.response.NotificationPreferenceResponse;
import com.ak4n1.turn_management.feature.notification.repository.NotificationPreferenceRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Servicio para gestión de preferencias de notificación.
 * Implementa US-T035.
 */
@Service
public class NotificationPreferenceService {

    private static final Logger logger = LoggerFactory.getLogger(NotificationPreferenceService.class);

    private final NotificationPreferenceRepository preferenceRepository;

    public NotificationPreferenceService(NotificationPreferenceRepository preferenceRepository) {
        this.preferenceRepository = preferenceRepository;
    }

    /**
     * Obtiene las preferencias de notificación de un usuario.
     * Si no existen, retorna preferencias por defecto (todas habilitadas).
     * 
     * @param userId ID del usuario
     * @return Preferencias del usuario
     */
    public NotificationPreferenceResponse getPreferences(Long userId) {
        NotificationPreference preference = preferenceRepository.findByUserId(userId)
            .orElseGet(() -> {
                // Crear preferencias por defecto si no existen
                NotificationPreference defaultPref = new NotificationPreference(userId);
                return preferenceRepository.save(defaultPref);
            });

        return mapToResponse(preference);
    }

    /**
     * Actualiza las preferencias de notificación de un usuario.
     * 
     * @param userId ID del usuario
     * @param request Datos actualizados
     * @return Preferencias actualizadas
     */
    @Transactional
    public NotificationPreferenceResponse updatePreferences(Long userId, UpdateNotificationPreferenceRequest request) {
        NotificationPreference preference = preferenceRepository.findByUserId(userId)
            .orElseGet(() -> new NotificationPreference(userId));

        // Actualizar valores
        preference.setEmailEnabled(request.getEmailEnabled());
        preference.setAppointmentCreated(request.getAppointmentCreated());
        preference.setAppointmentConfirmed(request.getAppointmentConfirmed());
        preference.setAppointmentCancelled(request.getAppointmentCancelled());
        preference.setAppointmentRescheduled(request.getAppointmentRescheduled());
        preference.setReminderEnabled(request.getReminderEnabled());
        preference.setReminderHoursBefore(request.getReminderHoursBefore());
        preference.touch();

        preference = preferenceRepository.save(preference);

        logger.info("Preferencias de notificación actualizadas - Usuario: {}, Email habilitado: {}, Recordatorios: {}", 
            userId, request.getEmailEnabled(), request.getReminderEnabled());

        return mapToResponse(preference);
    }

    /**
     * Verifica si un usuario quiere recibir un tipo específico de notificación.
     * 
     * @param userId ID del usuario
     * @param notificationType Tipo de notificación
     * @return true si el usuario quiere recibir la notificación, false en caso contrario
     */
    public boolean shouldSendNotification(Long userId, String notificationType) {
        NotificationPreference preference = preferenceRepository.findByUserId(userId)
            .orElseGet(() -> new NotificationPreference(userId)); // Defaults: todas habilitadas

        // Si los emails están deshabilitados globalmente, no enviar nada
        if (Boolean.FALSE.equals(preference.getEmailEnabled())) {
            return false;
        }

        // Verificar tipo específico
        return switch (notificationType) {
            case "APPOINTMENT_CREATED" -> Boolean.TRUE.equals(preference.getAppointmentCreated());
            case "APPOINTMENT_CONFIRMED" -> Boolean.TRUE.equals(preference.getAppointmentConfirmed());
            case "APPOINTMENT_CANCELLED_BY_USER", "APPOINTMENT_CANCELLED_BY_ADMIN" -> 
                Boolean.TRUE.equals(preference.getAppointmentCancelled());
            case "APPOINTMENT_RESCHEDULED" -> Boolean.TRUE.equals(preference.getAppointmentRescheduled());
            case "APPOINTMENT_REMINDER" -> Boolean.TRUE.equals(preference.getReminderEnabled());
            default -> true; // Por defecto, enviar (para otros tipos de notificaciones)
        };
    }

    /**
     * Verifica si un usuario quiere recibir un tipo específico de notificación (usando enum).
     * 
     * @param userId ID del usuario
     * @param notificationType Tipo de notificación
     * @return true si el usuario quiere recibir la notificación, false en caso contrario
     */
    public boolean shouldReceiveNotification(Long userId, NotificationType notificationType) {
        NotificationPreference preference = preferenceRepository.findByUserId(userId)
            .orElseGet(() -> new NotificationPreference(userId)); // Defaults: todas habilitadas

        // Si los emails están deshabilitados globalmente, no enviar nada
        if (Boolean.FALSE.equals(preference.getEmailEnabled())) {
            return false;
        }

        // Verificar tipo específico
        return switch (notificationType) {
            case APPOINTMENT_CREATED -> Boolean.TRUE.equals(preference.getAppointmentCreated());
            case APPOINTMENT_CONFIRMED -> Boolean.TRUE.equals(preference.getAppointmentConfirmed());
            case APPOINTMENT_CANCELLED, APPOINTMENT_CANCELLED_BY_ADMIN -> 
                Boolean.TRUE.equals(preference.getAppointmentCancelled());
            case APPOINTMENT_RESCHEDULED -> Boolean.TRUE.equals(preference.getAppointmentRescheduled());
            case APPOINTMENT_REMINDER -> Boolean.TRUE.equals(preference.getReminderEnabled());
            // Para notificaciones manuales (ADMIN_ANNOUNCEMENT, SYSTEM_MAINTENANCE, etc.), 
            // siempre enviar si emailEnabled es true
            case ADMIN_ANNOUNCEMENT, SYSTEM_MAINTENANCE, IMPORTANT_UPDATE, PROMOTION, REMINDER -> 
                Boolean.TRUE.equals(preference.getEmailEnabled());
            default -> true; // Por defecto, enviar (para otros tipos de notificaciones)
        };
    }

    /**
     * Obtiene las horas antes del turno para enviar recordatorio.
     * 
     * @param userId ID del usuario
     * @return Horas antes del turno (por defecto: 12)
     */
    public int getReminderHoursBefore(Long userId) {
        NotificationPreference preference = preferenceRepository.findByUserId(userId)
            .orElseGet(() -> new NotificationPreference(userId)); // Defaults: 12 horas
        return preference.getReminderHoursBefore() != null ? preference.getReminderHoursBefore() : 12;
    }

    /**
     * Mapea una entidad a un DTO de respuesta.
     * 
     * @param preference Entidad de preferencias
     * @return DTO de respuesta
     */
    private NotificationPreferenceResponse mapToResponse(NotificationPreference preference) {
        return new NotificationPreferenceResponse(
            preference.getId(),
            preference.getUserId(),
            preference.getEmailEnabled(),
            preference.getAppointmentCreated(),
            preference.getAppointmentConfirmed(),
            preference.getAppointmentCancelled(),
            preference.getAppointmentRescheduled(),
            preference.getReminderEnabled(),
            preference.getReminderHoursBefore(),
            preference.getCreatedAt(),
            preference.getUpdatedAt()
        );
    }
}

