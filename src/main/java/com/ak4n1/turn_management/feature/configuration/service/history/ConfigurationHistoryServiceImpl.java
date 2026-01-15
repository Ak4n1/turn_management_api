package com.ak4n1.turn_management.feature.configuration.service.history;

import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationHistoryResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationVersionResponse;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class ConfigurationHistoryServiceImpl implements ConfigurationHistoryService {

    private static final Logger logger = LoggerFactory.getLogger(ConfigurationHistoryServiceImpl.class);

    private final CalendarConfigurationRepository repository;
    private final AppointmentRepository appointmentRepository;
    private final UserService userService;

    public ConfigurationHistoryServiceImpl(
            CalendarConfigurationRepository repository,
            AppointmentRepository appointmentRepository,
            UserService userService) {
        this.repository = repository;
        this.appointmentRepository = appointmentRepository;
        this.userService = userService;
    }

    @Override
    @Transactional(readOnly = true)
    public ConfigurationHistoryResponse getConfigurationHistory() {
        logger.info("Consultando historial de configuraciones del calendario");

        // 1. Obtener todas las configuraciones ordenadas por versión descendente
        List<CalendarConfiguration> configurations = repository.findAllByOrderByVersionDesc();

        logger.info("Configuraciones encontradas: {}", configurations.size());

        // 2. Mapear a DTOs con información adicional
        List<ConfigurationVersionResponse> versions = new ArrayList<>();
        CalendarConfiguration previousConfig = null;

        for (CalendarConfiguration config : configurations) {
            ConfigurationVersionResponse versionResponse = new ConfigurationVersionResponse();
            versionResponse.setVersionId(config.getId());
            versionResponse.setVersion(config.getVersion());
            versionResponse.setCreatedAt(config.getCreatedAt().toString());
            versionResponse.setCreatedByUserId(config.getCreatedByUserId());
            versionResponse.setNotes(config.getNotes());
            versionResponse.setActive(Boolean.TRUE.equals(config.getActive()));

            // Obtener email del usuario que creó la configuración
            Optional<User> user = userService.findById(config.getCreatedByUserId());
            if (user.isPresent()) {
                versionResponse.setCreatedByEmail(user.get().getEmail());
            }

            // Contar turnos asociados a esta versión
            long appointmentsCount = appointmentRepository.countByCalendarConfigVersion(config.getVersion());
            versionResponse.setAppointmentsCount((int) appointmentsCount);

            // Detectar cambios comparando con la versión anterior
            List<String> changes = detectChanges(config, previousConfig);
            versionResponse.setChanges(changes);

            versions.add(versionResponse);
            previousConfig = config;
        }

        logger.info("Historial generado - Total versiones: {}", versions.size());

        return new ConfigurationHistoryResponse(versions, versions.size());
    }

    /**
     * Detecta los cambios entre dos versiones de configuración.
     */
    private List<String> detectChanges(CalendarConfiguration current, CalendarConfiguration previous) {
        List<String> changes = new ArrayList<>();

        if (previous == null) {
            // Primera versión
            changes.add("Configuración inicial creada");
            if (current.getAppointmentDurationMinutes() != null) {
                changes.add(String.format("Duración de turno: %d minutos", current.getAppointmentDurationMinutes()));
            }
            if (current.getWeeklyConfig() != null) {
                WeeklyConfig weekly = current.getWeeklyConfig();
                List<String> daysOpen = new ArrayList<>();
                if (Boolean.TRUE.equals(weekly.getMonday()))
                    daysOpen.add("lunes");
                if (Boolean.TRUE.equals(weekly.getTuesday()))
                    daysOpen.add("martes");
                if (Boolean.TRUE.equals(weekly.getWednesday()))
                    daysOpen.add("miércoles");
                if (Boolean.TRUE.equals(weekly.getThursday()))
                    daysOpen.add("jueves");
                if (Boolean.TRUE.equals(weekly.getFriday()))
                    daysOpen.add("viernes");
                if (Boolean.TRUE.equals(weekly.getSaturday()))
                    daysOpen.add("sábado");
                if (Boolean.TRUE.equals(weekly.getSunday()))
                    daysOpen.add("domingo");
                if (!daysOpen.isEmpty()) {
                    changes.add(String.format("Días abiertos: %s", String.join(", ", daysOpen)));
                }
            }
            return changes;
        }

        // Comparar duración de turnos
        Integer currentDuration = current.getAppointmentDurationMinutes();
        Integer previousDuration = previous.getAppointmentDurationMinutes();
        if (currentDuration != null && previousDuration != null && !currentDuration.equals(previousDuration)) {
            changes.add(
                    String.format("Duración de turno cambiada de %d a %d minutos", previousDuration, currentDuration));
        } else if (currentDuration != null && previousDuration == null) {
            changes.add(String.format("Duración de turno configurada: %d minutos", currentDuration));
        } else if (currentDuration == null && previousDuration != null) {
            changes.add("Duración de turno eliminada");
        }

        // Comparar configuración semanal
        WeeklyConfig currentWeekly = current.getWeeklyConfig();
        WeeklyConfig previousWeekly = previous.getWeeklyConfig();
        if (currentWeekly != null && previousWeekly != null) {
            if (!java.util.Objects.equals(currentWeekly.getMonday(), previousWeekly.getMonday())) {
                changes.add(String.format("Lunes: %s",
                        Boolean.TRUE.equals(currentWeekly.getMonday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getTuesday(), previousWeekly.getTuesday())) {
                changes.add(String.format("Martes: %s",
                        Boolean.TRUE.equals(currentWeekly.getTuesday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getWednesday(), previousWeekly.getWednesday())) {
                changes.add(String.format("Miércoles: %s",
                        Boolean.TRUE.equals(currentWeekly.getWednesday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getThursday(), previousWeekly.getThursday())) {
                changes.add(String.format("Jueves: %s",
                        Boolean.TRUE.equals(currentWeekly.getThursday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getFriday(), previousWeekly.getFriday())) {
                changes.add(String.format("Viernes: %s",
                        Boolean.TRUE.equals(currentWeekly.getFriday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getSaturday(), previousWeekly.getSaturday())) {
                changes.add(String.format("Sábado: %s",
                        Boolean.TRUE.equals(currentWeekly.getSaturday()) ? "abierto" : "cerrado"));
            }
            if (!java.util.Objects.equals(currentWeekly.getSunday(), previousWeekly.getSunday())) {
                changes.add(String.format("Domingo: %s",
                        Boolean.TRUE.equals(currentWeekly.getSunday()) ? "abierto" : "cerrado"));
            }
        }

        // Comparar horarios diarios (simplificado - solo detecta si hay cambios en cantidad)
        int currentDailyHoursCount = current.getDailyHours() != null ? current.getDailyHours().size() : 0;
        int previousDailyHoursCount = previous.getDailyHours() != null ? previous.getDailyHours().size() : 0;
        if (currentDailyHoursCount != previousDailyHoursCount) {
            changes.add(String.format("Horarios diarios: %d configurados (anterior: %d)", currentDailyHoursCount,
                    previousDailyHoursCount));
        }

        // Si no hay cambios detectados, agregar mensaje genérico
        if (changes.isEmpty()) {
            changes.add("Configuración actualizada (cambios menores)");
        }

        return changes;
    }
}
