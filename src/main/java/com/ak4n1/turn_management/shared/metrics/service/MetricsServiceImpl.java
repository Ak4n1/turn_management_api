package com.ak4n1.turn_management.shared.metrics.service;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import com.ak4n1.turn_management.feature.configuration.service.CalendarConfigurationService;
import com.ak4n1.turn_management.shared.exception.ApiException;
import com.ak4n1.turn_management.shared.metrics.dto.SystemMetricsResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Implementación del servicio de métricas del sistema.
 * 
 * Implementa US-T022.
 */
@Service
public class MetricsServiceImpl implements MetricsService {

    private static final Logger logger = LoggerFactory.getLogger(MetricsServiceImpl.class);

    private final AppointmentRepository appointmentRepository;
    private final CalendarConfigurationService calendarConfigurationService;
    private final CalendarConfigurationRepository calendarConfigurationRepository;

    public MetricsServiceImpl(
            AppointmentRepository appointmentRepository,
            CalendarConfigurationService calendarConfigurationService,
            CalendarConfigurationRepository calendarConfigurationRepository) {
        this.appointmentRepository = appointmentRepository;
        this.calendarConfigurationService = calendarConfigurationService;
        this.calendarConfigurationRepository = calendarConfigurationRepository;
    }

    /**
     * Obtiene métricas del sistema para un rango de fechas.
     */
    @Override
    public SystemMetricsResponse getSystemMetrics(LocalDate startDate, LocalDate endDate) {
        logger.info("Consultando métricas del sistema - Desde: {}, Hasta: {}", startDate, endDate);

        // 1. Validar rango de fechas
        validateDateRange(startDate, endDate);

        // 2. Obtener todos los turnos en el rango
        List<Appointment> appointments = appointmentRepository
            .findAllByAppointmentDateBetween(startDate, endDate);

        logger.info("Turnos encontrados en el rango: {}", appointments.size());

        // 4. Calcular métricas de turnos
        SystemMetricsResponse.AppointmentsMetricsResponse appointmentsMetrics = 
            calculateAppointmentsMetrics(appointments);

        // 5. Calcular métricas de slots
        SystemMetricsResponse.SlotsMetricsResponse slotsMetrics = 
            calculateSlotsMetrics(startDate, endDate, appointments);

        // 6. Calcular días más saturados
        List<SystemMetricsResponse.TopDayResponse> topDays = 
            calculateTopDays(appointments);

        // 7. Construir respuesta
        SystemMetricsResponse response = new SystemMetricsResponse();
        response.setPeriod(new SystemMetricsResponse.PeriodResponse(
            startDate.toString(),
            endDate.toString()
        ));
        response.setAppointments(appointmentsMetrics);
        response.setSlots(slotsMetrics);
        response.setTopDays(topDays);

        logger.info("Métricas calculadas - Turnos creados: {}, Cancelados: {}, No-shows: {}",
            appointmentsMetrics.getCreated(), appointmentsMetrics.getCancelled(), 
            appointmentsMetrics.getNoShow());

        return response;
    }

    /**
     * Valida el rango de fechas.
     */
    private void validateDateRange(LocalDate startDate, LocalDate endDate) {
        if (startDate == null || endDate == null) {
            throw new ApiException(
                "Las fechas 'startDate' y 'endDate' son requeridas",
                HttpStatus.BAD_REQUEST);
        }

        if (startDate.isAfter(endDate)) {
            throw new ApiException(
                "La fecha 'startDate' no puede ser posterior a 'endDate'",
                HttpStatus.BAD_REQUEST);
        }

        // Limitar el rango a máximo 1 año
        if (startDate.plusYears(1).isBefore(endDate) || startDate.plusYears(1).equals(endDate.plusDays(1))) {
            throw new ApiException(
                "El rango de fechas no puede exceder 1 año",
                HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Calcula métricas de turnos.
     */
    private SystemMetricsResponse.AppointmentsMetricsResponse calculateAppointmentsMetrics(
            List<Appointment> appointments) {
        
        SystemMetricsResponse.AppointmentsMetricsResponse metrics = 
            new SystemMetricsResponse.AppointmentsMetricsResponse();

        // Contar por estado
        long created = appointments.stream()
            .filter(a -> a.getState() == AppointmentState.CREATED)
            .count();
        
        long confirmed = appointments.stream()
            .filter(a -> a.getState() == AppointmentState.CONFIRMED)
            .count();
        
        long cancelled = appointments.stream()
            .filter(a -> a.getState() == AppointmentState.CANCELLED)
            .count();
        
        long noShow = appointments.stream()
            .filter(a -> a.getState() == AppointmentState.NO_SHOW)
            .count();

        metrics.setCreated(created);
        metrics.setConfirmed(confirmed);
        metrics.setCancelled(cancelled);
        metrics.setNoShow(noShow);

        // Calcular tasa de cancelación (cancelados / creados * 100)
        if (created > 0) {
            double cancellationRate = (double) cancelled / created * 100.0;
            metrics.setCancellationRate(Math.round(cancellationRate * 10.0) / 10.0); // Redondear a 1 decimal
        } else {
            metrics.setCancellationRate(0.0);
        }

        return metrics;
    }

    /**
     * Calcula métricas de slots.
     */
    private SystemMetricsResponse.SlotsMetricsResponse calculateSlotsMetrics(
            LocalDate startDate, LocalDate endDate, List<Appointment> appointments) {
        
        SystemMetricsResponse.SlotsMetricsResponse metrics = 
            new SystemMetricsResponse.SlotsMetricsResponse();

        // Obtener duración de turnos de la configuración activa
        Integer appointmentDurationMinutes = null;
        var activeConfig = calendarConfigurationRepository.findByActiveTrue();
        if (activeConfig.isPresent()) {
            appointmentDurationMinutes = activeConfig.get().getAppointmentDurationMinutes();
        }
        
        // Si no hay duración configurada, usar 30 minutos por defecto
        if (appointmentDurationMinutes == null || appointmentDurationMinutes <= 0) {
            appointmentDurationMinutes = 30;
            logger.warn("No hay duración de turnos configurada, usando 30 minutos por defecto");
        }

        // Calcular slots ofrecidos usando el servicio de configuración
        long offeredSlots = 0;
        LocalDate currentDate = startDate;
        
        while (!currentDate.isAfter(endDate)) {
            try {
                AvailabilityResponse availability = calendarConfigurationService.checkAvailability(currentDate);
                if (availability.getIsAvailable() && availability.getTimeRanges() != null) {
                    // Contar slots disponibles usando la duración real de turnos
                    int slotsInDay = 0;
                    for (TimeRangeResponse timeRange : availability.getTimeRanges()) {
                        LocalTime start = LocalTime.parse(timeRange.getStart());
                        LocalTime end = LocalTime.parse(timeRange.getEnd());
                        // Calcular slots según la duración configurada
                        long minutes = java.time.Duration.between(start, end).toMinutes();
                        slotsInDay += (int) (minutes / appointmentDurationMinutes);
                    }
                    offeredSlots += slotsInDay;
                }
            } catch (Exception e) {
                logger.warn("No se pudo obtener disponibilidad para la fecha {}: {}", currentDate, e.getMessage());
            }
            currentDate = currentDate.plusDays(1);
        }

        // Contar slots usados (turnos confirmados, rescheduled, y no-show)
        long usedSlots = appointments.stream()
            .filter(a -> a.getState() == AppointmentState.CONFIRMED || 
                        a.getState() == AppointmentState.RESCHEDULED ||
                        a.getState() == AppointmentState.NO_SHOW)
            .count();

        metrics.setOffered(offeredSlots);
        metrics.setUsed(usedSlots);

        // Calcular tasa de uso
        if (offeredSlots > 0) {
            double usageRate = (double) usedSlots / offeredSlots * 100.0;
            metrics.setUsageRate(Math.round(usageRate * 10.0) / 10.0); // Redondear a 1 decimal
        } else {
            metrics.setUsageRate(0.0);
        }

        return metrics;
    }

    /**
     * Calcula los días más saturados (top 10).
     */
    private List<SystemMetricsResponse.TopDayResponse> calculateTopDays(List<Appointment> appointments) {
        // Agrupar por fecha y contar turnos
        Map<LocalDate, Long> appointmentsByDate = appointments.stream()
            .filter(a -> a.getState() == AppointmentState.CONFIRMED || 
                        a.getState() == AppointmentState.RESCHEDULED ||
                        a.getState() == AppointmentState.NO_SHOW)
            .collect(Collectors.groupingBy(
                Appointment::getAppointmentDate,
                Collectors.counting()
            ));

        // Ordenar por cantidad de turnos (descendente) y tomar top 10
        return appointmentsByDate.entrySet().stream()
            .sorted(Map.Entry.<LocalDate, Long>comparingByValue().reversed())
            .limit(10)
            .map(entry -> new SystemMetricsResponse.TopDayResponse(
                entry.getKey().toString(),
                entry.getValue()
            ))
            .collect(Collectors.toList());
    }
}

