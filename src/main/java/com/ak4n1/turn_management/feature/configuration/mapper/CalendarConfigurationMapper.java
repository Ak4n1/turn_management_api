package com.ak4n1.turn_management.feature.configuration.mapper;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.TimeRangeRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.WeeklyConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarConfigurationResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.DailyHoursResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.WeeklyConfigResponse;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
public class CalendarConfigurationMapper {

    /**
     * Convierte WeeklyConfigRequest a WeeklyConfig (entidad embebida).
     */
    public WeeklyConfig toWeeklyConfig(WeeklyConfigRequest request) {
        if (request == null) {
            return null;
        }
        return new WeeklyConfig(
            request.getMonday(),
            request.getTuesday(),
            request.getWednesday(),
            request.getThursday(),
            request.getFriday(),
            request.getSaturday(),
            request.getSunday()
        );
    }

    /**
     * Convierte WeeklyConfig (entidad embebida) a WeeklyConfigResponse.
     */
    public WeeklyConfigResponse toWeeklyConfigResponse(WeeklyConfig weeklyConfig) {
        if (weeklyConfig == null) {
            return null;
        }
        return new WeeklyConfigResponse(
            weeklyConfig.getMonday(),
            weeklyConfig.getTuesday(),
            weeklyConfig.getWednesday(),
            weeklyConfig.getThursday(),
            weeklyConfig.getFriday(),
            weeklyConfig.getSaturday(),
            weeklyConfig.getSunday()
        );
    }

    /**
     * Convierte CalendarConfiguration (entidad) a CalendarConfigurationResponse.
     */
    public CalendarConfigurationResponse toResponse(CalendarConfiguration configuration) {
        if (configuration == null) {
            return null;
        }
        CalendarConfigurationResponse response = new CalendarConfigurationResponse();
        response.setId(configuration.getId());
        response.setVersion(configuration.getVersion());
        response.setActive(configuration.getActive());
        response.setWeeklyConfig(toWeeklyConfigResponse(configuration.getWeeklyConfig()));
        response.setAppointmentDurationMinutes(configuration.getAppointmentDurationMinutes());
        response.setCreatedByUserId(configuration.getCreatedByUserId());
        response.setNotes(configuration.getNotes());
        response.setCreatedAt(configuration.getCreatedAt());
        response.setUpdatedAt(configuration.getUpdatedAt());
        
        // Mapear horarios diarios
        if (configuration.getDailyHours() != null) {
            response.setDailyHours(configuration.getDailyHours().stream()
                .map(this::toDailyHoursResponse)
                .collect(Collectors.toList()));
        }
        
        return response;
    }

    /**
     * Convierte TimeRangeRequest a TimeRange (entidad embebida).
     */
    public TimeRange toTimeRange(TimeRangeRequest request) {
        if (request == null) {
            return null;
        }
        return new TimeRange(request.getStart(), request.getEnd());
    }

    /**
     * Convierte TimeRange (entidad embebida) a TimeRangeResponse.
     */
    public TimeRangeResponse toTimeRangeResponse(TimeRange timeRange) {
        if (timeRange == null) {
            return null;
        }
        return new TimeRangeResponse(timeRange.getStart(), timeRange.getEnd());
    }

    /**
     * Convierte DailyHoursRequest a DailyHours (entidad).
     */
    public DailyHours toDailyHours(DailyHoursRequest request) {
        if (request == null) {
            return null;
        }
        DailyHours dailyHours = new DailyHours(request.getDayOfWeek());
        
        if (request.getTimeRanges() != null) {
            List<TimeRange> timeRanges = request.getTimeRanges().stream()
                .map(this::toTimeRange)
                .collect(Collectors.toList());
            dailyHours.setTimeRanges(timeRanges);
        }
        
        return dailyHours;
    }

    /**
     * Convierte DailyHours (entidad) a DailyHoursResponse.
     */
    public DailyHoursResponse toDailyHoursResponse(DailyHours dailyHours) {
        if (dailyHours == null) {
            return null;
        }
        DailyHoursResponse response = new DailyHoursResponse();
        response.setDayOfWeek(dailyHours.getDayOfWeek());
        
        if (dailyHours.getTimeRanges() != null) {
            response.setTimeRanges(dailyHours.getTimeRanges().stream()
                .map(this::toTimeRangeResponse)
                .collect(Collectors.toList()));
        }
        
        return response;
    }

    /**
     * Convierte DailyHoursConfigRequest (Map con días como strings) a List de DailyHours.
     * 
     * El Map tiene claves como "monday", "tuesday", etc. que se convierten a números 1-7.
     * 
     * @param request DTO con el Map de días
     * @return Lista de DailyHours
     */
    public List<DailyHours> toDailyHoursList(DailyHoursConfigRequest request) {
        if (request == null || request.getDailyHours() == null || request.getDailyHours().isEmpty()) {
            return new ArrayList<>();
        }

        Map<String, Integer> dayNameToNumber = new HashMap<>();
        dayNameToNumber.put("monday", 1);
        dayNameToNumber.put("tuesday", 2);
        dayNameToNumber.put("wednesday", 3);
        dayNameToNumber.put("thursday", 4);
        dayNameToNumber.put("friday", 5);
        dayNameToNumber.put("saturday", 6);
        dayNameToNumber.put("sunday", 7);

        List<DailyHours> dailyHoursList = new ArrayList<>();

        for (Map.Entry<String, List<TimeRangeRequest>> entry : request.getDailyHours().entrySet()) {
            String dayName = entry.getKey().toLowerCase();
            Integer dayOfWeek = dayNameToNumber.get(dayName);

            if (dayOfWeek == null) {
                // Día inválido, se ignorará
                continue;
            }

            List<TimeRangeRequest> timeRangeRequests = entry.getValue();
            if (timeRangeRequests == null || timeRangeRequests.isEmpty()) {
                continue;
            }

            DailyHours dailyHours = new DailyHours(dayOfWeek);
            List<TimeRange> timeRanges = timeRangeRequests.stream()
                .map(this::toTimeRange)
                .collect(Collectors.toList());
            dailyHours.setTimeRanges(timeRanges);

            dailyHoursList.add(dailyHours);
        }

        return dailyHoursList;
    }
}

