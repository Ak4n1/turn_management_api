package com.ak4n1.turn_management.feature.configuration.service.validation;

import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursConfigRequest;
import com.ak4n1.turn_management.feature.configuration.util.DayNameUtils;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * Validador para configuraciones de horarios diarios.
 * Valida el request y las reglas de negocio relacionadas con DailyHours.
 */
@Component
public class DailyHoursValidator implements ConfigurationValidator<DailyHoursConfigRequest> {

    @Override
    public void validate(DailyHoursConfigRequest request) {
        validateRequest(request);
    }

    /**
     * Valida que el request tenga la estructura correcta.
     * 
     * @param request Request a validar
     * @throws ApiException si la validación falla
     */
    public void validateRequest(DailyHoursConfigRequest request) {
        if (request == null) {
            throw new ApiException("La configuración de horarios no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        if (request.getDailyHours() == null || request.getDailyHours().isEmpty()) {
            throw new ApiException(
                    "Debe proporcionar al menos un día con horarios",
                    HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida que solo días abiertos tengan horarios configurados.
     * 
     * @param dailyHoursList Lista de horarios diarios
     * @param weeklyConfig   Configuración semanal que define qué días están abiertos
     * @throws ApiException si la validación falla
     */
    public void validateOnlyOpenDaysHaveHours(List<DailyHours> dailyHoursList, WeeklyConfig weeklyConfig) {
        if (weeklyConfig == null) {
            throw new ApiException(
                    "La configuración semanal no está definida",
                    HttpStatus.BAD_REQUEST);
        }

        for (DailyHours dailyHours : dailyHoursList) {
            Integer dayOfWeek = dailyHours.getDayOfWeek();
            Boolean isDayOpen = weeklyConfig.isDayOpen(dayOfWeek);

            if (isDayOpen == null || !isDayOpen) {
                String dayName = DayNameUtils.getDayName(dayOfWeek);
                throw new ApiException(
                        String.format(
                                "No se pueden definir horarios para días cerrados. El día '%s' está cerrado según la configuración semanal.",
                                dayName),
                        HttpStatus.BAD_REQUEST);
            }
        }
    }

    /**
     * Valida que no haya superposiciones de rangos horarios en el mismo día.
     * 
     * @param dailyHoursList Lista de horarios diarios
     * @throws ApiException si la validación falla
     */
    public void validateNoOverlaps(List<DailyHours> dailyHoursList) {
        for (DailyHours dailyHours : dailyHoursList) {
            if (!dailyHours.hasNoOverlaps()) {
                String dayName = DayNameUtils.getDayName(dailyHours.getDayOfWeek());
                throw new ApiException(
                        String.format("Los rangos horarios no pueden superponerse. Hay superposiciones en el día '%s'.",
                                dayName),
                        HttpStatus.BAD_REQUEST);
            }
        }
    }

    /**
     * Valida que todos los rangos horarios sean válidos (start < end y formato correcto).
     * 
     * @param dailyHoursList Lista de horarios diarios
     * @throws ApiException si la validación falla
     */
    public void validateTimeRanges(List<DailyHours> dailyHoursList) {
        for (DailyHours dailyHours : dailyHoursList) {
            if (!dailyHours.allRangesValid()) {
                String dayName = DayNameUtils.getDayName(dailyHours.getDayOfWeek());
                throw new ApiException(
                        String.format(
                                "Los rangos horarios del día '%s' no son válidos. El horario de inicio debe ser anterior al horario de fin.",
                                dayName),
                        HttpStatus.BAD_REQUEST);
            }
        }
    }
}
