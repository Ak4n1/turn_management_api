package com.ak4n1.turn_management.feature.configuration.service.validation;

import com.ak4n1.turn_management.feature.configuration.domain.DailyHours;
import com.ak4n1.turn_management.feature.configuration.domain.TimeRange;
import com.ak4n1.turn_management.feature.configuration.dto.request.AppointmentDurationRequest;
import com.ak4n1.turn_management.feature.configuration.util.DayNameUtils;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Validador para configuraciones de duración de turnos.
 * Valida el request y la compatibilidad con los rangos horarios existentes.
 */
@Component
public class AppointmentDurationValidator implements ConfigurationValidator<AppointmentDurationRequest> {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentDurationValidator.class);

    @Override
    public void validate(AppointmentDurationRequest request) {
        validateRequest(request);
    }

    /**
     * Valida que el request tenga la estructura correcta.
     * 
     * @param request Request a validar
     * @throws ApiException si la validación falla
     */
    public void validateRequest(AppointmentDurationRequest request) {
        if (request == null) {
            throw new ApiException("La configuración de duración no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        if (request.getDurationMinutes() == null) {
            throw new ApiException("La duración del turno es obligatoria", HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida que la duración sea compatible con todos los rangos horarios existentes.
     * La duración debe dividir cada rango horario (no debe haber residuo).
     * 
     * @param durationMinutes Duración propuesta en minutos
     * @param dailyHoursList  Lista de horarios diarios configurados
     * @throws ApiException si la duración no es compatible con algún rango
     */
    public void validateDurationCompatibility(Integer durationMinutes, List<DailyHours> dailyHoursList) {
        if (dailyHoursList == null || dailyHoursList.isEmpty()) {
            return; // No hay horarios para validar
        }

        for (DailyHours dailyHours : dailyHoursList) {
            if (dailyHours.getTimeRanges() == null || dailyHours.getTimeRanges().isEmpty()) {
                continue;
            }

            for (TimeRange timeRange : dailyHours.getTimeRanges()) {
                Integer rangeDurationMinutes = timeRange.getDurationMinutes();

                if (rangeDurationMinutes == null) {
                    // Rango inválido, debería haberse validado antes, pero lo ignoramos aquí
                    continue;
                }

                // Validar que la duración divida el rango horario
                if (rangeDurationMinutes % durationMinutes != 0) {
                    String dayName = DayNameUtils.getDayName(dailyHours.getDayOfWeek());
                    throw new ApiException(
                            String.format(
                                    "La duración de %d minutos no es compatible con los horarios configurados. " +
                                            "El rango del %s (%s - %s) tiene una duración de %d minutos, " +
                                            "que no es divisible por %d minutos. " +
                                            "Ejemplo: para un rango de %d minutos, las duraciones válidas son divisores de %d (ej: %s minutos).",
                                    durationMinutes,
                                    dayName,
                                    timeRange.getStart(),
                                    timeRange.getEnd(),
                                    rangeDurationMinutes,
                                    durationMinutes,
                                    rangeDurationMinutes,
                                    rangeDurationMinutes,
                                    getDivisorsExample(rangeDurationMinutes)),
                            HttpStatus.BAD_REQUEST);
                }
            }
        }

        logger.debug("Duración {} minutos validada correctamente contra todos los rangos horarios", durationMinutes);
    }

    /**
     * Obtiene ejemplos de divisores para mensajes de error más útiles.
     * 
     * @param number Número del cual obtener divisores comunes
     * @return String con ejemplos de divisores válidos
     */
    private String getDivisorsExample(Integer number) {
        // Buscar divisores comunes en el rango 15-240
        List<Integer> commonDivisors = new ArrayList<>();
        for (int i = 15; i <= Math.min(number, 240); i += 15) {
            if (number % i == 0) {
                commonDivisors.add(i);
            }
        }

        if (commonDivisors.isEmpty()) {
            return "ninguno (ajuste los horarios o la duración)";
        }

        // Mostrar máximo 3 ejemplos
        int maxExamples = Math.min(3, commonDivisors.size());
        return commonDivisors.subList(0, maxExamples).stream()
                .map(String::valueOf)
                .collect(Collectors.joining(", "));
    }
}
