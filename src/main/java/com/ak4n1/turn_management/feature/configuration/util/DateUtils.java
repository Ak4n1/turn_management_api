package com.ak4n1.turn_management.feature.configuration.util;

import com.ak4n1.turn_management.shared.exception.ApiException;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;

/**
 * Utilidades para manipulación de fechas en el contexto de configuración del calendario.
 */
public class DateUtils {

    private DateUtils() {
        // Clase de utilidad - no debe ser instanciada
        throw new UnsupportedOperationException("Clase de utilidad - no debe ser instanciada");
    }

    /**
     * Obtiene la fecha actual en la zona horaria GMT-3 (America/Argentina/Buenos_Aires).
     * 
     * @return Fecha actual en GMT-3
     */
    public static LocalDate getTodayGMT3() {
        ZoneId gmtMinus3 = ZoneId.of("America/Argentina/Buenos_Aires");
        return ZonedDateTime.now(gmtMinus3).toLocalDate();
    }

    /**
     * Valida que el rango de fechas sea válido.
     * - Las fechas no pueden ser nulas
     * - La fecha de inicio no puede ser posterior a la fecha de fin
     * - El rango no puede exceder 90 días
     * 
     * @param startDate Fecha de inicio
     * @param endDate Fecha de fin
     * @throws ApiException Si el rango de fechas es inválido
     */
    public static void validateDateRange(LocalDate startDate, LocalDate endDate) {
        if (startDate == null || endDate == null) {
            throw new ApiException("Las fechas de inicio y fin son obligatorias", HttpStatus.BAD_REQUEST);
        }

        if (startDate.isAfter(endDate)) {
            throw new ApiException(
                    "La fecha de inicio no puede ser posterior a la fecha de fin",
                    HttpStatus.BAD_REQUEST);
        }

        long daysBetween = ChronoUnit.DAYS.between(startDate, endDate) + 1;
        if (daysBetween > 90) {
            throw new ApiException(
                    "El rango de fechas no puede exceder 90 días. Rango solicitado: " + daysBetween + " días",
                    HttpStatus.BAD_REQUEST);
        }
    }
}
