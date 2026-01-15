package com.ak4n1.turn_management.feature.configuration.service.validation;

import com.ak4n1.turn_management.feature.configuration.domain.WeeklyConfig;
import com.ak4n1.turn_management.feature.configuration.dto.request.WeeklyConfigRequest;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

/**
 * Validador para configuraciones semanales.
 * Valida tanto el request como la entidad de dominio WeeklyConfig.
 */
@Component
public class WeeklyConfigValidator implements ConfigurationValidator<WeeklyConfigRequest> {

    private static final Logger logger = LoggerFactory.getLogger(WeeklyConfigValidator.class);

    @Override
    public void validate(WeeklyConfigRequest request) {
        validateRequest(request);
    }

    /**
     * Valida que el request tenga todos los campos requeridos.
     * Las validaciones de @NotNull en el DTO ya se ejecutan antes,
     * pero esta validación adicional asegura que los valores no sean null.
     * 
     * @param request Request a validar
     * @throws ApiException si la validación falla
     */
    public void validateRequest(WeeklyConfigRequest request) {
        if (request == null) {
            throw new ApiException("La configuración no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        // Validar que todos los días estén presentes (no null)
        if (request.getMonday() == null || request.getTuesday() == null ||
                request.getWednesday() == null || request.getThursday() == null ||
                request.getFriday() == null || request.getSaturday() == null ||
                request.getSunday() == null) {
            throw new ApiException("Todos los días de la semana deben estar definidos (true o false)",
                    HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Valida la lógica de negocio de la configuración semanal.
     * 
     * Reglas:
     * - Al menos un día debe estar abierto (no tiene sentido tener todos los días cerrados)
     * - No hay solapamientos lógicos (esto se verifica en el futuro cuando haya horarios)
     * 
     * @param weeklyConfig Configuración semanal a validar
     * @throws ApiException si la validación falla
     */
    public void validateDomain(WeeklyConfig weeklyConfig) {
        if (weeklyConfig == null) {
            throw new ApiException("La configuración semanal no puede ser nula", HttpStatus.BAD_REQUEST);
        }

        // Validar que al menos un día esté abierto
        int openDays = weeklyConfig.countOpenDays();
        if (openDays == 0) {
            throw new ApiException(
                    "Debe haber al menos un día abierto en la semana. No se puede configurar todos los días como cerrados.",
                    HttpStatus.BAD_REQUEST);
        }

        logger.debug("Configuración semanal validada - Días abiertos: {}", openDays);
    }
}
