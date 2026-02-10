package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.dto.request.CalendarExceptionRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarExceptionResponse;

import java.util.List;

/**
 * Servicio para gestión de excepciones de calendario por fecha específica.
 * Las excepciones permiten abrir/cerrar días puntuales o modificar sus horarios.
 */
public interface CalendarExceptionService {

    /**
     * Crea una nueva excepción de calendario por fecha específica.
     * 
     * Reglas de negocio:
     * - La fecha no puede ser pasada
     * - Si isOpen = true, debe tener timeRanges
     * - Si isOpen = false, no debe tener timeRanges
     * - No se permiten excepciones duplicadas para la misma fecha
     * - Se calcula el impacto (turnos afectados)
     * 
     * @param request DTO con los datos de la excepción
     * @param userId ID del usuario que crea la excepción (para auditoría)
     * @return Excepción creada con impacto calculado
     * @throws IllegalArgumentException si la excepción es inválida
     */
    CalendarExceptionResponse createException(CalendarExceptionRequest request, Long userId);

    /**
     * Obtiene todas las excepciones activas del calendario.
     * 
     * @return Lista de excepciones activas ordenadas por fecha
     */
    List<CalendarExceptionResponse> getAllActiveExceptions();

    /**
     * Actualiza una excepción existente.
     *
     * @param id      ID de la excepción
     * @param request DTO con los nuevos datos
     * @param userId  ID del usuario que realiza la actualización
     * @return Excepción actualizada
     */
    CalendarExceptionResponse updateException(Long id, CalendarExceptionRequest request, Long userId);

    /**
     * Desactiva (elimina lógicamente) una excepción.
     *
     * @param id ID de la excepción
     */
    void deactivateException(Long id);
}

