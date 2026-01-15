package com.ak4n1.turn_management.feature.configuration.service.slots;

import com.ak4n1.turn_management.feature.configuration.domain.ManualBlock;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.TimeRangeResponse;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

/**
 * Servicio para generar slots disponibles de turnos.
 */
public interface SlotGenerationService {

    /**
     * Obtiene los slots disponibles para una fecha específica.
     * 
     * @param date Fecha para la cual generar slots
     * @return Respuesta con slots disponibles
     */
    SlotsResponse getAvailableSlots(LocalDate date);

    /**
     * Genera slots desde un rango horario según la duración configurada.
     * Excluye slots que están bloqueados.
     * 
     * @param timeRange Rango horario
     * @param appointmentDurationMinutes Duración de cada turno en minutos
     * @param blocks Lista de bloqueos operativos
     * @return Lista de slots generados
     */
    List<SlotResponse> generateSlotsFromRange(TimeRangeResponse timeRange,
                                              Integer appointmentDurationMinutes,
                                              List<ManualBlock> blocks);

    /**
     * Excluye slots ocupados por turnos existentes.
     * Marca como no disponibles los slots que tienen turnos en estados activos.
     * 
     * @param slots Lista de slots a filtrar
     * @param date Fecha de los turnos
     * @return Lista de slots con disponibilidad actualizada
     */
    List<SlotResponse> excludeOccupiedSlots(List<SlotResponse> slots, LocalDate date);

    /**
     * Verifica si un slot está bloqueado por algún bloqueo operativo.
     * 
     * @param slotStart Hora de inicio del slot
     * @param slotEnd Hora de fin del slot
     * @param blocks Lista de bloqueos operativos
     * @return true si el slot está bloqueado, false en caso contrario
     */
    boolean isSlotBlocked(LocalTime slotStart, LocalTime slotEnd, List<ManualBlock> blocks);
}
