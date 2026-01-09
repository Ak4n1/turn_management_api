package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.configuration.dto.request.AppointmentDurationRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.DailyHoursConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.WeeklyConfigRequest;
import com.ak4n1.turn_management.feature.configuration.dto.request.PreviewImpactRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityRangeResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.AvailabilityResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.CalendarConfigurationResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.ConsolidatedCalendarResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.PreviewImpactResponse;
import com.ak4n1.turn_management.feature.configuration.dto.response.SlotsResponse;

import java.time.LocalDate;

/**
 * Servicio para gestión de configuraciones de calendario.
 * Implementa la lógica de negocio relacionada con el calendario semanal base y horarios diarios.
 */
public interface CalendarConfigurationService {

    /**
     * Crea una nueva configuración semanal base.
     * 
     * Reglas de negocio:
     * - Crea una nueva versión (incrementa automáticamente)
     * - Desactiva la configuración anterior (si existe)
     * - Activa la nueva configuración
     * - Valida que no haya solapamientos lógicos
     * 
     * @param request DTO con la configuración semanal
     * @param userId ID del usuario que crea la configuración (para auditoría)
     * @return Configuración creada
     * @throws IllegalArgumentException si la configuración es inválida
     */
    CalendarConfigurationResponse createWeeklyConfig(WeeklyConfigRequest request, Long userId);

    /**
     * Configura horarios diarios para la configuración activa.
     * 
     * Reglas de negocio:
     * - Solo puede configurarse si existe configuración semanal activa
     * - Solo días abiertos pueden tener horarios
     * - Los rangos no pueden superponerse
     * - Crea una nueva versión de la configuración
     * - Desactiva la configuración anterior
     * 
     * @param request DTO con los horarios diarios
     * @param userId ID del usuario que crea la configuración (para auditoría)
     * @return Configuración actualizada
     * @throws IllegalArgumentException si la configuración es inválida
     */
    CalendarConfigurationResponse configureDailyHours(DailyHoursConfigRequest request, Long userId);

    /**
     * Configura la duración de los turnos para la configuración activa.
     * 
     * Reglas de negocio:
     * - Solo puede configurarse si existe configuración activa con horarios
     * - Duración debe estar entre 15 y 240 minutos
     * - Duración debe ser divisible por 15
     * - Duración debe ser compatible con los rangos horarios (debe dividir cada rango)
     * - Crea una nueva versión de la configuración
     * - Desactiva la configuración anterior
     * 
     * @param request DTO con la duración en minutos
     * @param userId ID del usuario que crea la configuración (para auditoría)
     * @return Configuración actualizada
     * @throws IllegalArgumentException si la configuración es inválida
     */
    CalendarConfigurationResponse configureAppointmentDuration(AppointmentDurationRequest request, Long userId);

    /**
     * Obtiene la configuración activa actual.
     * 
     * @return Configuración activa, o null si no existe
     */
    CalendarConfigurationResponse getActiveConfiguration();

    /**
     * Obtiene el calendario consolidado para un rango de fechas.
     * 
     * Evalúa cada día aplicando el orden de precedencia:
     * 1. Bloqueos operativos (prioridad máxima)
     * 2. Excepciones por fecha
     * 3. Configuración base del calendario semanal
     * 
     * Para cada día retorna:
     * - Estado (OPEN, CLOSED, PARTIAL)
     * - Tipo de regla aplicada (BLOCK, EXCEPTION, BASE)
     * - Descripción explicativa
     * - Rangos horarios disponibles (si aplica)
     * - Indicador de turnos existentes
     * 
     * @param startDate Fecha de inicio del rango
     * @param endDate Fecha de fin del rango
     * @return Calendario consolidado con información de cada día
     * @throws IllegalArgumentException si el rango de fechas es inválido
     */
    ConsolidatedCalendarResponse getConsolidatedCalendar(LocalDate startDate, LocalDate endDate);

    /**
     * Previsualiza el impacto de cambios propuestos sin aplicarlos.
     * 
     * Calcula:
     * - Días afectados (días que cambiarían su disponibilidad)
     * - Slots que desaparecerían (comparando slots actuales vs propuestos)
     * - Turnos existentes impactados (por ahora siempre 0 ya que no existe Appointment)
     * 
     * Los cambios propuestos pueden ser:
     * - WEEKLY_CONFIG: Cambio en configuración semanal
     * - DAILY_HOURS: Cambio en horarios diarios
     * - APPOINTMENT_DURATION: Cambio en duración de turnos
     * - EXCEPTION: Nueva excepción
     * - BLOCK: Nuevo bloqueo
     * 
     * @param request DTO con los cambios propuestos y rango de fechas a evaluar
     * @return Información detallada del impacto calculado
     * @throws IllegalArgumentException si los cambios propuestos son inválidos
     */
    PreviewImpactResponse previewImpact(PreviewImpactRequest request);

    /**
     * Evalúa la disponibilidad de una fecha específica aplicando el orden de precedencia.
     * 
     * Orden de precedencia (prioridad de mayor a menor):
     * 1. Bloqueos operativos - Si hay un bloqueo, la fecha NO está disponible
     * 2. Excepciones por fecha - Si hay una excepción, se usa esa configuración
     * 3. Configuración base - Si no hay bloqueo ni excepción, se usa la configuración semanal base
     * 
     * Validaciones:
     * - La fecha no puede ser pasada (se compara con fecha actual en GMT-3)
     * - Debe existir una configuración activa
     * 
     * @param date Fecha a evaluar
     * @return Información de disponibilidad con regla aplicada y rangos horarios
     * @throws IllegalArgumentException si la fecha es inválida o es pasada
     */
    AvailabilityResponse checkAvailability(LocalDate date);

    /**
     * Genera los slots disponibles (intervalos de tiempo) para una fecha específica.
     * 
     * Los slots se generan según:
     * - Los rangos horarios disponibles para la fecha (aplicando precedencia)
     * - La duración de turnos configurada
     * 
     * Se excluyen:
     * - Slots que están fuera de los rangos horarios
     * - Slots que están bloqueados (por bloqueos operativos)
     * - Slots que están ocupados (por ahora siempre disponibles ya que no existe Appointment)
     * 
     * Validaciones:
     * - La fecha debe estar disponible (no cerrada)
     * - Debe existir duración de turnos configurada
     * - La fecha no puede ser pasada
     * 
     * @param date Fecha para la cual generar slots
     * @return Lista de slots disponibles con información detallada
     * @throws IllegalArgumentException si la fecha no está disponible o no hay duración configurada
     */
    SlotsResponse getAvailableSlots(LocalDate date);

    /**
     * Obtiene la disponibilidad de un rango de fechas.
     * 
     * Evalúa cada día en el rango y retorna:
     * - Estado de disponibilidad (FULL, PARTIAL, CLOSED)
     * - Cantidad de slots disponibles
     * - Cantidad total de slots
     * 
     * Estados:
     * - FULL: Todos los slots están disponibles (availableSlots == totalSlots)
     * - PARTIAL: Algunos slots están disponibles (0 < availableSlots < totalSlots)
     * - CLOSED: No hay slots disponibles (totalSlots == 0 o availableSlots == 0)
     * 
     * Validaciones:
     * - Rango máximo: 90 días
     * - Fechas válidas (startDate <= endDate)
     * - Fechas no pasadas (startDate >= hoy)
     * - Debe existir configuración activa con duración configurada
     * 
     * @param startDate Fecha de inicio del rango
     * @param endDate Fecha de fin del rango
     * @return Disponibilidad por día en el rango especificado
     * @throws IllegalArgumentException si el rango es inválido o excede el máximo permitido
     */
    AvailabilityRangeResponse getAvailabilityRange(LocalDate startDate, LocalDate endDate);

    /**
     * Obtiene el historial completo de cambios en la configuración del calendario.
     * 
     * Implementa US-T018.1:
     * - Solo accesible por admin
     * - Muestra todas las versiones de configuración
     * - Incluye quién hizo cada cambio y cuándo
     * - Detecta qué cambió en cada versión
     * - Cuenta turnos asociados a cada versión
     * - Versiones ordenadas por fecha descendente
     * 
     * @return Historial completo de configuraciones
     */
    com.ak4n1.turn_management.feature.configuration.dto.response.ConfigurationHistoryResponse getConfigurationHistory();
}

