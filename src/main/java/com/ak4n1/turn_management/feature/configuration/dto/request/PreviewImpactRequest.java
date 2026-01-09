package com.ak4n1.turn_management.feature.configuration.dto.request;

import jakarta.validation.constraints.NotNull;

import java.time.LocalDate;

/**
 * DTO para previsualizar el impacto de cambios propuestos en la configuración del calendario.
 * Permite calcular el impacto sin aplicar los cambios.
 */
public class PreviewImpactRequest {

    /**
     * Tipo de cambio propuesto.
     * - WEEKLY_CONFIG: Cambio en la configuración semanal
     * - DAILY_HOURS: Cambio en horarios diarios
     * - APPOINTMENT_DURATION: Cambio en la duración de turnos
     * - EXCEPTION: Nueva excepción
     * - BLOCK: Nuevo bloqueo
     */
    @NotNull(message = "El tipo de cambio es obligatorio")
    private String changeType; // WEEKLY_CONFIG, DAILY_HOURS, APPOINTMENT_DURATION, EXCEPTION, BLOCK

    /**
     * Rango de fechas para evaluar el impacto (opcional).
     * Si no se especifica, se evalúa para los próximos 90 días.
     */
    private LocalDate startDate;
    private LocalDate endDate;

    /**
     * Configuración semanal propuesta (solo si changeType = WEEKLY_CONFIG).
     */
    private WeeklyConfigRequest weeklyConfig;

    /**
     * Horarios diarios propuestos (solo si changeType = DAILY_HOURS).
     */
    private DailyHoursConfigRequest dailyHours;

    /**
     * Duración de turnos propuesta en minutos (solo si changeType = APPOINTMENT_DURATION).
     */
    private Integer appointmentDurationMinutes;

    /**
     * Excepción propuesta (solo si changeType = EXCEPTION).
     */
    private CalendarExceptionRequest exception;

    /**
     * Bloqueo propuesto (solo si changeType = BLOCK).
     */
    private ManualBlockRequest block;

    public PreviewImpactRequest() {
    }

    // Getters and Setters
    public String getChangeType() {
        return changeType;
    }

    public void setChangeType(String changeType) {
        this.changeType = changeType;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public WeeklyConfigRequest getWeeklyConfig() {
        return weeklyConfig;
    }

    public void setWeeklyConfig(WeeklyConfigRequest weeklyConfig) {
        this.weeklyConfig = weeklyConfig;
    }

    public DailyHoursConfigRequest getDailyHours() {
        return dailyHours;
    }

    public void setDailyHours(DailyHoursConfigRequest dailyHours) {
        this.dailyHours = dailyHours;
    }

    public Integer getAppointmentDurationMinutes() {
        return appointmentDurationMinutes;
    }

    public void setAppointmentDurationMinutes(Integer appointmentDurationMinutes) {
        this.appointmentDurationMinutes = appointmentDurationMinutes;
    }

    public CalendarExceptionRequest getException() {
        return exception;
    }

    public void setException(CalendarExceptionRequest exception) {
        this.exception = exception;
    }

    public ManualBlockRequest getBlock() {
        return block;
    }

    public void setBlock(ManualBlockRequest block) {
        this.block = block;
    }
}

