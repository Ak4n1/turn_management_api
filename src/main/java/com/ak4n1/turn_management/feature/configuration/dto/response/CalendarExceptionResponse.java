package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO de respuesta para excepciones de calendario.
 * Incluye el impacto calculado (turnos afectados).
 */
public class CalendarExceptionResponse {

    private Long id;
    private LocalDate exceptionDate;
    private Boolean isOpen;
    private List<TimeRangeResponse> timeRanges;
    private String reason;
    private Boolean active;
    private Long createdByUserId;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    /**
     * Impacto calculado: cantidad de turnos afectados por esta excepción.
     * Por ahora siempre será 0 ya que no existe aún el dominio Appointment.
     * Se calculará en el futuro cuando existan turnos.
     */
    private Integer affectedAppointmentsCount;

    public CalendarExceptionResponse() {
    }

    public CalendarExceptionResponse(Long id, LocalDate exceptionDate, Boolean isOpen, 
                                     List<TimeRangeResponse> timeRanges, String reason, 
                                     Boolean active, Long createdByUserId, 
                                     LocalDateTime createdAt, LocalDateTime updatedAt,
                                     Integer affectedAppointmentsCount) {
        this.id = id;
        this.exceptionDate = exceptionDate;
        this.isOpen = isOpen;
        this.timeRanges = timeRanges;
        this.reason = reason;
        this.active = active;
        this.createdByUserId = createdByUserId;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.affectedAppointmentsCount = affectedAppointmentsCount;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalDate getExceptionDate() {
        return exceptionDate;
    }

    public void setExceptionDate(LocalDate exceptionDate) {
        this.exceptionDate = exceptionDate;
    }

    public Boolean getIsOpen() {
        return isOpen;
    }

    public void setIsOpen(Boolean isOpen) {
        this.isOpen = isOpen;
    }

    public List<TimeRangeResponse> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRangeResponse> timeRanges) {
        this.timeRanges = timeRanges;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }

    public Long getCreatedByUserId() {
        return createdByUserId;
    }

    public void setCreatedByUserId(Long createdByUserId) {
        this.createdByUserId = createdByUserId;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    public Integer getAffectedAppointmentsCount() {
        return affectedAppointmentsCount;
    }

    public void setAffectedAppointmentsCount(Integer affectedAppointmentsCount) {
        this.affectedAppointmentsCount = affectedAppointmentsCount;
    }
}

