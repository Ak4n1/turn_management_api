package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO de respuesta para bloqueos operativos.
 * Incluye la lista de turnos afectados.
 */
public class ManualBlockResponse {

    private Long id;
    private LocalDate blockDate;
    private Boolean isFullDay;
    private TimeRangeResponse timeRange;
    private String reason;
    private Boolean active;
    private Boolean affectsExistingAppointments;
    private Long createdByUserId;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    /**
     * Lista de turnos afectados por este bloqueo.
     * Por ahora siempre será una lista vacía ya que no existe el dominio Appointment.
     * Se calculará en el futuro cuando existan turnos.
     * 
     * Formato: Lista de objetos con información básica del turno afectado.
     * Ejemplo futuro:
     * [
     *   {
     *     "appointmentId": 123,
     *     "userId": 456,
     *     "date": "2024-04-25",
     *     "startTime": "10:00",
     *     "endTime": "10:30"
     *   }
     * ]
     */
    private List<AffectedAppointmentInfo> affectedAppointments;

    public ManualBlockResponse() {
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalDate getBlockDate() {
        return blockDate;
    }

    public void setBlockDate(LocalDate blockDate) {
        this.blockDate = blockDate;
    }

    public Boolean getIsFullDay() {
        return isFullDay;
    }

    public void setIsFullDay(Boolean isFullDay) {
        this.isFullDay = isFullDay;
    }

    public TimeRangeResponse getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRangeResponse timeRange) {
        this.timeRange = timeRange;
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

    public Boolean getAffectsExistingAppointments() {
        return affectsExistingAppointments;
    }

    public void setAffectsExistingAppointments(Boolean affectsExistingAppointments) {
        this.affectsExistingAppointments = affectsExistingAppointments;
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

    public List<AffectedAppointmentInfo> getAffectedAppointments() {
        return affectedAppointments;
    }

    public void setAffectedAppointments(List<AffectedAppointmentInfo> affectedAppointments) {
        this.affectedAppointments = affectedAppointments;
    }

    /**
     * Clase interna para representar información de turnos afectados.
     * Por ahora es solo una estructura de datos vacía.
     * Se completará cuando exista el dominio Appointment.
     */
    public static class AffectedAppointmentInfo {
        private Long appointmentId;
        private Long userId;
        private LocalDate date;
        private String startTime;
        private String endTime;

        public AffectedAppointmentInfo() {
        }

        public AffectedAppointmentInfo(Long appointmentId, Long userId, LocalDate date, 
                                      String startTime, String endTime) {
            this.appointmentId = appointmentId;
            this.userId = userId;
            this.date = date;
            this.startTime = startTime;
            this.endTime = endTime;
        }

        // Getters and Setters
        public Long getAppointmentId() {
            return appointmentId;
        }

        public void setAppointmentId(Long appointmentId) {
            this.appointmentId = appointmentId;
        }

        public Long getUserId() {
            return userId;
        }

        public void setUserId(Long userId) {
            this.userId = userId;
        }

        public LocalDate getDate() {
            return date;
        }

        public void setDate(LocalDate date) {
            this.date = date;
        }

        public String getStartTime() {
            return startTime;
        }

        public void setStartTime(String startTime) {
            this.startTime = startTime;
        }

        public String getEndTime() {
            return endTime;
        }

        public void setEndTime(String endTime) {
            this.endTime = endTime;
        }
    }
}

