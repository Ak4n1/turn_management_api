package com.ak4n1.turn_management.feature.appointment.dto.response;

/**
 * DTO de respuesta para un turno en la vista de administrador.
 * Incluye información del usuario que reservó el turno.
 * 
 * Implementa US-T017.
 */
public class AdminAppointmentResponse {
    
    private Long id;
    private Long userId;
    private String userEmail;
    private String userFirstName;
    private String userLastName;
    private String date;
    private String startTime;
    private String endTime;
    private Integer durationMinutes;
    private String state;
    private String expiresAt;
    private String confirmedAt;
    private String cancelledAt;
    private String cancellationReason;
    private Long previousAppointmentId;
    /** ID del turno siguiente si este turno fue reprogramado (estado RESCHEDULED). */
    private Long nextAppointmentId;
    /** Fecha del nuevo turno cuando este fue reprogramado (formato YYYY-MM-DD). */
    private String nextAppointmentDate;
    /** Hora de inicio del nuevo turno (formato HH:mm). */
    private String nextAppointmentStartTime;
    /** Hora de fin del nuevo turno (formato HH:mm). */
    private String nextAppointmentEndTime;
    private Integer calendarConfigVersion;
    private String createdAt;
    private String updatedAt;

    public AdminAppointmentResponse() {
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getUserEmail() {
        return userEmail;
    }

    public void setUserEmail(String userEmail) {
        this.userEmail = userEmail;
    }

    public String getUserFirstName() {
        return userFirstName;
    }

    public void setUserFirstName(String userFirstName) {
        this.userFirstName = userFirstName;
    }

    public String getUserLastName() {
        return userLastName;
    }

    public void setUserLastName(String userLastName) {
        this.userLastName = userLastName;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
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

    public Integer getDurationMinutes() {
        return durationMinutes;
    }

    public void setDurationMinutes(Integer durationMinutes) {
        this.durationMinutes = durationMinutes;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getExpiresAt() {
        return expiresAt;
    }

    public void setExpiresAt(String expiresAt) {
        this.expiresAt = expiresAt;
    }

    public String getConfirmedAt() {
        return confirmedAt;
    }

    public void setConfirmedAt(String confirmedAt) {
        this.confirmedAt = confirmedAt;
    }

    public String getCancelledAt() {
        return cancelledAt;
    }

    public void setCancelledAt(String cancelledAt) {
        this.cancelledAt = cancelledAt;
    }

    public String getCancellationReason() {
        return cancellationReason;
    }

    public void setCancellationReason(String cancellationReason) {
        this.cancellationReason = cancellationReason;
    }

    public Long getPreviousAppointmentId() {
        return previousAppointmentId;
    }

    public void setPreviousAppointmentId(Long previousAppointmentId) {
        this.previousAppointmentId = previousAppointmentId;
    }

    public Long getNextAppointmentId() {
        return nextAppointmentId;
    }

    public void setNextAppointmentId(Long nextAppointmentId) {
        this.nextAppointmentId = nextAppointmentId;
    }

    public String getNextAppointmentDate() {
        return nextAppointmentDate;
    }

    public void setNextAppointmentDate(String nextAppointmentDate) {
        this.nextAppointmentDate = nextAppointmentDate;
    }

    public String getNextAppointmentStartTime() {
        return nextAppointmentStartTime;
    }

    public void setNextAppointmentStartTime(String nextAppointmentStartTime) {
        this.nextAppointmentStartTime = nextAppointmentStartTime;
    }

    public String getNextAppointmentEndTime() {
        return nextAppointmentEndTime;
    }

    public void setNextAppointmentEndTime(String nextAppointmentEndTime) {
        this.nextAppointmentEndTime = nextAppointmentEndTime;
    }

    public Integer getCalendarConfigVersion() {
        return calendarConfigVersion;
    }

    public void setCalendarConfigVersion(Integer calendarConfigVersion) {
        this.calendarConfigVersion = calendarConfigVersion;
    }

    public String getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(String createdAt) {
        this.createdAt = createdAt;
    }

    public String getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(String updatedAt) {
        this.updatedAt = updatedAt;
    }
}

