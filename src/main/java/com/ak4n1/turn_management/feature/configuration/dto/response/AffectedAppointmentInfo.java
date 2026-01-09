package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.time.LocalDate;
import java.time.LocalTime;

/**
 * DTO que representa información de un turno afectado por un cambio.
 * Por ahora solo contiene información básica ya que no existe el dominio Appointment.
 */
public class AffectedAppointmentInfo {

    /**
     * ID del turno (por ahora será null ya que no existe el dominio Appointment).
     */
    private Long id;

    /**
     * Fecha del turno.
     */
    private LocalDate date;

    /**
     * Hora de inicio del turno.
     */
    private LocalTime time;

    /**
     * NUEVO: ID del usuario del turno.
     */
    private Long userId;

    /**
     * NUEVO: Nombre del usuario.
     */
    private String userFirstName;

    /**
     * NUEVO: Apellido del usuario.
     */
    private String userLastName;

    /**
     * NUEVO: Email del usuario.
     */
    private String userEmail;

    public AffectedAppointmentInfo() {
    }

    public AffectedAppointmentInfo(Long id, LocalDate date, LocalTime time) {
        this.id = id;
        this.date = date;
        this.time = time;
    }

    /**
     * NUEVO: Constructor completo con información de usuario.
     */
    public AffectedAppointmentInfo(Long id, LocalDate date, LocalTime time, Long userId, 
                                   String userFirstName, String userLastName, String userEmail) {
        this.id = id;
        this.date = date;
        this.time = time;
        this.userId = userId;
        this.userFirstName = userFirstName;
        this.userLastName = userLastName;
        this.userEmail = userEmail;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public LocalTime getTime() {
        return time;
    }

    public void setTime(LocalTime time) {
        this.time = time;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
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

    public String getUserEmail() {
        return userEmail;
    }

    public void setUserEmail(String userEmail) {
        this.userEmail = userEmail;
    }
}

