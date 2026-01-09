package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/**
 * Configuración semanal base.
 * Define qué días de la semana están abiertos o cerrados por defecto.
 * Esta es la base sobre la cual se aplican excepciones y bloqueos.
 */
@Embeddable
public class WeeklyConfig {

    @Column(nullable = false)
    private Boolean monday = false;

    @Column(nullable = false)
    private Boolean tuesday = false;

    @Column(nullable = false)
    private Boolean wednesday = false;

    @Column(nullable = false)
    private Boolean thursday = false;

    @Column(nullable = false)
    private Boolean friday = false;

    @Column(nullable = false)
    private Boolean saturday = false;

    @Column(nullable = false)
    private Boolean sunday = false;

    public WeeklyConfig() {
    }

    public WeeklyConfig(Boolean monday, Boolean tuesday, Boolean wednesday, 
                       Boolean thursday, Boolean friday, Boolean saturday, Boolean sunday) {
        this.monday = monday;
        this.tuesday = tuesday;
        this.wednesday = wednesday;
        this.thursday = thursday;
        this.friday = friday;
        this.saturday = saturday;
        this.sunday = sunday;
    }

    // Getters and Setters
    public Boolean getMonday() {
        return monday;
    }

    public void setMonday(Boolean monday) {
        this.monday = monday;
    }

    public Boolean getTuesday() {
        return tuesday;
    }

    public void setTuesday(Boolean tuesday) {
        this.tuesday = tuesday;
    }

    public Boolean getWednesday() {
        return wednesday;
    }

    public void setWednesday(Boolean wednesday) {
        this.wednesday = wednesday;
    }

    public Boolean getThursday() {
        return thursday;
    }

    public void setThursday(Boolean thursday) {
        this.thursday = thursday;
    }

    public Boolean getFriday() {
        return friday;
    }

    public void setFriday(Boolean friday) {
        this.friday = friday;
    }

    public Boolean getSaturday() {
        return saturday;
    }

    public void setSaturday(Boolean saturday) {
        this.saturday = saturday;
    }

    public Boolean getSunday() {
        return sunday;
    }

    public void setSunday(Boolean sunday) {
        this.sunday = sunday;
    }

    /**
     * Verifica si un día específico está abierto.
     * 
     * @param dayOfWeek 1=Lunes, 2=Martes, ..., 7=Domingo (según DayOfWeek de Java)
     * @return true si el día está abierto, false en caso contrario
     */
    public Boolean isDayOpen(int dayOfWeek) {
        return switch (dayOfWeek) {
            case 1 -> monday;
            case 2 -> tuesday;
            case 3 -> wednesday;
            case 4 -> thursday;
            case 5 -> friday;
            case 6 -> saturday;
            case 7 -> sunday;
            default -> false;
        };
    }

    /**
     * Cuenta cuántos días están abiertos en la semana.
     */
    public int countOpenDays() {
        int count = 0;
        if (monday) count++;
        if (tuesday) count++;
        if (wednesday) count++;
        if (thursday) count++;
        if (friday) count++;
        if (saturday) count++;
        if (sunday) count++;
        return count;
    }
}

