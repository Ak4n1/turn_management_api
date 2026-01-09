package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * Rango horario (inicio y fin).
 * Formato: HH:mm (24 horas).
 * 
 * Ejemplo: 09:00 - 12:00
 */
@Embeddable
public class TimeRange {

    @Column(name = "start_time", nullable = false, length = 5)
    private String start; // Formato HH:mm

    @Column(name = "end_time", nullable = false, length = 5)
    private String end; // Formato HH:mm

    public TimeRange() {
    }

    public TimeRange(String start, String end) {
        this.start = start;
        this.end = end;
    }

    // Getters and Setters
    public String getStart() {
        return start;
    }

    public void setStart(String start) {
        this.start = start;
    }

    public String getEnd() {
        return end;
    }

    public void setEnd(String end) {
        this.end = end;
    }

    /**
     * Convierte el string de inicio a LocalTime.
     * 
     * @return LocalTime del inicio
     * @throws DateTimeParseException si el formato es inválido
     */
    public LocalTime getStartAsLocalTime() {
        if (start == null) {
            return null;
        }
        return LocalTime.parse(start, DateTimeFormatter.ofPattern("HH:mm"));
    }

    /**
     * Convierte el string de fin a LocalTime.
     * 
     * @return LocalTime del fin
     * @throws DateTimeParseException si el formato es inválido
     */
    public LocalTime getEndAsLocalTime() {
        if (end == null) {
            return null;
        }
        return LocalTime.parse(end, DateTimeFormatter.ofPattern("HH:mm"));
    }

    /**
     * Valida que el rango sea válido (start < end).
     * 
     * @return true si es válido, false en caso contrario
     */
    public boolean isValid() {
        if (start == null || end == null) {
            return false;
        }
        try {
            LocalTime startTime = getStartAsLocalTime();
            LocalTime endTime = getEndAsLocalTime();
            return startTime.isBefore(endTime);
        } catch (DateTimeParseException e) {
            return false;
        }
    }

    /**
     * Verifica si este rango se superpone con otro.
     * 
     * @param other Otro rango horario
     * @return true si hay superposición, false en caso contrario
     */
    public boolean overlaps(TimeRange other) {
        if (other == null || start == null || end == null || 
            other.start == null || other.end == null) {
            return false;
        }
        
        try {
            LocalTime thisStart = getStartAsLocalTime();
            LocalTime thisEnd = getEndAsLocalTime();
            LocalTime otherStart = other.getStartAsLocalTime();
            LocalTime otherEnd = other.getEndAsLocalTime();
            
            // Superposición: thisStart < otherEnd && thisEnd > otherStart
            return thisStart.isBefore(otherEnd) && thisEnd.isAfter(otherStart);
        } catch (DateTimeParseException e) {
            return false;
        }
    }

    /**
     * Obtiene la duración del rango en minutos.
     * 
     * @return Duración en minutos, o null si el rango es inválido
     */
    public Integer getDurationMinutes() {
        if (!isValid()) {
            return null;
        }
        try {
            LocalTime startTime = getStartAsLocalTime();
            LocalTime endTime = getEndAsLocalTime();
            return (int) java.time.Duration.between(startTime, endTime).toMinutes();
        } catch (Exception e) {
            return null;
        }
    }
}

