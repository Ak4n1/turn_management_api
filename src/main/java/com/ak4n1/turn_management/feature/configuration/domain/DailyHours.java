package com.ak4n1.turn_management.feature.configuration.domain;

import jakarta.persistence.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Horarios de un día específico.
 * Contiene una lista de rangos horarios para ese día.
 */
@Entity
@Table(name = "daily_hours", indexes = {
    @Index(name = "idx_calendar_config_day", columnList = "calendar_configuration_id, day_of_week")
})
public class DailyHours {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    /**
     * Configuración de calendario a la que pertenece.
     */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "calendar_configuration_id", nullable = false)
    private CalendarConfiguration calendarConfiguration;

    /**
     * Día de la semana: 1=Lunes, 2=Martes, ..., 7=Domingo
     */
    @Column(name = "day_of_week", nullable = false)
    private Integer dayOfWeek;

    /**
     * Lista de rangos horarios para este día.
     * Usamos @ElementCollection para almacenar una lista de objetos embebidos.
     */
    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(name = "daily_hours_time_ranges", 
                     joinColumns = @JoinColumn(name = "daily_hours_id"))
    @OrderColumn(name = "range_order")
    private List<TimeRange> timeRanges = new ArrayList<>();

    public DailyHours() {
    }

    public DailyHours(Integer dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    // Getters and Setters
    public Integer getDayOfWeek() {
        return dayOfWeek;
    }

    public void setDayOfWeek(Integer dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public CalendarConfiguration getCalendarConfiguration() {
        return calendarConfiguration;
    }

    public void setCalendarConfiguration(CalendarConfiguration calendarConfiguration) {
        this.calendarConfiguration = calendarConfiguration;
    }

    public List<TimeRange> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRange> timeRanges) {
        this.timeRanges = timeRanges != null ? timeRanges : new ArrayList<>();
    }

    /**
     * Agrega un rango horario a este día.
     */
    public void addTimeRange(TimeRange timeRange) {
        if (timeRanges == null) {
            timeRanges = new ArrayList<>();
        }
        timeRanges.add(timeRange);
    }

    /**
     * Valida que no haya superposiciones entre los rangos de este día.
     * 
     * @return true si no hay superposiciones, false en caso contrario
     */
    public boolean hasNoOverlaps() {
        if (timeRanges == null || timeRanges.size() <= 1) {
            return true;
        }
        
        for (int i = 0; i < timeRanges.size(); i++) {
            for (int j = i + 1; j < timeRanges.size(); j++) {
                if (timeRanges.get(i).overlaps(timeRanges.get(j))) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Valida que todos los rangos sean válidos (start < end).
     * 
     * @return true si todos son válidos, false en caso contrario
     */
    public boolean allRangesValid() {
        if (timeRanges == null || timeRanges.isEmpty()) {
            return true; // Día sin horarios es válido
        }
        return timeRanges.stream().allMatch(TimeRange::isValid);
    }
}

