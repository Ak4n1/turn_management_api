package com.ak4n1.turn_management.feature.configuration.dto.response;

import java.util.List;

/**
 * DTO de respuesta para horarios de un día.
 */
public class DailyHoursResponse {

    private Integer dayOfWeek;
    private List<TimeRangeResponse> timeRanges;

    public DailyHoursResponse() {
    }

    public DailyHoursResponse(Integer dayOfWeek, List<TimeRangeResponse> timeRanges) {
        this.dayOfWeek = dayOfWeek;
        this.timeRanges = timeRanges;
    }

    // Getters and Setters
    public Integer getDayOfWeek() {
        return dayOfWeek;
    }

    public void setDayOfWeek(Integer dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    public List<TimeRangeResponse> getTimeRanges() {
        return timeRanges;
    }

    public void setTimeRanges(List<TimeRangeResponse> timeRanges) {
        this.timeRanges = timeRanges;
    }
}

