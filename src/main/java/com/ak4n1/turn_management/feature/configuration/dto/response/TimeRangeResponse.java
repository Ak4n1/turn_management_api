package com.ak4n1.turn_management.feature.configuration.dto.response;

/**
 * DTO de respuesta para rango horario.
 */
public class TimeRangeResponse {

    private String start;
    private String end;

    public TimeRangeResponse() {
    }

    public TimeRangeResponse(String start, String end) {
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
}

