package com.ak4n1.turn_management.shared.metrics.dto;

import java.util.List;

/**
 * DTO de respuesta para métricas del sistema.
 * 
 * Implementa US-T022.
 */
public class SystemMetricsResponse {

    private PeriodResponse period;
    private AppointmentsMetricsResponse appointments;
    private SlotsMetricsResponse slots;
    private List<TopDayResponse> topDays;

    public SystemMetricsResponse() {
    }

    public PeriodResponse getPeriod() {
        return period;
    }

    public void setPeriod(PeriodResponse period) {
        this.period = period;
    }

    public AppointmentsMetricsResponse getAppointments() {
        return appointments;
    }

    public void setAppointments(AppointmentsMetricsResponse appointments) {
        this.appointments = appointments;
    }

    public SlotsMetricsResponse getSlots() {
        return slots;
    }

    public void setSlots(SlotsMetricsResponse slots) {
        this.slots = slots;
    }

    public List<TopDayResponse> getTopDays() {
        return topDays;
    }

    public void setTopDays(List<TopDayResponse> topDays) {
        this.topDays = topDays;
    }

    // Clases anidadas
    public static class PeriodResponse {
        private String start;
        private String end;

        public PeriodResponse() {
        }

        public PeriodResponse(String start, String end) {
            this.start = start;
            this.end = end;
        }

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

    public static class AppointmentsMetricsResponse {
        private long created;
        private long confirmed;
        private long cancelled;
        private long noShow;
        private double cancellationRate;

        public AppointmentsMetricsResponse() {
        }

        public long getCreated() {
            return created;
        }

        public void setCreated(long created) {
            this.created = created;
        }

        public long getConfirmed() {
            return confirmed;
        }

        public void setConfirmed(long confirmed) {
            this.confirmed = confirmed;
        }

        public long getCancelled() {
            return cancelled;
        }

        public void setCancelled(long cancelled) {
            this.cancelled = cancelled;
        }

        public long getNoShow() {
            return noShow;
        }

        public void setNoShow(long noShow) {
            this.noShow = noShow;
        }

        public double getCancellationRate() {
            return cancellationRate;
        }

        public void setCancellationRate(double cancellationRate) {
            this.cancellationRate = cancellationRate;
        }
    }

    public static class SlotsMetricsResponse {
        private long offered;
        private long used;
        private double usageRate;

        public SlotsMetricsResponse() {
        }

        public long getOffered() {
            return offered;
        }

        public void setOffered(long offered) {
            this.offered = offered;
        }

        public long getUsed() {
            return used;
        }

        public void setUsed(long used) {
            this.used = used;
        }

        public double getUsageRate() {
            return usageRate;
        }

        public void setUsageRate(double usageRate) {
            this.usageRate = usageRate;
        }
    }

    public static class TopDayResponse {
        private String date;
        private long appointments;

        public TopDayResponse() {
        }

        public TopDayResponse(String date, long appointments) {
            this.date = date;
            this.appointments = appointments;
        }

        public String getDate() {
            return date;
        }

        public void setDate(String date) {
            this.date = date;
        }

        public long getAppointments() {
            return appointments;
        }

        public void setAppointments(long appointments) {
            this.appointments = appointments;
        }
    }
}

