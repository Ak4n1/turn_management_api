package com.ak4n1.turn_management.feature.configuration.service.cancellation;

import com.ak4n1.turn_management.feature.appointment.domain.Appointment;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentHistory;
import com.ak4n1.turn_management.feature.appointment.domain.AppointmentState;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentHistoryRepository;
import com.ak4n1.turn_management.feature.appointment.repository.AppointmentRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
public class AppointmentCancellationServiceImpl implements AppointmentCancellationService {

    private static final Logger logger = LoggerFactory.getLogger(AppointmentCancellationServiceImpl.class);

    private final AppointmentRepository appointmentRepository;
    private final AppointmentHistoryRepository appointmentHistoryRepository;

    public AppointmentCancellationServiceImpl(
            AppointmentRepository appointmentRepository,
            AppointmentHistoryRepository appointmentHistoryRepository) {
        this.appointmentRepository = appointmentRepository;
        this.appointmentHistoryRepository = appointmentHistoryRepository;
    }

    @Override
    @Transactional
    public void cancelAffectedAppointmentsByDayClosure(
            List<Appointment> appointments,
            Long adminUserId,
            String reason) {
        logger.info("Iniciando cancelación masiva de {} turno(s) afectados por cierre de días - Admin: {}, Razón: {}",
                appointments.size(), adminUserId, reason);

        // Filtrar turnos válidos para cancelar
        List<Appointment> appointmentsToCancel = new ArrayList<>();
        List<AppointmentHistory> historiesToSave = new ArrayList<>();

        for (Appointment appointment : appointments) {
            try {
                // Validar que el turno no esté ya cancelado o completado
                if (appointment.getState() == AppointmentState.CANCELLED ||
                        appointment.getState() == AppointmentState.CANCELLED_BY_ADMIN ||
                        appointment.getState() == AppointmentState.COMPLETED ||
                        appointment.getState() == AppointmentState.EXPIRED) {
                    logger.debug("Turno ID {} omitido - Estado actual: {}", appointment.getId(),
                            appointment.getState());
                    continue;
                }

                // Guardar estado anterior
                AppointmentState previousState = appointment.getState();

                // Cancelar el turno por admin
                appointment.cancelByAdmin();
                appointmentsToCancel.add(appointment);

                // Preparar historial para batch save
                AppointmentHistory history = new AppointmentHistory(
                        appointment.getId(),
                        adminUserId,
                        previousState,
                        AppointmentState.CANCELLED_BY_ADMIN,
                        "CANCELLED_BY_ADMIN",
                        reason,
                        null // clientIp no disponible en este contexto
                );
                historiesToSave.add(history);

                logger.debug(
                        "Turno preparado para cancelación masiva - ID: {}, Usuario: {}, Estado anterior: {}, Razón: {}",
                        appointment.getId(), appointment.getUserId(), previousState, reason);

            } catch (Exception e) {
                // NO lanzar excepción - un error en un turno no debe bloquear la cancelación de
                // los demás
                logger.error("Error al preparar turno para cancelación masiva - Turno ID: {}, Error: {}",
                        appointment.getId(), e.getMessage(), e);
            }
        }

        // Batch save de todos los turnos cancelados (MUCHO MÁS RÁPIDO)
        if (!appointmentsToCancel.isEmpty()) {
            try {
                appointmentRepository.saveAll(appointmentsToCancel);
                logger.info("Batch save de {} turno(s) cancelado(s) completado", appointmentsToCancel.size());
            } catch (Exception e) {
                logger.error("Error en batch save de turnos cancelados: {}", e.getMessage(), e);
                throw e; // Re-lanzar porque esto es crítico
            }
        }

        // Batch save de todos los historiales (MUCHO MÁS RÁPIDO)
        if (!historiesToSave.isEmpty()) {
            try {
                appointmentHistoryRepository.saveAll(historiesToSave);
                logger.info("Batch save de {} historial(es) de cancelación completado", historiesToSave.size());
            } catch (Exception e) {
                logger.error("Error en batch save de historiales: {}", e.getMessage(), e);
                // No re-lanzar porque los turnos ya están cancelados, solo falta el historial
            }
        }

        logger.info("Cancelación masiva completada - {} turno(s) cancelado(s) en batch", appointmentsToCancel.size());
    }
}
