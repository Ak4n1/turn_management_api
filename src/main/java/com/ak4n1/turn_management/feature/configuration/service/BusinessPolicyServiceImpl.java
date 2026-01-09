package com.ak4n1.turn_management.feature.configuration.service;

import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.service.UserService;
import com.ak4n1.turn_management.feature.configuration.domain.BusinessPolicy;
import com.ak4n1.turn_management.feature.configuration.dto.request.BusinessPolicyRequest;
import com.ak4n1.turn_management.feature.configuration.dto.response.BusinessPolicyResponse;
import com.ak4n1.turn_management.feature.configuration.repository.BusinessPolicyRepository;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

/**
 * Implementación del servicio de políticas de negocio.
 * 
 * Implementa US-T019.
 */
@Service
public class BusinessPolicyServiceImpl implements BusinessPolicyService {

    private static final Logger logger = LoggerFactory.getLogger(BusinessPolicyServiceImpl.class);

    private final BusinessPolicyRepository repository;
    private final UserService userService;

    public BusinessPolicyServiceImpl(BusinessPolicyRepository repository, UserService userService) {
        this.repository = repository;
        this.userService = userService;
    }

    /**
     * Crea o actualiza una política de negocio.
     */
    @Override
    @Transactional
    public BusinessPolicyResponse createOrUpdatePolicy(BusinessPolicyRequest request, Long userId) {
        logger.info("Creando o actualizando política de negocio - Usuario: {}", userId);

        // 1. Validar request
        validatePolicyRequest(request);

        // 2. Desactivar política anterior si existe
        Optional<BusinessPolicy> existingActivePolicy = repository.findByActiveTrue();
        if (existingActivePolicy.isPresent()) {
            BusinessPolicy oldPolicy = existingActivePolicy.get();
            oldPolicy.setActive(false);
            repository.save(oldPolicy);
            logger.info("Política anterior desactivada - ID: {}", oldPolicy.getId());
        }

        // 3. Crear nueva política activa
        BusinessPolicy policy = new BusinessPolicy();
        policy.setActive(true);
        policy.setMaxAppointmentsPerUserPerDay(request.getMaxAppointmentsPerUser().getPerDay());
        policy.setMaxAppointmentsPerUserPerWeek(request.getMaxAppointmentsPerUser().getPerWeek());
        policy.setMaxAppointmentsPerUserPerMonth(request.getMaxAppointmentsPerUser().getPerMonth());
        policy.setMinimumAdvanceHours(request.getTimeWindows().getMinAdvanceHours());
        policy.setMaximumAdvanceDays(request.getTimeWindows().getMaxAdvanceDays());
        policy.setMinimumCancellationWindowHours(request.getTimeWindows().getMinimumCancellationWindowHours());
        policy.setCreatedAppointmentTtlMinutes(request.getTimeWindows().getCreatedAppointmentTtlMinutes());
        policy.setAllowMultipleReservations(request.getAllowMultipleReservations() != null ? request.getAllowMultipleReservations() : false);
        policy.setCreatedByUserId(userId);
        policy.setNotes(request.getNotes());

        BusinessPolicy savedPolicy = repository.save(policy);
        logger.info("Política creada/actualizada - ID: {}", savedPolicy.getId());

        // 4. Mapear a response
        return mapToResponse(savedPolicy);
    }

    /**
     * Obtiene la política activa actual.
     */
    @Override
    @Transactional(readOnly = true)
    public BusinessPolicyResponse getActivePolicy() {
        logger.info("Consultando política activa");

        Optional<BusinessPolicy> policy = repository.findByActiveTrue();
        if (policy.isEmpty()) {
            return null;
        }

        return mapToResponse(policy.get());
    }

    /**
     * Obtiene la política activa como entidad (para uso interno).
     */
    @Override
    @Transactional(readOnly = true)
    public BusinessPolicy getActivePolicyEntity() {
        logger.debug("Consultando política activa (entidad)");

        Optional<BusinessPolicy> policy = repository.findByActiveTrue();
        return policy.orElse(null);
    }

    /**
     * Valida la request de política.
     */
    private void validatePolicyRequest(BusinessPolicyRequest request) {
        // Validaciones de negocio

        // Validar que minAdvanceHours sea menor que maxAdvanceDays * 24
        int minAdvanceHours = request.getTimeWindows().getMinAdvanceHours();
        int maxAdvanceDays = request.getTimeWindows().getMaxAdvanceDays();
        int maxAdvanceHours = maxAdvanceDays * 24;

        if (minAdvanceHours >= maxAdvanceHours) {
            throw new ApiException(
                String.format("minAdvanceHours (%d) debe ser menor que maxAdvanceDays * 24 (%d)", 
                    minAdvanceHours, maxAdvanceHours),
                HttpStatus.BAD_REQUEST);
        }

        // Validar que perDay <= perWeek <= perMonth (si perMonth está definido)
        int perDay = request.getMaxAppointmentsPerUser().getPerDay();
        int perWeek = request.getMaxAppointmentsPerUser().getPerWeek();
        Integer perMonth = request.getMaxAppointmentsPerUser().getPerMonth();

        if (perDay > perWeek) {
            throw new ApiException(
                String.format("perDay (%d) no puede ser mayor que perWeek (%d)", perDay, perWeek),
                HttpStatus.BAD_REQUEST);
        }

        if (perMonth != null && perWeek > perMonth) {
            throw new ApiException(
                String.format("perWeek (%d) no puede ser mayor que perMonth (%d)", perWeek, perMonth),
                HttpStatus.BAD_REQUEST);
        }

        logger.debug("Request de política validada correctamente");
    }

    /**
     * Mapea entidad BusinessPolicy a BusinessPolicyResponse.
     */
    private BusinessPolicyResponse mapToResponse(BusinessPolicy policy) {
        BusinessPolicyResponse response = new BusinessPolicyResponse();
        response.setId(policy.getId());
        response.setActive(policy.getActive());

        // Mapear maxAppointmentsPerUser
        BusinessPolicyResponse.MaxAppointmentsPerUserResponse maxAppointments = 
            new BusinessPolicyResponse.MaxAppointmentsPerUserResponse();
        maxAppointments.setPerDay(policy.getMaxAppointmentsPerUserPerDay());
        maxAppointments.setPerWeek(policy.getMaxAppointmentsPerUserPerWeek());
        maxAppointments.setPerMonth(policy.getMaxAppointmentsPerUserPerMonth());
        response.setMaxAppointmentsPerUser(maxAppointments);

        // Mapear timeWindows
        BusinessPolicyResponse.TimeWindowsResponse timeWindows = 
            new BusinessPolicyResponse.TimeWindowsResponse();
        timeWindows.setMinAdvanceHours(policy.getMinimumAdvanceHours());
        timeWindows.setMaxAdvanceDays(policy.getMaximumAdvanceDays());
        timeWindows.setMinimumCancellationWindowHours(policy.getMinimumCancellationWindowHours());
        timeWindows.setCreatedAppointmentTtlMinutes(policy.getCreatedAppointmentTtlMinutes());
        response.setTimeWindows(timeWindows);

        response.setAllowMultipleReservations(policy.getAllowMultipleReservations());
        response.setNotes(policy.getNotes());
        response.setCreatedByUserId(policy.getCreatedByUserId());
        response.setCreatedAt(policy.getCreatedAt());
        response.setUpdatedAt(policy.getUpdatedAt());

        // Obtener email del usuario
        Optional<User> user = userService.findById(policy.getCreatedByUserId());
        if (user.isPresent()) {
            response.setCreatedByEmail(user.get().getEmail());
        }

        return response;
    }
}

