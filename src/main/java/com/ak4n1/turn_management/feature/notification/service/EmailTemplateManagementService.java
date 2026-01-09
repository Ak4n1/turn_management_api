package com.ak4n1.turn_management.feature.notification.service;

import com.ak4n1.turn_management.feature.notification.domain.EmailTemplateEntity;
import com.ak4n1.turn_management.feature.notification.dto.request.CreateEmailTemplateRequest;
import com.ak4n1.turn_management.feature.notification.dto.response.EmailTemplateResponse;
import com.ak4n1.turn_management.feature.notification.repository.EmailTemplateRepository;
import com.ak4n1.turn_management.feature.notification.service.template.EmailTemplateType;
import com.ak4n1.turn_management.shared.exception.ApiException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Servicio para gestión de plantillas de email.
 * Implementa US-T034.
 */
@Service
public class EmailTemplateManagementService {

    private static final Logger logger = LoggerFactory.getLogger(EmailTemplateManagementService.class);
    private static final Pattern VARIABLE_PATTERN = Pattern.compile("\\{\\{([a-zA-Z0-9_]+)\\}\\}");

    private final EmailTemplateRepository templateRepository;

    public EmailTemplateManagementService(EmailTemplateRepository templateRepository) {
        this.templateRepository = templateRepository;
    }

    /**
     * Obtiene todas las plantillas activas.
     * 
     * @return Lista de plantillas activas
     */
    public List<EmailTemplateResponse> getAllActiveTemplates() {
        List<EmailTemplateEntity> templates = templateRepository.findByActiveTrueOrderByTemplateTypeAscVersionDesc();
        return templates.stream()
            .map(this::mapToResponse)
            .collect(Collectors.toList());
    }

    /**
     * Obtiene todas las plantillas de un tipo específico (incluyendo versiones antiguas).
     * 
     * @param type Tipo de plantilla
     * @return Lista de plantillas ordenadas por versión descendente
     */
    public List<EmailTemplateResponse> getTemplatesByType(EmailTemplateType type) {
        List<EmailTemplateEntity> templates = templateRepository.findByTemplateTypeOrderByVersionDesc(type);
        return templates.stream()
            .map(this::mapToResponse)
            .collect(Collectors.toList());
    }

    /**
     * Obtiene la plantilla activa de un tipo específico.
     * 
     * @param type Tipo de plantilla
     * @return Plantilla activa o null si no existe
     */
    public EmailTemplateResponse getActiveTemplate(EmailTemplateType type) {
        return templateRepository.findActiveByType(type)
            .map(this::mapToResponse)
            .orElse(null);
    }

    /**
     * Crea una nueva versión de una plantilla.
     * 
     * @param request Datos de la plantilla
     * @param userId ID del usuario que crea la plantilla
     * @return Plantilla creada
     */
    @Transactional
    public EmailTemplateResponse createTemplate(CreateEmailTemplateRequest request, Long userId) {
        // Validar variables en el contenido
        List<String> detectedVariables = extractVariables(request.getSubject() + " " + request.getBody());
        if (request.getVariables() != null && !request.getVariables().isEmpty()) {
            // Validar que todas las variables declaradas estén en el contenido
            for (String declaredVar : request.getVariables()) {
                if (!detectedVariables.contains(declaredVar)) {
                    logger.warn("Variable '{}' declarada pero no encontrada en el contenido", declaredVar);
                }
            }
        }

        // Obtener la versión más reciente del tipo
        List<EmailTemplateEntity> existingTemplates = templateRepository.findByTemplateTypeOrderByVersionDesc(request.getType());
        int nextVersion = existingTemplates.isEmpty() ? 1 : existingTemplates.get(0).getVersion() + 1;

        // Si se está activando una nueva plantilla, desactivar las anteriores del mismo tipo
        if (Boolean.TRUE.equals(request.getActive())) {
            List<EmailTemplateEntity> activeTemplates = existingTemplates.stream()
                .filter(EmailTemplateEntity::getActive)
                .collect(Collectors.toList());
            for (EmailTemplateEntity activeTemplate : activeTemplates) {
                activeTemplate.setActive(false);
                templateRepository.save(activeTemplate);
            }
        }

        // Crear nueva plantilla
        EmailTemplateEntity template = new EmailTemplateEntity(
            request.getType(),
            request.getSubject(),
            request.getBody(),
            request.getVariables() != null ? request.getVariables() : detectedVariables,
            userId
        );
        template.setVersion(nextVersion);
        template.setActive(request.getActive());
        template.setNotes(request.getNotes());

        template = templateRepository.save(template);

        logger.info("Plantilla creada - Tipo: {}, Versión: {}, Activa: {}", 
            request.getType(), nextVersion, request.getActive());

        return mapToResponse(template);
    }

    /**
     * Actualiza una plantilla existente (crea una nueva versión).
     * 
     * @param templateId ID de la plantilla a actualizar
     * @param request Datos actualizados
     * @param userId ID del usuario que actualiza
     * @return Nueva versión de la plantilla
     */
    @Transactional
    public EmailTemplateResponse updateTemplate(Long templateId, CreateEmailTemplateRequest request, Long userId) {
        EmailTemplateEntity existingTemplate = templateRepository.findById(templateId)
            .orElseThrow(() -> new ApiException("Plantilla no encontrada", HttpStatus.NOT_FOUND));

        // Validar que el tipo no cambie
        if (existingTemplate.getTemplateType() != request.getType()) {
            throw new ApiException("No se puede cambiar el tipo de una plantilla existente", HttpStatus.BAD_REQUEST);
        }

        // Crear nueva versión (no actualizar la existente)
        CreateEmailTemplateRequest createRequest = new CreateEmailTemplateRequest();
        createRequest.setType(request.getType());
        createRequest.setSubject(request.getSubject());
        createRequest.setBody(request.getBody());
        createRequest.setVariables(request.getVariables());
        createRequest.setActive(request.getActive());
        createRequest.setNotes(request.getNotes());

        return createTemplate(createRequest, userId);
    }

    /**
     * Activa o desactiva una plantilla.
     * 
     * @param templateId ID de la plantilla
     * @param active Estado activo
     * @param userId ID del usuario que realiza la acción
     * @return Plantilla actualizada
     */
    @Transactional
    public EmailTemplateResponse setTemplateActive(Long templateId, Boolean active, Long userId) {
        EmailTemplateEntity template = templateRepository.findById(templateId)
            .orElseThrow(() -> new ApiException("Plantilla no encontrada", HttpStatus.NOT_FOUND));

        // Si se está desactivando, verificar que haya otra plantilla activa del mismo tipo
        if (Boolean.FALSE.equals(active)) {
            long activeCount = templateRepository.countByTemplateTypeAndActiveTrue(template.getTemplateType());
            if (activeCount <= 1 && Boolean.TRUE.equals(template.getActive())) {
                throw new ApiException(
                    "No se puede desactivar la única plantilla activa de este tipo. Debe haber al menos una plantilla activa.",
                    HttpStatus.BAD_REQUEST);
            }
        }

        // Si se está activando, desactivar las demás del mismo tipo
        if (Boolean.TRUE.equals(active)) {
            List<EmailTemplateEntity> activeTemplates = templateRepository.findByTemplateTypeOrderByVersionDesc(template.getTemplateType())
                .stream()
                .filter(EmailTemplateEntity::getActive)
                .collect(Collectors.toList());
            for (EmailTemplateEntity activeTemplate : activeTemplates) {
                if (!activeTemplate.getId().equals(templateId)) {
                    activeTemplate.setActive(false);
                    templateRepository.save(activeTemplate);
                }
            }
        }

        template.setActive(active);
        template.setUpdatedAt(LocalDateTime.now());
        template = templateRepository.save(template);

        logger.info("Plantilla {} - Tipo: {}, Versión: {}, Activa: {}", 
            active ? "activada" : "desactivada", template.getTemplateType(), template.getVersion(), active);

        return mapToResponse(template);
    }

    /**
     * Extrae las variables ({{variableName}}) de un texto.
     * 
     * @param text Texto a analizar
     * @return Lista de nombres de variables encontradas
     */
    private List<String> extractVariables(String text) {
        return VARIABLE_PATTERN.matcher(text)
            .results()
            .map(match -> match.group(1))
            .distinct()
            .collect(Collectors.toList());
    }

    /**
     * Mapea una entidad a un DTO de respuesta.
     * 
     * @param template Entidad de plantilla
     * @return DTO de respuesta
     */
    private EmailTemplateResponse mapToResponse(EmailTemplateEntity template) {
        return new EmailTemplateResponse(
            template.getId(),
            template.getTemplateType(),
            template.getVersion(),
            template.getSubject(),
            template.getBody(),
            template.getVariables(),
            template.getActive(),
            template.getNotes(),
            template.getCreatedByUserId(),
            template.getCreatedAt(),
            template.getUpdatedAt()
        );
    }
}

