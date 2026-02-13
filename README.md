# TurnFlow - API Backend

Sistema de gestion de turnos para organizaciones. API REST desarrollada en Spring Boot 4 que permite a los usuarios solicitar turnos, confirmarlos y reprogramarlos, mientras los administradores configuran calendarios, gestionan citas y envian notificaciones.

## Tecnologias

- Java 17
- Spring Boot 4
- Spring Data JPA
- Spring Security (JWT)
- Spring WebSocket
- MariaDB
- Caffeine (cache)
- Bucket4j (rate limiting)

## Arquitectura de paquetes

```
com.ak4n1.turn_management
|
+-- feature/
|   +-- appointment/
|   |   +-- controller/       # AppointmentController, AdminAppointmentController
|   |   +-- domain/           # Appointment, AppointmentState, RescheduleRequest, etc.
|   |   +-- dto/request/
|   |   +-- dto/response/
|   |   +-- repository/       # AppointmentRepository, AppointmentHistoryRepository, RescheduleRequestRepository
|   |   +-- service/          # AppointmentService, AppointmentExpirationService, AppointmentReminderService, RescheduleRequestExpirationService
|   |
|   +-- auth/
|   |   +-- controller/       # AuthController, AdminUserSearchController
|   |   +-- domain/           # User, Role
|   |   +-- dto/request/
|   |   +-- dto/response/
|   |   +-- mapper/           # UserMapper
|   |   +-- repository/       # UserRepository, RoleRepository, RefreshTokenRepository
|   |   +-- service/          # AuthService, UserService, AdminUserService, PasswordService, CustomUserDetailsService
|   |   +-- validation/       # ValidPhone, ValidPhoneValidator
|   |
|   +-- configuration/
|   |   +-- controller/       # CalendarConfigurationController, AvailabilityController, BusinessPolicyController
|   |   +-- domain/           # CalendarConfiguration, CalendarException, ManualBlock, WeeklyConfig, DailyHours
|   |   +-- dto/request/
|   |   +-- mapper/           # CalendarConfigurationMapper
|   |   +-- repository/       # CalendarConfigurationRepository, CalendarExceptionRepository, ManualBlockRepository, BusinessPolicyRepository
|   |   +-- service/          # CalendarConfigurationService, CalendarExceptionService, ManualBlockService, BusinessPolicyService
|   |   +-- service/configuration/    # ConfigurationManagementService, ConfigurationVersionService
|   |   +-- service/validation/       # WeeklyConfigValidator, DailyHoursValidator, AppointmentDurationValidator
|   |   +-- service/slots/            # SlotGenerationService
|   |   +-- service/impact/           # ImpactCalculationService
|   |   +-- service/history/          # ConfigurationHistoryService
|   |   +-- service/cancellation/     # AppointmentCancellationService
|   |   +-- service/evaluation/       # DayEvaluationService
|   |   +-- usecase/                  # CreateCalendarExceptionUseCase
|   |   +-- util/                     # DateUtils, DayNameUtils
|   |
|   +-- notification/
|       +-- controller/       # UserNotificationController, AdminManualNotificationController, AdminNotificationController, AdminEmailTemplateController, NotificationPreferenceController
|       +-- domain/           # SystemNotification, NotificationType, RelatedEntityType
|       +-- dto/request/
|       +-- dto/response/
|       +-- repository/       # SystemNotificationRepository, NotificationPreferenceRepository, EmailTemplateRepository, etc.
|       +-- service/          # SystemNotificationService, EmailService, WebSocketNotificationService, NotificationPreferenceService, EmailVerificationService, etc.
|
+-- shared/
|   +-- audit/
|   |   +-- controller/       # AuditController
|   |   +-- service/          # AuditService
|   |
|   +-- config/               # SecurityConfig, JacksonConfig, CacheConfig
|   +-- exception/            # GlobalExceptionHandler, ErrorResponse
|   +-- metrics/
|   |   +-- controller/       # MetricsController
|   |   +-- service/          # MetricsService
|   |
|   +-- scheduling/           # DistributedLockService, DistributedLockRepository
|   +-- security/
|   |   +-- jwt/              # JwtAuthenticationFilter, JwtTokenValidator
|   |   +-- rateLimit/        # RateLimitFilter, RateLimitService, estrategias por IP y usuario
|   |
|   +-- websocket/
|       +-- AppointmentWebSocketHandler
|       +-- WebSocketHandshakeInterceptor
|       +-- WebSocketSecurityValidator
|       +-- dto/
|       +-- util/             # WebSocketMessageType
|
+-- TurnManagementApplication.java
```

## Features

### Appointment (Turnos)

Gestion completa de turnos: creacion, confirmacion, cancelacion, reprogramacion y recordatorios. Los usuarios solicitan turnos desde slots disponibles; los admins pueden ver todos los turnos, cancelarlos o crear overrides. Las solicitudes de reprogramacion se aprueban o rechazan manualmente. Jobs programados expiran turnos no confirmados y envian recordatorios por email.

### Auth (Autenticacion)

Registro, login con JWT, refresh token, verificacion de email y recuperacion de contraseña. Roles ROLE_USER y ROLE_ADMIN. Los admins pueden buscar usuarios, gestionar roles y habilitar/deshabilitar cuentas.

### Configuration (Calendario)

Configuracion semanal (dias abiertos/cerrados), horarios diarios por dia, duracion de turnos y politicas de negocio (TTL de confirmacion, anticipacion minima, etc.). Excepciones de calendario (dias especiales) y bloqueos manuales. Versionado de configuraciones para mantener integridad con turnos existentes. Generacion de slots disponibles respetando configuracion activa, excepciones y bloqueos.

### Notification (Notificaciones)

Notificaciones in-app (campanita) por turnos, reprogramaciones y anuncios. Admins pueden enviar notificaciones manuales a todos los usuarios o a seleccionados. Preferencias de notificacion por tipo. Emails transaccionales (verificacion, recordatorios, reset password). WebSocket para actualizaciones en tiempo real (nuevas notificaciones, conteo de no leidas, usuarios en linea para admins).

### Shared

- **Audit**: registro de acciones para trazabilidad
- **Metrics**: endpoint de metricas basicas
- **Scheduling**: locks distribuidos para jobs en cluster (expiracion, recordatorios)
- **Security**: JWT, rate limiting por IP y usuario
- **WebSocket**: conexiones en tiempo real, heartbeat, idle timeout

## Patrones arquitectonicos

- **Arquitectura en capas**: Controller -> Service -> Repository. Los controladores reciben peticiones, delegan en servicios y estos usan repositorios para persistencia.

- **DTO (Data Transfer Object)**: Separacion entre entidades de dominio y objetos de transferencia. Request/Response DTOs para entrada y salida de la API.

- **Repository**: Abstraccion de acceso a datos con Spring Data JPA. Interfaces que extienden JpaRepository.

- **Service Layer**: Logica de negocio encapsulada en servicios. Orquestacion de repositorios, validaciones y notificaciones.

- **Use Case**: Casos de uso especificos (ej. CreateCalendarExceptionUseCase) para operaciones complejas que coordinan multiples servicios.

- **Validator**: Validadores dedicados (WeeklyConfigValidator, DailyHoursValidator, AppointmentDurationValidator, ValidPhoneValidator) para reglas de negocio reutilizables.

- **Mapper**: Conversion entre entidades y DTOs (UserMapper, CalendarConfigurationMapper).

- **Global Exception Handler**: Manejo centralizado de excepciones con @ControllerAdvice, respuestas de error consistentes.

- **Strategy**: Rate limiting con estrategias combinadas (IP y usuario). DayEvaluationService para evaluar disponibilidad por dia.

- **Cache**: Caffeine para cachear configuracion activa del calendario y reducir consultas a BD.

- **Scheduled Jobs**: Tareas programadas para expiracion de turnos, recordatorios y limpieza de notificaciones, con locks distribuidos para evitar ejecucion duplicada en cluster.
