# 🔧 Propuesta de Refactorización: CalendarConfigurationServiceImpl

## 📊 Análisis Actual

### Estado Actual
- **Archivo**: `CalendarConfigurationServiceImpl.java`
- **Líneas**: ~2,668 líneas
- **Problema**: Viola el principio de responsabilidad única (SRP)
- **Dependencias**: 10+ repositorios y servicios inyectados

### Responsabilidades Identificadas

El servicio actual tiene **7 responsabilidades principales**:

1. **Gestión de Configuración** (CRUD, versionado, activación)
2. **Validación de Configuraciones** (múltiples validadores privados)
3. **Evaluación de Días** (aplicar reglas de precedencia: bloqueos → excepciones → base)
4. **Cálculo de Impacto** (preview de cambios, turnos afectados)
5. **Generación de Slots** (calcular horarios disponibles)
6. **Cancelación de Turnos** (cancelar turnos afectados, notificaciones)
7. **Historial y Auditoría** (versiones, cambios detectados)

---

## 🎯 Objetivo de la Refactorización

**Separar responsabilidades** en servicios especializados siguiendo **SOLID**:
- ✅ **Single Responsibility Principle (SRP)**: Cada servicio una responsabilidad
- ✅ **Open/Closed Principle (OCP)**: Extensible sin modificar código existente
- ✅ **Dependency Inversion Principle (DIP)**: Depender de abstracciones (interfaces)

---

## 📁 Nueva Estructura de Paquetes

```
com.ak4n1.turn_management.feature.configuration/
│
├── service/                                    # Servicios principales (orquestadores)
│   ├── CalendarConfigurationService.java       # Interface (sin cambios)
│   └── CalendarConfigurationServiceImpl.java  # ⭐ REFACTORIZADO (orquestador delgado)
│
├── service/
│   ├── configuration/                          # 🆕 Gestión de configuración
│   │   ├── ConfigurationManagementService.java
│   │   └── ConfigurationVersionService.java
│   │
│   ├── validation/                             # 🆕 Validaciones
│   │   ├── ConfigurationValidator.java
│   │   ├── WeeklyConfigValidator.java
│   │   ├── DailyHoursValidator.java
│   │   └── AppointmentDurationValidator.java
│   │
│   ├── evaluation/                             # 🆕 Evaluación de días
│   │   ├── DayEvaluationService.java
│   │   ├── DayEvaluator.java                   # Strategy pattern para diferentes tipos
│   │   └── evaluators/
│   │       ├── BlockDayEvaluator.java
│   │       ├── ExceptionDayEvaluator.java
│   │       └── BaseDayEvaluator.java
│   │
│   ├── impact/                                 # 🆕 Cálculo de impacto
│   │   ├── ImpactCalculationService.java
│   │   ├── ImpactCalculator.java               # Strategy pattern
│   │   └── calculators/
│   │       ├── WeeklyConfigImpactCalculator.java
│   │       ├── DailyHoursImpactCalculator.java
│   │       ├── AppointmentDurationImpactCalculator.java
│   │       ├── ExceptionImpactCalculator.java
│   │       └── BlockImpactCalculator.java
│   │
│   ├── slots/                                  # 🆕 Generación de slots
│   │   ├── SlotGenerationService.java
│   │   └── SlotFilterService.java
│   │
│   ├── cancellation/                           # 🆕 Cancelación de turnos
│   │   ├── AppointmentCancellationService.java
│   │   └── AffectedAppointmentsService.java
│   │
│   └── history/                                # 🆕 Historial y auditoría
│       └── ConfigurationHistoryService.java
│
└── util/                                       # 🆕 Utilidades compartidas
    ├── DateUtils.java
    └── DayNameUtils.java
```

---

## 🔨 Nuevos Servicios y Responsabilidades

### 1. **ConfigurationManagementService**
**Responsabilidad**: Gestión CRUD de configuraciones

**Métodos**:
```java
public interface ConfigurationManagementService {
    CalendarConfiguration createWeeklyConfig(WeeklyConfigRequest request, Long userId);
    CalendarConfiguration configureDailyHours(DailyHoursConfigRequest request, Long userId);
    CalendarConfiguration configureAppointmentDuration(AppointmentDurationRequest request, Long userId);
    CalendarConfiguration getActiveConfiguration();
    void deactivatePreviousConfiguration();
}
```

**Código que se mueve aquí**:
- `createWeeklyConfig()` (líneas 111-315)
- `configureDailyHours()` (líneas 393-473)
- `configureAppointmentDuration()` (líneas 556-649)
- `getActiveConfiguration()` (líneas 318-322)
- `deactivatePreviousConfiguration()` (líneas 381-389)

---

### 2. **ConfigurationVersionService**
**Responsabilidad**: Gestión de versionado

**Métodos**:
```java
public interface ConfigurationVersionService {
    Integer calculateNextVersion();
    void validateVersionTransition(CalendarConfiguration current, CalendarConfiguration newConfig);
}
```

**Código que se mueve aquí**:
- `calculateNextVersion()` (líneas 372-376)

---

### 3. **ConfigurationValidator** (Interface)
**Responsabilidad**: Validaciones de configuraciones

**Implementaciones**:
```java
public interface ConfigurationValidator<T> {
    void validate(T request);
}

// Implementaciones específicas:
- WeeklyConfigValidator
- DailyHoursValidator  
- AppointmentDurationValidator
```

**Código que se mueve aquí**:
- `validateWeeklyConfigRequest()` (líneas 328-351)
- `validateWeeklyConfig()` (líneas 352-367)
- `validateDailyHoursConfigRequest()` (líneas 477-488)
- `validateOnlyOpenDaysHaveHours()` (líneas 496-517)
- `validateNoOverlaps()` (líneas 523-534)
- `validateTimeRanges()` (líneas 541-553)
- `validateAppointmentDurationRequest()` (líneas 653-671)
- `validateDurationCompatibility()` (líneas 672-715)

---

### 4. **DayEvaluationService**
**Responsabilidad**: Evaluar disponibilidad de días aplicando precedencia

**Métodos**:
```java
public interface DayEvaluationService {
    ConsolidatedDayResponse evaluateDay(LocalDate date, CalendarConfiguration config,
                                       List<CalendarException> exceptions,
                                       List<ManualBlock> blocks);
    
    Boolean hasExistingAppointments(LocalDate date);
    Integer countExistingAppointments(LocalDate date);
}
```

**Código que se mueve aquí**:
- `evaluateDay()` (líneas 810-834)
- `evaluateDayWithBlock()` (líneas 853-878)
- `evaluateDayWithException()` (líneas 882-911)
- `evaluateDayWithBase()` (líneas 915-1025)
- `calculateHasExistingAppointments()` (líneas 1027-1054)
- `countExistingAppointments()` (líneas 1055-1071)

**Strategy Pattern para evaluadores**:
```java
public interface DayEvaluator {
    ConsolidatedDayResponse evaluate(LocalDate date, CalendarConfiguration config,
                                    List<CalendarException> exceptions,
                                    List<ManualBlock> blocks);
}

// Implementaciones:
- BlockDayEvaluator: Evalúa días con bloqueos
- ExceptionDayEvaluator: Evalúa días con excepciones
- BaseDayEvaluator: Evalúa días con configuración base
```

---

### 5. **ImpactCalculationService**
**Responsabilidad**: Calcular impacto de cambios propuestos

**Métodos**:
```java
public interface ImpactCalculationService {
    PreviewImpactResponse previewImpact(PreviewImpactRequest request);
    List<AffectedAppointmentInfo> calculateAffectedAppointments(LocalDate startDate, LocalDate endDate);
    List<AffectedAppointmentInfo> calculateAffectedAppointmentsForDates(List<LocalDate> dates);
}
```

**Código que se mueve aquí**:
- `previewImpact()` (líneas 1171-1231)
- `calculateImpact()` (líneas 1278-1304)
- `calculateWeeklyConfigImpact()` (líneas 1308-1455)
- `calculateDailyHoursImpact()` (líneas 1459-1528)
- `calculateAppointmentDurationImpact()` (líneas 1532-1605)
- `calculateExceptionImpact()` (líneas 1609-1688)
- `calculateBlockImpact()` (líneas 1692-1767)
- `calculateAffectedAppointments()` (líneas 1812-1861)
- `calculateAffectedAppointmentsForDates()` (líneas 1869-1933)
- `countSlotsForDay()` (líneas 1772-1788)
- `getDayOpenFromRequest()` (líneas 1792-1804)

**Strategy Pattern para calculadores**:
```java
public interface ImpactCalculator {
    ImpactCalculationResult calculate(PreviewImpactRequest request, CalendarConfiguration currentConfig);
}

// Implementaciones:
- WeeklyConfigImpactCalculator
- DailyHoursImpactCalculator
- AppointmentDurationImpactCalculator
- ExceptionImpactCalculator
- BlockImpactCalculator
```

---

### 6. **SlotGenerationService**
**Responsabilidad**: Generar slots disponibles para una fecha

**Métodos**:
```java
public interface SlotGenerationService {
    SlotsResponse getAvailableSlots(LocalDate date);
    List<SlotResponse> generateSlotsFromRange(LocalTime start, LocalTime end, Integer durationMinutes);
    List<SlotResponse> excludeOccupiedSlots(List<SlotResponse> slots, LocalDate date);
    boolean isSlotBlocked(LocalTime slotStart, LocalTime slotEnd, List<ManualBlock> blocks);
}
```

**Código que se mueve aquí**:
- `getAvailableSlots()` (líneas 2027-2324)
- `generateSlotsFromRange()` (líneas 2219-2257)
- `excludeOccupiedSlots()` (líneas 2298-2323)
- `isSlotBlocked()` (líneas 2259-2297)

---

### 7. **AppointmentCancellationService**
**Responsabilidad**: Cancelar turnos afectados por cambios de configuración

**Métodos**:
```java
public interface AppointmentCancellationService {
    void cancelAffectedAppointments(List<Long> appointmentIds, String reason, Boolean sendNotifications);
    void cancelAffectedAppointmentsByDayClosure(List<com.ak4n1.turn_management.feature.appointment.domain.Appointment> appointments,
                                                 String reason, Boolean sendNotifications);
}
```

**Código que se mueve aquí**:
- `cancelAffectedAppointmentsByDayClosure()` (líneas 1080-1158)

---

### 8. **AffectedAppointmentsService**
**Responsabilidad**: Identificar y gestionar turnos afectados

**Métodos**:
```java
public interface AffectedAppointmentsService {
    List<AffectedAppointmentInfo> findAffectedAppointments(List<LocalDate> dates);
    List<AffectedAppointmentInfo> findAffectedAppointmentsInRange(LocalDate startDate, LocalDate endDate);
}
```

---

### 9. **ConfigurationHistoryService**
**Responsabilidad**: Historial y auditoría de configuraciones

**Métodos**:
```java
public interface ConfigurationHistoryService {
    ConfigurationHistoryResponse getConfigurationHistory();
    List<String> detectChanges(CalendarConfiguration current, CalendarConfiguration previous);
}
```

**Código que se mueve aquí**:
- `getConfigurationHistory()` (líneas 2525-2568)
- `detectChanges()` (líneas 2572-2668)

---

### 10. **Utilidades Compartidas**

#### **DateUtils.java**
```java
public class DateUtils {
    public static LocalDate getTodayGMT3();
    public static void validateDateRange(LocalDate startDate, LocalDate endDate);
}
```

**Código que se mueve aquí**:
- `getTodayGMT3()` (líneas 2488-2492)
- `validateDateRange()` (líneas 779-797)

#### **DayNameUtils.java**
```java
public class DayNameUtils {
    public static String getDayName(Integer dayOfWeek);
    public static String capitalizeFirst(String str);
}
```

**Código que se mueve aquí**:
- `getDayName()` (líneas 2499-2511)
- `capitalizeFirst()` (líneas 1162-1168)

---

## 🔄 CalendarConfigurationServiceImpl Refactorizado

El servicio principal se convierte en un **orquestador delgado** que delega a los servicios especializados:

```java
@Service
public class CalendarConfigurationServiceImpl implements CalendarConfigurationService {

    private final ConfigurationManagementService configurationManagementService;
    private final ConfigurationValidator<WeeklyConfigRequest> weeklyConfigValidator;
    private final ConfigurationValidator<DailyHoursConfigRequest> dailyHoursValidator;
    private final ConfigurationValidator<AppointmentDurationRequest> appointmentDurationValidator;
    private final DayEvaluationService dayEvaluationService;
    private final ImpactCalculationService impactCalculationService;
    private final SlotGenerationService slotGenerationService;
    private final AppointmentCancellationService appointmentCancellationService;
    private final ConfigurationHistoryService configurationHistoryService;
    private final CalendarConfigurationMapper mapper;

    // Constructor con inyección de dependencias

    @Override
    @Transactional
    public CalendarConfigurationResponse createWeeklyConfig(WeeklyConfigRequest request, Long userId) {
        // 1. Validar
        weeklyConfigValidator.validate(request);
        
        // 2. Delegar creación
        CalendarConfiguration config = configurationManagementService
            .createWeeklyConfig(request, userId);
        
        // 3. Procesar turnos afectados (si aplica)
        if (request.getAppointmentIdsToCancel() != null && 
            !request.getAppointmentIdsToCancel().isEmpty()) {
            appointmentCancellationService.cancelAffectedAppointments(
                request.getAppointmentIdsToCancel(),
                request.getCancellationReason() != null ? 
                    request.getCancellationReason() : "Día cerrado según nueva configuración",
                Boolean.TRUE.equals(request.getAutoCancelAffectedAppointments())
            );
        }
        
        return mapper.toResponse(config);
    }

    @Override
    public ConsolidatedCalendarResponse getConsolidatedCalendar(LocalDate startDate, LocalDate endDate) {
        DateUtils.validateDateRange(startDate, endDate);
        
        CalendarConfiguration activeConfig = configurationManagementService.getActiveConfiguration();
        if (activeConfig == null) {
            throw new ApiException("No hay configuración activa", HttpStatus.NOT_FOUND);
        }

        // Obtener excepciones y bloqueos en rango
        List<CalendarException> exceptions = exceptionRepository
            .findByActiveTrueAndExceptionDateBetween(startDate, endDate);
        List<ManualBlock> blocks = manualBlockRepository
            .findByActiveTrueAndBlockDateBetween(startDate, endDate);

        // Evaluar cada día
        List<ConsolidatedDayResponse> days = new ArrayList<>();
        LocalDate currentDate = startDate;
        while (!currentDate.isAfter(endDate)) {
            ConsolidatedDayResponse day = dayEvaluationService.evaluateDay(
                currentDate, activeConfig, exceptions, blocks);
            days.add(day);
            currentDate = currentDate.plusDays(1);
        }

        return new ConsolidatedCalendarResponse(days);
    }

    @Override
    public PreviewImpactResponse previewImpact(PreviewImpactRequest request) {
        return impactCalculationService.previewImpact(request);
    }

    @Override
    public AvailabilityResponse checkAvailability(LocalDate date) {
        // Validación y delegación a dayEvaluationService
        // ...
    }

    @Override
    public SlotsResponse getAvailableSlots(LocalDate date) {
        return slotGenerationService.getAvailableSlots(date);
    }

    @Override
    public AvailabilityRangeResponse getAvailabilityRange(LocalDate startDate, LocalDate endDate) {
        // Validación y delegación a dayEvaluationService + slotGenerationService
        // ...
    }

    @Override
    public ConfigurationHistoryResponse getConfigurationHistory() {
        return configurationHistoryService.getConfigurationHistory();
    }
}
```

**Tamaño estimado**: ~200-300 líneas (vs 2,668 actuales)

---

## 📋 Plan de Migración

### Fase 1: Extraer Utilidades (Bajo Riesgo)
1. Crear `DateUtils.java` y `DayNameUtils.java`
2. Reemplazar llamadas en `CalendarConfigurationServiceImpl`
3. ✅ Tests pasan

### Fase 2: Extraer Validadores (Bajo Riesgo)
1. Crear interfaces y clases de validación
2. Mover métodos de validación
3. Inyectar validadores en servicio principal
4. ✅ Tests pasan

### Fase 3: Extraer DayEvaluationService (Medio Riesgo)
1. Crear `DayEvaluationService` y evaluadores
2. Mover lógica de evaluación
3. Actualizar `getConsolidatedCalendar()` y `checkAvailability()`
4. ✅ Tests pasan

### Fase 4: Extraer SlotGenerationService (Medio Riesgo)
1. Crear `SlotGenerationService`
2. Mover lógica de generación de slots
3. Actualizar `getAvailableSlots()` y `getAvailabilityRange()`
4. ✅ Tests pasan

### Fase 5: Extraer ImpactCalculationService (Alto Riesgo)
1. Crear `ImpactCalculationService` y calculadores
2. Mover lógica de cálculo de impacto
3. Actualizar `previewImpact()`
4. ✅ Tests pasan

### Fase 6: Extraer AppointmentCancellationService (Alto Riesgo)
1. Crear `AppointmentCancellationService`
2. Mover lógica de cancelación
3. Actualizar `createWeeklyConfig()`
4. ✅ Tests pasan

### Fase 7: Extraer ConfigurationManagementService (Alto Riesgo)
1. Crear `ConfigurationManagementService` y `ConfigurationVersionService`
2. Mover lógica de CRUD y versionado
3. Actualizar métodos principales
4. ✅ Tests pasan

### Fase 8: Extraer ConfigurationHistoryService (Bajo Riesgo)
1. Crear `ConfigurationHistoryService`
2. Mover lógica de historial
3. Actualizar `getConfigurationHistory()`
4. ✅ Tests pasan

### Fase 9: Limpieza Final
1. Eliminar código muerto
2. Optimizar imports
3. Revisar documentación
4. ✅ Code review

---

## ✅ Beneficios Esperados

### Mantenibilidad
- ✅ Cada servicio tiene una responsabilidad clara
- ✅ Código más fácil de entender y modificar
- ✅ Cambios aislados (no afectan otros servicios)

### Testabilidad
- ✅ Servicios pequeños y fáciles de testear
- ✅ Mocks más simples (menos dependencias)
- ✅ Tests unitarios más rápidos

### Escalabilidad
- ✅ Fácil agregar nuevas validaciones
- ✅ Fácil agregar nuevos tipos de evaluadores
- ✅ Fácil agregar nuevos calculadores de impacto

### Reutilización
- ✅ Servicios reutilizables en otros contextos
- ✅ Utilidades compartidas
- ✅ Lógica de negocio centralizada

### Rendimiento
- ✅ Posibilidad de cachear servicios independientes
- ✅ Paralelización de operaciones independientes
- ✅ Optimizaciones específicas por servicio

---

## 🎯 Métricas de Éxito

### Antes
- ❌ 2,668 líneas en un solo archivo
- ❌ 10+ dependencias inyectadas
- ❌ 7 responsabilidades mezcladas
- ❌ Difícil de testear
- ❌ Difícil de mantener

### Después (Objetivo)
- ✅ ~200-300 líneas en servicio principal (orquestador)
- ✅ 5-10 servicios especializados (~200-400 líneas cada uno)
- ✅ 1 responsabilidad por servicio
- ✅ Fácil de testear (servicios pequeños)
- ✅ Fácil de mantener (cambios aislados)

---

## 📝 Notas Adicionales

### Patrones de Diseño Aplicados
- **Strategy Pattern**: Para evaluadores de días y calculadores de impacto
- **Factory Pattern**: Para crear evaluadores/calculadores según el tipo
- **Dependency Injection**: Para inyectar servicios especializados

### Consideraciones
- Mantener compatibilidad con la interfaz `CalendarConfigurationService`
- No romper contratos existentes
- Migración incremental (fase por fase)
- Tests deben pasar en cada fase

### Próximos Pasos
1. Revisar y aprobar esta propuesta
2. Crear issues/tareas para cada fase
3. Comenzar con Fase 1 (utilidades)
4. Iterar fase por fase con validación continua

---

**Autor**: AI Assistant  
**Fecha**: 2026-01-XX  
**Versión**: 1.0

