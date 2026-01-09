-- Actualizar day_of_week para turnos existentes
-- Calcula el día de la semana (1=Lunes, 7=Domingo) desde appointment_date
UPDATE appointments 
SET day_of_week = DAYOFWEEK(appointment_date)
WHERE day_of_week IS NULL;

-- Ajustar para que 1=Lunes, 7=Domingo (DAYOFWEEK devuelve 1=Domingo, 7=Sábado)
UPDATE appointments 
SET day_of_week = CASE 
    WHEN day_of_week = 1 THEN 7  -- Domingo
    WHEN day_of_week = 2 THEN 1  -- Lunes
    WHEN day_of_week = 3 THEN 2  -- Martes
    WHEN day_of_week = 4 THEN 3  -- Miércoles
    WHEN day_of_week = 5 THEN 4  -- Jueves
    WHEN day_of_week = 6 THEN 5  -- Viernes
    WHEN day_of_week = 7 THEN 6  -- Sábado
END
WHERE day_of_week IS NOT NULL;

