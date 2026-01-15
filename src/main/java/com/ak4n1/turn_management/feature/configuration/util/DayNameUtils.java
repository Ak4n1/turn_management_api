package com.ak4n1.turn_management.feature.configuration.util;

/**
 * Utilidades para manipulación de nombres de días en español.
 */
public class DayNameUtils {

    private DayNameUtils() {
        // Clase de utilidad - no debe ser instanciada
        throw new UnsupportedOperationException("Clase de utilidad - no debe ser instanciada");
    }

    /**
     * Obtiene el nombre del día en español para mensajes de error.
     * 
     * @param dayOfWeek 1=Lunes, 2=Martes, ..., 7=Domingo
     * @return Nombre del día en español
     */
    public static String getDayName(Integer dayOfWeek) {
        return switch (dayOfWeek) {
            case 1 -> "lunes";
            case 2 -> "martes";
            case 3 -> "miércoles";
            case 4 -> "jueves";
            case 5 -> "viernes";
            case 6 -> "sábado";
            case 7 -> "domingo";
            default -> "día " + dayOfWeek;
        };
    }

    /**
     * Capitaliza la primera letra de una cadena.
     * 
     * @param str Cadena a capitalizar
     * @return Cadena con la primera letra en mayúscula, o la cadena original si es null o vacía
     */
    public static String capitalizeFirst(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
