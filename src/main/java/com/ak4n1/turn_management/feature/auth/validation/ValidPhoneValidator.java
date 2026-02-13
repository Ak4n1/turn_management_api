package com.ak4n1.turn_management.feature.auth.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

/**
 * Valida formato de teléfono: opcional (null o vacío válido).
 * Si tiene valor: solo dígitos, 10-11 (local) o 12-13 con código de país.
 */
public class ValidPhoneValidator implements ConstraintValidator<ValidPhone, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        if (value == null || value.isBlank()) {
            return true;
        }
        String digits = value.replaceAll("\\D", "");
        if (digits.isEmpty()) {
            return false;
        }
        int len = digits.length();
        if (digits.startsWith("54") && len >= 12) {
            // Con código de país: 54 + opcional 9 + 10 dígitos → 12 o 13
            String rest = digits.substring(2);
            return rest.length() == 10 || (rest.length() == 11 && rest.startsWith("9"));
        }
        // Local: 10 dígitos (área + número) o 11 (15 + área + número)
        return len == 10 || len == 11;
    }
}
