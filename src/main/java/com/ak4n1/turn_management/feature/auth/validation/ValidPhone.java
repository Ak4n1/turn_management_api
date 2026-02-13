package com.ak4n1.turn_management.feature.auth.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Valida que el teléfono tenga un formato válido (opcional: null o vacío permitido).
 * Acepta números locales de 10-11 dígitos o con código de país.
 */
@Documented
@Constraint(validatedBy = ValidPhoneValidator.class)
@Target({ ElementType.FIELD })
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidPhone {

    String message() default "El teléfono no tiene un formato válido";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
