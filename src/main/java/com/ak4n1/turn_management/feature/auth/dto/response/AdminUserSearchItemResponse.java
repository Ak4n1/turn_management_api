package com.ak4n1.turn_management.feature.auth.dto.response;

/**
 * Respuesta liviana para búsquedas de usuarios (solo admin).
 * Pensado para selectores/autocomplete en UI (ej: envío manual de notificaciones).
 */
public class AdminUserSearchItemResponse {

    private Long id;
    private String email;
    private String firstName;
    private String lastName;

    public AdminUserSearchItemResponse() {
    }

    public AdminUserSearchItemResponse(Long id, String email, String firstName, String lastName) {
        this.id = id;
        this.email = email;
        this.firstName = firstName;
        this.lastName = lastName;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
}

