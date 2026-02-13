package com.ak4n1.turn_management.feature.auth.dto.response;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

public class UserResponse {

    private Long id;
    private String email;
    private String firstName;
    private String lastName;
    private String phone;
    private String street;
    private String streetNumber;
    private String floorApt;
    private String city;
    private String postalCode;
    private LocalDate birthDate;
    /** Flag derivado: indica si el perfil está completo para pedir turnos. */
    private Boolean profileComplete;
    private Boolean enabled;
    private Boolean emailVerified;
    private Set<String> roles;
    private LocalDateTime createdAt;

    public UserResponse() {
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

    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    public String getStreet() { return street; }
    public void setStreet(String street) { this.street = street; }
    public String getStreetNumber() { return streetNumber; }
    public void setStreetNumber(String streetNumber) { this.streetNumber = streetNumber; }
    public String getFloorApt() { return floorApt; }
    public void setFloorApt(String floorApt) { this.floorApt = floorApt; }
    public String getCity() { return city; }
    public void setCity(String city) { this.city = city; }
    public String getPostalCode() { return postalCode; }
    public void setPostalCode(String postalCode) { this.postalCode = postalCode; }
    public LocalDate getBirthDate() { return birthDate; }
    public void setBirthDate(LocalDate birthDate) { this.birthDate = birthDate; }
    public Boolean getProfileComplete() { return profileComplete; }
    public void setProfileComplete(Boolean profileComplete) { this.profileComplete = profileComplete; }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Boolean getEmailVerified() {
        return emailVerified;
    }

    public void setEmailVerified(Boolean emailVerified) {
        this.emailVerified = emailVerified;
    }

    public Set<String> getRoles() {
        return roles;
    }

    public void setRoles(Set<String> roles) {
        this.roles = roles;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
}

