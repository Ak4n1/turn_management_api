package com.ak4n1.turn_management.feature.auth.dto.request;

import com.ak4n1.turn_management.feature.auth.validation.ValidPhone;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;

/**
 * Request para actualizar el perfil del usuario autenticado.
 * Todos los campos son opcionales (actualización parcial).
 */
public class UpdateProfileRequest {

    @Size(max = 50)
    private String firstName;

    @Size(max = 50)
    private String lastName;

    @Size(max = 30)
    @ValidPhone
    private String phone;

    @Size(max = 100)
    private String street;

    @Size(max = 20)
    private String streetNumber;

    @Size(max = 20)
    private String floorApt;

    @Size(max = 80)
    private String city;

    @Size(max = 20)
    private String postalCode;

    private LocalDate birthDate;

    public UpdateProfileRequest() {
    }

    public String getFirstName() { return firstName; }
    public void setFirstName(String firstName) { this.firstName = firstName; }
    public String getLastName() { return lastName; }
    public void setLastName(String lastName) { this.lastName = lastName; }
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
}
