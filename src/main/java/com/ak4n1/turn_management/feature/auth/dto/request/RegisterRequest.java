package com.ak4n1.turn_management.feature.auth.dto.request;

import com.ak4n1.turn_management.feature.auth.validation.ValidPhone;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

public class RegisterRequest {

    @NotBlank(message = "Email is required")
    @Email(message = "Email must be valid")
    @Size(max = 100, message = "Email must not exceed 100 characters")
    private String email;

    @NotBlank(message = "Password is required")
    @Size(min = 8, message = "Password must be at least 8 characters")
    @Pattern(
        regexp = "^(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,}$",
        message = "Password must contain at least one uppercase letter, one number, and one special character (@$!%*?&)"
    )
    private String password;

    @NotBlank(message = "First name is required")
    @Size(max = 50, message = "First name must not exceed 50 characters")
    private String firstName;

    @NotBlank(message = "Last name is required")
    @Size(max = 50, message = "Last name must not exceed 50 characters")
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
    private java.time.LocalDate birthDate;

    public RegisterRequest() {
    }

    public RegisterRequest(String email, String password) {
        this.email = email;
        this.password = password;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
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
    public java.time.LocalDate getBirthDate() { return birthDate; }
    public void setBirthDate(java.time.LocalDate birthDate) { this.birthDate = birthDate; }
}

