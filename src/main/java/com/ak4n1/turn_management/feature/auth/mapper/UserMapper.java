package com.ak4n1.turn_management.feature.auth.mapper;

import com.ak4n1.turn_management.feature.auth.domain.Role;
import com.ak4n1.turn_management.feature.auth.domain.User;
import com.ak4n1.turn_management.feature.auth.dto.response.UserResponse;
import org.springframework.stereotype.Component;

import java.util.stream.Collectors;

@Component
public class UserMapper {

    public UserResponse toUserResponse(User user) {
        UserResponse response = new UserResponse();
        response.setId(user.getId());
        response.setEmail(user.getEmail());
        response.setFirstName(user.getFirstName());
        response.setLastName(user.getLastName());
        response.setPhone(user.getPhone());
        response.setStreet(user.getStreet());
        response.setStreetNumber(user.getStreetNumber());
        response.setFloorApt(user.getFloorApt());
        response.setCity(user.getCity());
        response.setPostalCode(user.getPostalCode());
        response.setBirthDate(user.getBirthDate());
        response.setProfileComplete(user.isProfileComplete());
        response.setEnabled(user.getEnabled());
        response.setEmailVerified(user.getEmailVerified());
        response.setRoles(user.getRoles().stream()
                .map(Role::getName)
                .collect(Collectors.toSet()));
        response.setCreatedAt(user.getCreatedAt());
        return response;
    }
}

