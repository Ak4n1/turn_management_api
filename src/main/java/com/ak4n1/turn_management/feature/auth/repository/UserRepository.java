package com.ak4n1.turn_management.feature.auth.repository;

import com.ak4n1.turn_management.feature.auth.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    Optional<User> findByEmail(String email);

    boolean existsByEmail(String email);

    @Query("SELECT u FROM User u LEFT JOIN FETCH u.roles WHERE u.email = :email")
    Optional<User> findByEmailWithRoles(@Param("email") String email);

    @Query("SELECT u FROM User u LEFT JOIN FETCH u.roles WHERE u.id = :id")
    Optional<User> findByIdWithRoles(@Param("id") Long id);

    /**
     * Busca usuarios por email o nombre (parcial, case-insensitive).
     * 
     * @param searchTerm Término de búsqueda
     * @return Lista de IDs de usuarios que coinciden
     */
    @Query("SELECT u.id FROM User u WHERE " +
           "LOWER(u.email) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "OR LOWER(u.firstName) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "OR LOWER(u.lastName) LIKE LOWER(CONCAT('%', :searchTerm, '%'))")
    List<Long> findUserIdsBySearchTerm(@Param("searchTerm") String searchTerm);

    /**
     * Encuentra todos los usuarios que tienen el rol ADMIN.
     * 
     * @return Lista de usuarios con rol ADMIN
     */
    @Query("SELECT DISTINCT u FROM User u JOIN u.roles r WHERE r.name = 'ADMIN'")
    List<User> findAllAdmins();

    /**
     * Encuentra todos los usuarios habilitados (enabled = true).
     * 
     * @return Lista de usuarios habilitados
     */
    @Query("SELECT u FROM User u WHERE u.enabled = true")
    List<User> findAllEnabled();
}

