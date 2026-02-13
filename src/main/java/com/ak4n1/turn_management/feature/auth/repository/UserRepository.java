package com.ak4n1.turn_management.feature.auth.repository;

import com.ak4n1.turn_management.feature.auth.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
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
     * Encuentra todos los usuarios que tienen el rol ADMIN (nombre en BD: ROLE_ADMIN).
     * 
     * @return Lista de usuarios con rol ROLE_ADMIN
     */
    @Query("SELECT DISTINCT u FROM User u JOIN u.roles r WHERE r.name = 'ROLE_ADMIN'")
    List<User> findAllAdmins();

    /**
     * Encuentra todos los usuarios habilitados (enabled = true).
     * 
     * @return Lista de usuarios habilitados
     */
    @Query("SELECT u FROM User u WHERE u.enabled = true")
    List<User> findAllEnabled();

    /**
     * Busca usuarios con filtros para administrador (listado paginado).
     *
     * @param search Búsqueda por email, nombre, apellido (opcional)
     * @param createdFrom Fecha creación desde (opcional)
     * @param createdTo Fecha creación hasta (opcional)
     * @param emailVerified null=all, true=verificados, false=no verificados
     * @param profileComplete null=all, true=completos, false=incompletos
     * @param roleFilter null=all, ROLE_ADMIN, ROLE_USER
     * @param enabled null=all, true=activos, false=deshabilitados
     * @param pageable Paginación
     */
    @Query(value = "SELECT * FROM users u WHERE " +
           "(:search IS NULL OR :search = '' OR LOWER(u.email) LIKE LOWER(CONCAT('%', :search, '%')) " +
           "       OR LOWER(u.first_name) LIKE LOWER(CONCAT('%', :search, '%')) " +
           "       OR LOWER(u.last_name) LIKE LOWER(CONCAT('%', :search, '%'))) " +
           "AND (:createdFrom IS NULL OR u.created_at >= :createdFrom) " +
           "AND (:createdTo IS NULL OR u.created_at <= :createdTo) " +
           "AND (:emailVerified IS NULL OR u.email_verified = :emailVerified) " +
           "AND (:profileComplete IS NULL OR (" +
           "  (u.phone IS NOT NULL AND TRIM(u.phone) != '') AND (u.street IS NOT NULL AND TRIM(u.street) != '') " +
           "  AND (u.street_number IS NOT NULL AND TRIM(u.street_number) != '') AND (u.floor_apt IS NOT NULL AND TRIM(u.floor_apt) != '') " +
           "  AND (u.city IS NOT NULL AND TRIM(u.city) != '') AND (u.postal_code IS NOT NULL AND TRIM(u.postal_code) != '') " +
           "  AND u.birth_date IS NOT NULL) = :profileComplete) " +
           "AND (:roleFilter IS NULL OR :roleFilter = '' OR u.id IN " +
           "  (SELECT ur2.user_id FROM user_roles ur2 JOIN roles r2 ON ur2.role_id = r2.id WHERE r2.name = :roleFilter)) " +
           "AND (:enabled IS NULL OR u.enabled = :enabled) " +
           "ORDER BY u.created_at DESC",
           countQuery = "SELECT COUNT(*) FROM users u WHERE " +
           "(:search IS NULL OR :search = '' OR LOWER(u.email) LIKE LOWER(CONCAT('%', :search, '%')) " +
           "       OR LOWER(u.first_name) LIKE LOWER(CONCAT('%', :search, '%')) " +
           "       OR LOWER(u.last_name) LIKE LOWER(CONCAT('%', :search, '%'))) " +
           "AND (:createdFrom IS NULL OR u.created_at >= :createdFrom) " +
           "AND (:createdTo IS NULL OR u.created_at <= :createdTo) " +
           "AND (:emailVerified IS NULL OR u.email_verified = :emailVerified) " +
           "AND (:profileComplete IS NULL OR (" +
           "  (u.phone IS NOT NULL AND TRIM(u.phone) != '') AND (u.street IS NOT NULL AND TRIM(u.street) != '') " +
           "  AND (u.street_number IS NOT NULL AND TRIM(u.street_number) != '') AND (u.floor_apt IS NOT NULL AND TRIM(u.floor_apt) != '') " +
           "  AND (u.city IS NOT NULL AND TRIM(u.city) != '') AND (u.postal_code IS NOT NULL AND TRIM(u.postal_code) != '') " +
           "  AND u.birth_date IS NOT NULL) = :profileComplete) " +
           "AND (:roleFilter IS NULL OR :roleFilter = '' OR u.id IN " +
           "  (SELECT ur2.user_id FROM user_roles ur2 JOIN roles r2 ON ur2.role_id = r2.id WHERE r2.name = :roleFilter)) " +
           "AND (:enabled IS NULL OR u.enabled = :enabled)",
           nativeQuery = true)
    Page<User> findAllWithFilters(
            @Param("search") String search,
            @Param("createdFrom") LocalDateTime createdFrom,
            @Param("createdTo") LocalDateTime createdTo,
            @Param("emailVerified") Boolean emailVerified,
            @Param("profileComplete") Boolean profileComplete,
            @Param("roleFilter") String roleFilter,
            @Param("enabled") Boolean enabled,
            Pageable pageable);
}

