-- Script para insertar roles básicos (ROLE_USER y ROLE_ADMIN)
-- Ejecutar después de vaciar tablas o en base limpia

INSERT INTO roles (name, description, created_at)
VALUES
    ('ROLE_USER', 'Rol de usuario estándar', CURRENT_TIMESTAMP),
    ('ROLE_ADMIN', 'Rol de administrador', CURRENT_TIMESTAMP);
