package com.ak4n1.turn_management.shared.security.jwt;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtTokenProvider jwtTokenProvider;
    private final UserDetailsService userDetailsService;

    public JwtAuthenticationFilter(JwtTokenProvider jwtTokenProvider, UserDetailsService userDetailsService) {
        this.jwtTokenProvider = jwtTokenProvider;
        this.userDetailsService = userDetailsService;
    }

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {
        // Try to get token from cookie first, then fallback to Authorization header
        String jwt = getAccessTokenFromCookie(request);
        
        if (jwt == null) {
            // Fallback to Authorization header for backward compatibility
            final String authHeader = request.getHeader("Authorization");
            if (authHeader != null && authHeader.startsWith("Bearer ")) {
                jwt = authHeader.substring(7);
            }
        }

        if (jwt == null) {
            filterChain.doFilter(request, response);
            return;
        }

        final Long userId;
        try {
            userId = jwtTokenProvider.extractUserId(jwt);
        } catch (Exception e) {
            filterChain.doFilter(request, response);
            return;
        }

        if (userId != null && SecurityContextHolder.getContext().getAuthentication() == null) {
            try {
                // Load user by ID instead of email
                if (userDetailsService instanceof com.ak4n1.turn_management.feature.auth.service.CustomUserDetailsService) {
                    com.ak4n1.turn_management.feature.auth.service.CustomUserDetailsService customService = 
                        (com.ak4n1.turn_management.feature.auth.service.CustomUserDetailsService) userDetailsService;
                    
                    // Cargar el usuario completo para verificar emailVerified
                    com.ak4n1.turn_management.feature.auth.domain.User user = customService.loadUserEntityById(userId);
                    
                    // Verificar que el email esté verificado antes de autenticar
                    // Solo verificar para endpoints protegidos (no /api/auth/**)
                    String requestPath = request.getRequestURI();
                    boolean isPublicEndpoint = requestPath.startsWith("/api/auth/");
                    
                    if (!isPublicEndpoint && !user.getEmailVerified()) {
                        // Email no verificado, rechazar la autenticación para endpoints protegidos
                        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                        response.getWriter().write("{\"status\":403,\"error\":\"Forbidden\",\"message\":\"El email no está verificado. Por favor, verifica tu email antes de acceder.\",\"path\":\"" + requestPath + "\"}");
                        response.setContentType("application/json");
                        return;
                    }
                    
                    UserDetails userDetails = customService.loadUserById(userId);
                    if (jwtTokenProvider.validateToken(jwt, userId)) {
                        UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
                                userDetails,
                                null,
                                userDetails.getAuthorities()
                        );
                        authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                        SecurityContextHolder.getContext().setAuthentication(authToken);
                    }
                }
            } catch (org.springframework.security.core.userdetails.UsernameNotFoundException e) {
                // Usuario no existe (token de sesión anterior, usuario eliminado, etc.)
                // Simplemente continuamos sin autenticar - no es un error crítico
                // El token será ignorado y la request continuará normalmente
            } catch (Exception e) {
                // Cualquier otra excepción también se ignora para no bloquear la request
                // El token será tratado como inválido
            }
        }
        filterChain.doFilter(request, response);
    }

    private String getAccessTokenFromCookie(HttpServletRequest request) {
        jakarta.servlet.http.Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (jakarta.servlet.http.Cookie cookie : cookies) {
                if ("accessToken".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }
        return null;
    }
}

