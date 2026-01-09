package com.ak4n1.turn_management.shared.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CookieConfig {

    @Value("${cookie.secure:false}")
    private boolean secure;

    @Value("${cookie.http-only:true}")
    private boolean httpOnly;

    @Value("${cookie.same-site:Lax}")
    private String sameSite;

    @Value("${cookie.domain:localhost}")
    private String domain;

    @Value("${cookie.path:/}")
    private String path;

    @Value("${cookie.max-age:604800}")
    private int maxAge;

    public boolean isSecure() {
        return secure;
    }

    public boolean isHttpOnly() {
        return httpOnly;
    }

    public String getSameSite() {
        return sameSite;
    }

    public String getDomain() {
        return domain;
    }

    public String getPath() {
        return path;
    }

    public int getMaxAge() {
        return maxAge;
    }
}

