package com.ak4n1.turn_management.feature.notification.service.template;

public interface EmailTemplate {

    String getSubject();

    String getBody();

    String getTo();

    String getFrom();
}

