package com.ak4n1.turn_management;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
@EnableCaching
public class TurnManagementApplication {

	public static void main(String[] args) {
		SpringApplication.run(TurnManagementApplication.class, args);
	}

}
