package com.ak4n1.turn_management.feature.configuration.service.configuration;

import com.ak4n1.turn_management.feature.configuration.domain.CalendarConfiguration;
import com.ak4n1.turn_management.feature.configuration.repository.CalendarConfigurationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
public class ConfigurationVersionServiceImpl implements ConfigurationVersionService {

    private static final Logger logger = LoggerFactory.getLogger(ConfigurationVersionServiceImpl.class);

    private final CalendarConfigurationRepository repository;

    public ConfigurationVersionServiceImpl(CalendarConfigurationRepository repository) {
        this.repository = repository;
    }

    @Override
    @Transactional(readOnly = true)
    public Integer calculateNextVersion() {
        Optional<Integer> maxVersion = repository.findMaxVersion();
        return maxVersion.map(v -> v + 1).orElse(1);
    }

    @Override
    @Transactional
    public void deactivatePreviousConfiguration() {
        Optional<CalendarConfiguration> activeConfig = repository.findByActiveTrue();
        if (activeConfig.isPresent()) {
            CalendarConfiguration previous = activeConfig.get();
            previous.setActive(false);
            repository.save(previous);
            logger.info("Configuración anterior desactivada - Versión: {}", previous.getVersion());
        }
    }
}
