package com.alsm.runtime.config;

import org.springframework.batch.core.configuration.annotation.DefaultBatchConfigurer;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("nojpa")
@EnableBatchProcessing
public class NoJpaConfiguration extends DefaultBatchConfigurer {
}
