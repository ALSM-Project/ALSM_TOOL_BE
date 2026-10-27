package com.alsm.runtime.config;

import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

/** Spring Batch keeps its job repository in the application's own database and uses the JPA transaction manager. */
@Configuration
@Profile("!nojpa")
@EnableBatchProcessing(transactionManagerRef = "jpaTransactionManager")
public class JpaConfiguration {
}
