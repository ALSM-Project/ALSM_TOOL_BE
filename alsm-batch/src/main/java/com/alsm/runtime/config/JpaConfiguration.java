package com.alsm.runtime.config;

import javax.sql.DataSource;

import org.springframework.batch.core.configuration.annotation.DefaultBatchConfigurer;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.transaction.PlatformTransactionManager;

@Configuration
@Profile("!nojpa")
@EnableBatchProcessing
public class JpaConfiguration extends DefaultBatchConfigurer {

	@Autowired
	PlatformTransactionManager jpaTransactionManager;

	@Override
	public PlatformTransactionManager getTransactionManager() {
		return jpaTransactionManager;
	}

	@Override
	public void setDataSource(DataSource dataSource) {
	}

}
