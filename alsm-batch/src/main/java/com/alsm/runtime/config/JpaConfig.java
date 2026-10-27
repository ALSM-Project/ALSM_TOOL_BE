package com.alsm.runtime.config;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Profile;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.support.TransactionTemplate;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import com.alsm.runtime.helper.PropertiesHelper;

@Configuration
@DependsOn("applicationProfile")
@Profile("!nojpa")
@EnableTransactionManagement
public class JpaConfig {

	@Bean
	public DataSource dataSource() {
		PropertiesHelper properties = PropertiesHelper.getInstance("config/batch");

		HikariConfig hikariConfig = new HikariConfig();
		hikariConfig.setJdbcUrl(properties.getProperty("JpaConfig.url"));
		hikariConfig.setDriverClassName(properties.getProperty("JpaConfig.driver"));
		hikariConfig.setUsername(properties.getProperty("JpaConfig.userId"));
		hikariConfig.setPassword(properties.getProperty("JpaConfig.password"));
		hikariConfig.setAutoCommit(false);
		hikariConfig.setMaximumPoolSize(Integer.valueOf(properties.getProperty("JpaConfig.maximumPoolSize")));
		hikariConfig.setMinimumIdle(Integer.valueOf(properties.getProperty("JpaConfig.minimumIdle")));
		hikariConfig.setConnectionTimeout(Long.valueOf(properties.getProperty("JpaConfig.connectionTimeout")));

		return new HikariDataSource(hikariConfig);
	}

	@Bean
	public PlatformTransactionManager jpaTransactionManager(EntityManagerFactory emf) {
		return new JpaTransactionManager(emf);
	}

	@Bean
	public TransactionTemplate transactionTemplate(PlatformTransactionManager jpaTransactionManager) {
		PropertiesHelper properties = PropertiesHelper.getInstance("config/batch");
		TransactionTemplate transactionTemplate = new TransactionTemplate(jpaTransactionManager);
		transactionTemplate.setIsolationLevel(getTransactionLevel(properties.getProperty("JpaConfig.transaction")));
		return transactionTemplate;
	}

	private int getTransactionLevel(String transaction) {
		String level = transaction.toLowerCase();
		switch (level) {
		case "none":
			return TransactionDefinition.ISOLATION_DEFAULT;
		case "read-committed":
			return TransactionDefinition.ISOLATION_READ_COMMITTED;
		case "read-uncommitted":
			return TransactionDefinition.ISOLATION_READ_UNCOMMITTED;
		case "repeatable-read":
			return TransactionDefinition.ISOLATION_REPEATABLE_READ;
		case "serializable":
			return TransactionDefinition.ISOLATION_SERIALIZABLE;
		default:
			return TransactionDefinition.ISOLATION_DEFAULT;
		}
	}
}
