package com.alsm.runtime.config;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

import com.alsm.runtime.logging.Logger;

@Primary
@Component
@JobScope
public class BaseJobExecutionListener implements JobExecutionListener {

	protected Logger logger;

	public BaseJobExecutionListener() {
		this.logger = new Logger(getClass());
	}

	@Override
	public void beforeJob(JobExecution jobExecution) {
	}

	@Override
	public void afterJob(JobExecution jobExecution) {
	}
}
