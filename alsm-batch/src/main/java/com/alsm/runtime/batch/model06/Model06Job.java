package com.alsm.runtime.batch.model06;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import com.alsm.runtime.config.BaseJobExecutionListener;

@Configuration 
public class Model06Job {

	@Autowired
	private JobRepository jobRepository;

	@Autowired
	private PlatformTransactionManager transactionManager;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model06" )
	public Job model06(Model06Tasklet model06Tasklet) {
		return new JobBuilder("Model06", jobRepository).incrementer(new RunIdIncrementer()).start(model06Step(model06Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model06Step(Tasklet tasklet) {
		return new StepBuilder("Model06Step", jobRepository).tasklet(tasklet, transactionManager).build();
	}
}
