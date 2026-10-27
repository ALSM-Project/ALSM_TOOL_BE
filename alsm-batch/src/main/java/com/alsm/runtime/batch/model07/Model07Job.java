package com.alsm.runtime.batch.model07;

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
public class Model07Job {

	@Autowired
	private JobRepository jobRepository;

	@Autowired
	private PlatformTransactionManager transactionManager;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model07" )
	public Job model07(Model07Tasklet model07Tasklet) {
		return new JobBuilder("Model07", jobRepository).incrementer(new RunIdIncrementer()).start(model07Step(model07Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model07Step(Tasklet tasklet) {
		return new StepBuilder("Model07Step", jobRepository).tasklet(tasklet, transactionManager).build();
	}
}
