package com.alsm.runtime.batch.model04;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.alsm.runtime.config.BaseJobExecutionListener;

@Configuration 
public class Model04Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model04" )
	public Job model04(Model04Tasklet model04Tasklet) {
		return jobs.get("Model04").incrementer(new RunIdIncrementer()).start(model04Step(model04Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model04Step(Tasklet tasklet) {
		return steps.get("Model04Step").tasklet(tasklet).build();
	}
}
