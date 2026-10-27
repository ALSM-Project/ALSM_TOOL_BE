package com.alsm.runtime.batch.model09;

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
public class Model09Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model09" )
	public Job model09(Model09Tasklet model09Tasklet) {
		return jobs.get("Model09").incrementer(new RunIdIncrementer()).start(model09Step(model09Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model09Step(Tasklet tasklet) {
		return steps.get("Model09Step").tasklet(tasklet).build();
	}
}
