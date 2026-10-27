package com.alsm.runtime.batch.model07;

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
public class Model07Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model07" )
	public Job model07(Model07Tasklet model07Tasklet) {
		return jobs.get("Model07").incrementer(new RunIdIncrementer()).start(model07Step(model07Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model07Step(Tasklet tasklet) {
		return steps.get("Model07Step").tasklet(tasklet).build();
	}
}
