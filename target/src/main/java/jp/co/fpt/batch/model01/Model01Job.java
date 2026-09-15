package jp.co.fpt.batch.model01;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import jp.co.fpt.config.BaseJobExecutionListener;

@Configuration 
public class Model01Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model01" )
	public Job model01(Model01Tasklet model01Tasklet) {
		return jobs.get("Model01").incrementer(new RunIdIncrementer()).start(model01Step(model01Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model01Step(Tasklet tasklet) {
		return steps.get("Model01Step").tasklet(tasklet).build();
	}
}
