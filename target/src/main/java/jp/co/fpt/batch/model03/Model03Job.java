package jp.co.fpt.batch.model03;

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
public class Model03Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model03" )
	public Job model03(Model03Tasklet model03Tasklet) {
		return jobs.get("Model03").incrementer(new RunIdIncrementer()).start(model03Step(model03Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model03Step(Tasklet tasklet) {
		return steps.get("Model03Step").tasklet(tasklet).build();
	}
}
