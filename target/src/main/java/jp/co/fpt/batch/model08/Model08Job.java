package jp.co.fpt.batch.model08;

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
public class Model08Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model08" )
	public Job model08(Model08Tasklet model08Tasklet) {
		return jobs.get("Model08").incrementer(new RunIdIncrementer()).start(model08Step(model08Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model08Step(Tasklet tasklet) {
		return steps.get("Model08Step").tasklet(tasklet).build();
	}
}
