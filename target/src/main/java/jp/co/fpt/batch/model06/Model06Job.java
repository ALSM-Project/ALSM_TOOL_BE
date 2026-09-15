package jp.co.fpt.batch.model06;

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
public class Model06Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model06" )
	public Job model06(Model06Tasklet model06Tasklet) {
		return jobs.get("Model06").incrementer(new RunIdIncrementer()).start(model06Step(model06Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model06Step(Tasklet tasklet) {
		return steps.get("Model06Step").tasklet(tasklet).build();
	}
}
