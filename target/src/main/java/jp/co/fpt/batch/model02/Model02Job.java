package jp.co.fpt.batch.model02;

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
public class Model02Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model02" )
	public Job model02(Model02Tasklet model02Tasklet) {
		return jobs.get("Model02").incrementer(new RunIdIncrementer()).start(model02Step(model02Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model02Step(Tasklet tasklet) {
		return steps.get("Model02Step").tasklet(tasklet).build();
	}
}
