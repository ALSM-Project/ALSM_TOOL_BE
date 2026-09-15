package jp.co.fpt.batch.model05;

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
public class Model05Job {

	@Autowired
	private JobBuilderFactory jobs;

	@Autowired
	private StepBuilderFactory steps;

	@Autowired
	private BaseJobExecutionListener jobListener;

	@Bean(name ="Model05" )
	public Job model05(Model05Tasklet model05Tasklet) {
		return jobs.get("Model05").incrementer(new RunIdIncrementer()).start(model05Step(model05Tasklet))
				.listener(jobListener).build();
	}

	@Bean
	public Step model05Step(Tasklet tasklet) {
		return steps.get("Model05Step").tasklet(tasklet).build();
	}
}
