package jp.co.alsm.config;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

/** Listener the generated jobs register; it does nothing by default - add job-level hooks here. */
@Primary
@Component
@JobScope
public class BaseJobExecutionListener implements JobExecutionListener {

  @Override
  public void beforeJob(JobExecution jobExecution) {
  }

  @Override
  public void afterJob(JobExecution jobExecution) {
  }
}
