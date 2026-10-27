package com.alsm.runtime;

import java.util.Properties;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.converter.DefaultJobParametersConverter;
import org.springframework.batch.core.converter.JobParametersConverter;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.alsm.runtime.common.Constant;
import com.alsm.runtime.config.ApplicationContextProvider;

@SpringBootApplication(exclude = { DataSourceAutoConfiguration.class })
public class BatchApplication implements CommandLineRunner {

	@Autowired
	private JobLauncher jobLauncher;

	public static void main(String[] args) {
		SpringApplication.run(BatchApplication.class, args);
	}

	@Override
	public void run(String... args) throws Exception {
		if (args != null && args.length != 0) {
			JobParametersConverter converter = new DefaultJobParametersConverter();
			JobParameters jobParameters = converter.getJobParameters(splitArrayElementsIntoProperties(args, Constant.EQUAL));
			Job job = ApplicationContextProvider.getBean(args[0], Job.class);
			jobLauncher.run(job, jobParameters);
		}
	}

	private static Properties splitArrayElementsIntoProperties(
			String[] array, String delimiter) {

		if (ObjectUtils.isEmpty(array)) {
			return null;
		}

		Properties result = new Properties();
		for (String element : array) {
			String[] splittedElement = StringUtils.split(element, delimiter);
			if (splittedElement == null) {
				continue;
			}
			result.setProperty(splittedElement[0].trim(), splittedElement[1]);
		}
		return result;
	}
	
}
