package com.alsm.runtime.config;

import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
public class ApplicationProfile {

	private static Environment environment;

	public ApplicationProfile(Environment environment) {
		ApplicationProfile.environment = environment;
	}

	public static Environment getEnvironment() throws Exception {
		if (environment == null) {
			throw new Exception();
		}
		return environment;
	}
}