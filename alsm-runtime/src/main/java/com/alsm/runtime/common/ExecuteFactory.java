package com.alsm.runtime.common;

public class ExecuteFactory {

	private final static ExecuteFactory INSTANCE = new ExecuteFactory();

	private ExecuteFactory() {
	}

	public static ExecuteFactory getInstance() {
		return INSTANCE;
	}

	public IExecuteRepository getRepository() {
		return new ExecuteRepository();
	}
}
