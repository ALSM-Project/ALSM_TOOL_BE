package com.alsm.runtime.config;

import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

@Component
public final class ApplicationContextProvider implements ApplicationContextAware {

	private static final ApplicationContextProvider INSTANCE = new ApplicationContextProvider();
	private static ApplicationContext applicationContext;

	private ApplicationContextProvider() {
	}

	public static ApplicationContextProvider getInstance() {
		return INSTANCE;
	}

	public static void autowire(Object clazz) {
		ApplicationContextProvider.applicationContext.getAutowireCapableBeanFactory().autowireBean(clazz);
	}

	@Override
	public void setApplicationContext(final ApplicationContext applicationContext) {
		ApplicationContextProvider.applicationContext = applicationContext;
	}

	public ApplicationContext getApplicationContext() {
		return ApplicationContextProvider.applicationContext;
	}

	public static <T> T getBean(Class<T> beanClass) {
		return applicationContext.getBean(beanClass);
	}

	public static <T> T getBean(String beanName, Class<T> beanClass) {
		return applicationContext.getBean(beanName, beanClass);
	}

	public static <T> T getBeanNotRequired(Class<T> beanClass) {
		try {
			return applicationContext.getBean(beanClass);
		} catch (NoSuchBeanDefinitionException e) {
			return null;
		}
	}

	public static <T> T getBeanNotRequired(String beanName, Class<T> beanClass) {
		try {
			return applicationContext.getBean(beanName, beanClass);
		} catch (NoSuchBeanDefinitionException e) {
			return null;
		}
	}
}