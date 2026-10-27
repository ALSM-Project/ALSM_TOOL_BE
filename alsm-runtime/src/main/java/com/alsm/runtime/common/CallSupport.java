package com.alsm.runtime.common;

import java.lang.reflect.Method;

/**
 * Helper for generated COBOL CALL statements. The called program receives its arguments by
 * reference in COBOL, so what it returns is written back into the argument. A stand-alone
 * numeric variable is a plain Java value here and is passed by value, so nothing is written.
 */
public final class CallSupport {

	private CallSupport() {
	}

	public static void writeBack(Object argument, Object result) throws Exception {
		if (argument == null || result == null) {
			return;
		}
		String value = result.toString();
		if (argument instanceof Record) {
			((Record) argument).set(value, 0, ((Record) argument).length());
			return;
		}
		try {
			Method write = argument.getClass().getMethod("write", String.class);
			write.invoke(argument, value);
		} catch (NoSuchMethodException e) {
			// Passed by value: nothing to write back.
		}
	}
}
