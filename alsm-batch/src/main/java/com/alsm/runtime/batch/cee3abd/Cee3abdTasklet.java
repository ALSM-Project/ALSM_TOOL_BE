package com.alsm.runtime.batch.cee3abd;

/**
 * Stand-in for the mainframe service CEE3ABD, which ends a program abnormally. Generated programs
 * call it with the abend code (and a timing value) they set just before.
 */
public class Cee3abdTasklet {

	public Object execute(Object abendCode) {
		throw new IllegalStateException("Program abended (CEE3ABD), abend code " + abendCode);
	}
}
