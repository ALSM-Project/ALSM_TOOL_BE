package com.res.common;

/**
 * A translation failure that belongs to one COBOL program. It replaces the System.exit() calls the
 * translator used to make: an exit stopped the whole run, so one unreadable program in a project
 * took every other program down with it. The project driver catches this per program, records it
 * in the report and carries on.
 */
public class CobolTranslationException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	private final String sourceFile;
	private final int line;

	public CobolTranslationException(String message) {
		this(message, null, -1, null);
	}

	public CobolTranslationException(String message, String sourceFile, int line) {
		this(message, sourceFile, line, null);
	}

	public CobolTranslationException(String message, String sourceFile, int line, Throwable cause) {
		super(message, cause);
		this.sourceFile = sourceFile;
		this.line = line;
	}

	/** COBOL source file the error was found in, or null when it is not known. */
	public String getSourceFile() {
		return sourceFile;
	}

	/** Line in that file, or -1 when it is not known. */
	public int getLine() {
		return line;
	}
}
