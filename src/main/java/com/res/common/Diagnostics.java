package com.res.common;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Errors the parser and the symbol table report while a program is translated. They are printed and
 * translation goes on, so they never reach the caller as an exception; the project driver reads them
 * from here to say why a program failed and on which line.
 */
public final class Diagnostics {

	/** One reported problem. */
	public static final class Entry {
		public final String message;
		public final int line;

		Entry(String message, int line) {
			this.message = message;
			this.line = line;
		}
	}

	private static final ThreadLocal<List<Entry>> ENTRIES = new ThreadLocal<List<Entry>>() {
		@Override
		protected List<Entry> initialValue() {
			return new ArrayList<Entry>();
		}
	};

	private Diagnostics() {
	}

	public static void clear() {
		ENTRIES.get().clear();
	}

	public static void add(String message, int line) {
		ENTRIES.get().add(new Entry(firstLine(message), line));
	}

	public static List<Entry> entries() {
		return Collections.unmodifiableList(new ArrayList<Entry>(ENTRIES.get()));
	}

	private static String firstLine(String message) {
		if (message == null)
			return "";
		int newline = message.indexOf('\n');
		return (newline < 0 ? message : message.substring(0, newline)).trim();
	}
}
