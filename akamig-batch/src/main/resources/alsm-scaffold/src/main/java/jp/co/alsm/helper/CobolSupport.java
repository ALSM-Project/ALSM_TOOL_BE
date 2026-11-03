package jp.co.alsm.helper;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.Map;

/**
 * Helpers the converted code calls by their COBOL-runtime names (__delimitedBy, __justifyRight ...).
 * They used to be methods of the converter's own base classes; the Tasklet imports them statically.
 */
public final class CobolSupport {

	private static final Map<String, Object> ENVIRONMENT = new HashMap<String, Object>();
	private static BufferedReader sysin;

	private CobolSupport() {
	}

	/** ALL literal: the literal repeated to the given length. */
	public static String __all(String value, int length) {
		if (value.isEmpty())
			return value;
		StringBuilder out = new StringBuilder();
		while (out.length() < length)
			out.append(value);
		return out.substring(0, length);
	}

	public static String __all(long value, int length) {
		return __all(String.valueOf(value), length);
	}

	/** JUSTIFIED RIGHT: right-aligned in the field, cut on the left when too long. */
	public static String __justifyRight(String value, int length) {
		String text = value == null ? "" : value;
		String trimmed = text.replaceAll("\\s+$", "");
		if (trimmed.length() >= length)
			return trimmed.substring(trimmed.length() - length);
		StringBuilder out = new StringBuilder();
		for (int i = trimmed.length(); i < length; i++)
			out.append(' ');
		return out.append(trimmed).toString();
	}

	public static String __justifyRight(int value, int length) {
		return __justifyRight(String.valueOf(value), length);
	}

	public static String __justifyRight(long value, int length) {
		return __justifyRight(String.valueOf(value), length);
	}

	/** STRING ... DELIMITED BY x: the part of the source before the first x. */
	public static String __delimitedBy(String source, String delimiter) {
		if (source == null)
			return "";
		int at = delimiter == null || delimiter.isEmpty() ? -1 : source.indexOf(delimiter);
		return at >= 0 ? source.substring(0, at) : source;
	}

	public static String __delimitedBy(String source, char delimiter) {
		return __delimitedBy(source, String.valueOf(delimiter));
	}

	public static String __delimitedBy(String source, long delimiter) {
		return __delimitedBy(source, String.valueOf(delimiter));
	}

	/** DISPLAY ... UPON name. */
	public static void __putEnvironment(String name, Object value) {
		ENVIRONMENT.put(name.toUpperCase(), value);
	}

	/**
	 * ACCEPT ... FROM name: SYSIN reads the next line of standard input, ENVIRONMENT-VALUE the
	 * variable named by an earlier DISPLAY ... UPON ENVIRONMENT-NAME.
	 */
	public static String __getEnvironment(String name) {
		String key = name.toUpperCase();
		if (key.equals("SYSIN") || key.equals("CONSOLE")) {
			try {
				if (sysin == null)
					sysin = new BufferedReader(new InputStreamReader(System.in));
				if (!sysin.ready())
					return "";
				String line = sysin.readLine();
				return line == null ? "" : line;
			} catch (IOException e) {
				return "";
			}
		}
		if (key.equals("ENVIRONMENT-VALUE")) {
			Object variable = ENVIRONMENT.get("ENVIRONMENT-NAME");
			String value = variable == null ? null : System.getenv(variable.toString().trim());
			return value == null ? "" : value;
		}
		Object value = ENVIRONMENT.get(key);
		return value == null ? "" : value.toString();
	}

	public static String __normalizeLiteral(String literal) {
		return literal;
	}

	public static BigDecimal __round(BigDecimal value, int scale) {
		return value.setScale(scale, RoundingMode.HALF_UP);
	}

	public static BigDecimal __round(double value, int scale) {
		return BigDecimal.valueOf(value).setScale(scale, RoundingMode.HALF_UP);
	}

	public static long __round(long value, int scale) {
		return value;
	}
}
