package jp.co.alsm.helper;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;

/**
 * COBOL intrinsic functions (FUNCTION MOD, CURRENT-DATE, UPPER-CASE ...) the converter calls as
 * CobolFunction.name(arguments).
 */
public final class CobolFunction {

	private static final Random RANDOM = new Random();

	private CobolFunction() {
	}

	/** FUNCTION CURRENT-DATE: YYYYMMDDHHMMSShh followed by the offset from GMT (+hhmm). */
	public static String currentDate() {
		Date now = new Date();
		String stamp = new SimpleDateFormat("yyyyMMddHHmmss").format(now);
		String hundredths = String.format("%02d", (now.getTime() % 1000) / 10);
		String offset = new SimpleDateFormat("Z").format(now);
		return stamp + hundredths + offset;
	}

	public static String whenCompiled() {
		return currentDate();
	}

	/** FUNCTION MOD: the result takes the sign of the divisor, unlike Java's %. */
	public static long mod(long a, long b) {
		return Math.floorMod(a, b);
	}

	public static int mod(int a, int b) {
		return Math.floorMod(a, b);
	}

	public static double rem(double a, double b) {
		return a - b * (long) (a / b);
	}

	public static int length(String value) {
		return value.length();
	}

	public static int length(char value) {
		return 1;
	}

	public static String upperCase(String value) {
		return value.toUpperCase();
	}

	public static String lowerCase(String value) {
		return value.toLowerCase();
	}

	public static String reverse(String value) {
		return new StringBuilder(value).reverse().toString();
	}

	public static double numVal(String value) {
		try {
			return new BigDecimal(value.trim()).doubleValue();
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	public static int integer(double value) {
		return (int) Math.floor(value);
	}

	public static int integerPart(double value) {
		return (int) value;
	}

	public static double sqrt(double value) {
		return Math.sqrt(value);
	}

	public static double log(double value) {
		return Math.log(value);
	}

	public static double log10(double value) {
		return Math.log10(value);
	}

	public static double sin(double value) {
		return Math.sin(value);
	}

	public static double cos(double value) {
		return Math.cos(value);
	}

	public static double tan(double value) {
		return Math.tan(value);
	}

	public static double aSin(double value) {
		return Math.asin(value);
	}

	public static double aCos(double value) {
		return Math.acos(value);
	}

	public static double aTan(double value) {
		return Math.atan(value);
	}

	public static double random() {
		return RANDOM.nextDouble();
	}

	public static double random(double seed) {
		return new Random((long) seed).nextDouble();
	}

	public static double max(double... values) {
		double result = values[0];
		for (double v : values) {
			result = Math.max(result, v);
		}
		return result;
	}

	public static double min(double... values) {
		double result = values[0];
		for (double v : values) {
			result = Math.min(result, v);
		}
		return result;
	}

	public static double sum(double... values) {
		double result = 0;
		for (double v : values) {
			result += v;
		}
		return result;
	}

	public static double mean(double... values) {
		return sum(values) / values.length;
	}

	public static double range(double... values) {
		return max(values) - min(values);
	}

	public static double midrange(double... values) {
		return (max(values) + min(values)) / 2;
	}

	public static long factorial(long n) {
		long result = 1;
		for (long i = 2; i <= n; i++) {
			result *= i;
		}
		return result;
	}

	/** FUNCTION INTEGER-OF-DATE: days since 1600-12-31 for a YYYYMMDD date. */
	public static long integerOfDate(long yyyymmdd) {
		java.time.LocalDate date = java.time.LocalDate.of((int) (yyyymmdd / 10000), (int) (yyyymmdd / 100 % 100),
				(int) (yyyymmdd % 100));
		return java.time.temporal.ChronoUnit.DAYS.between(java.time.LocalDate.of(1600, 12, 31), date);
	}

	public static long dateOfInteger(long days) {
		java.time.LocalDate date = java.time.LocalDate.of(1600, 12, 31).plusDays(days);
		return date.getYear() * 10000L + date.getMonthValue() * 100L + date.getDayOfMonth();
	}

	/** FUNCTION NUMVAL-C: a number written with currency sign, thousands separators, CR or DB. */
	public static double numValC(String value) {
		String text = value.trim().toUpperCase();
		boolean negative = text.endsWith("CR") || text.endsWith("DB") || text.startsWith("-") || text.endsWith("-");
		text = text.replaceAll("CR$|DB$", "").replaceAll("[^0-9.]", "");
		if (text.isEmpty())
			return 0;
		try {
			double number = new BigDecimal(text).doubleValue();
			return negative ? -number : number;
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	public static double numValC(String value, String currency) {
		return numValC(value.replace(currency, ""));
	}

	/** HIGH-VALUES as text, long enough to fill any field it is moved to. */
	public static String highValues() {
		return fill((char) 0xFF);
	}

	/** LOW-VALUES as text, long enough to fill any field it is moved to. */
	public static String lowValues() {
		return fill((char) 0);
	}

	private static String fill(char c) {
		char[] chars = new char[256];
		java.util.Arrays.fill(chars, c);
		return new String(chars);
	}
}
