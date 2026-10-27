package com.alsm.runtime.helper;

import static com.alsm.runtime.helper.StringHelper.isNul;

public class Compare {

	public static final boolean equals(String o1, String o2) {
		return o1.compareTo(o2) == 0;
	}

	public static final boolean notEquals(String o1, String o2) {
		return o1.compareTo(o2) != 0;
	}

	public static final boolean lessThan(String o1, String o2) {
		return !isNul(o1) && (o1.compareTo(o2) < 0 || isNul(o2));
	}

	public static final boolean lessThanOrEqual(String o1, String o2) {
		return o1.compareTo(o2) <= 0 || isNul(o2);
	}

	public static final boolean greaterThan(String o1, String o2) {
		return (o1.compareTo(o2) > 0 || isNul(o1)) && !isNul(o2);
	}

	public static final boolean greaterThanOrEqual(String o1, String o2) {
		return o1.compareTo(o2) >= 0 || isNul(o1);
	}

	public static final boolean equals(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) == 0;
		} else {
			return Long.parseLong(o1) == Long.parseLong(o2);
		}
	}

	public static final boolean notEquals(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) != 0;
		} else {
			return Long.parseLong(o1) != Long.parseLong(o2);
		}
	}

	public static final boolean lessThan(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) < 0;
		} else {
			return Long.parseLong(o1) < Long.parseLong(o2);
		}
	}

	public static final boolean lessThanOrEqual(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) <= 0;
		} else {
			return Long.parseLong(o1) <= Long.parseLong(o2);
		}
	}

	public static final boolean greaterThan(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) > 0;
		} else {
			return Long.parseLong(o1) > Long.parseLong(o2);
		}
	}

	public static final boolean greaterThanOrEqual(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) >= 0;
		} else {
			return Long.parseLong(o1) >= Long.parseLong(o2);
		}
	}

}
