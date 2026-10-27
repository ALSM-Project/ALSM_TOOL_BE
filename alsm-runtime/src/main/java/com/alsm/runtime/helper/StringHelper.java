package com.alsm.runtime.helper;

import java.io.UnsupportedEncodingException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

import com.alsm.runtime.common.Constant;

public class StringHelper {

	public static boolean isSpace(String value) {
		if (value == null) {
			return false;
		}

		return value.trim().length() == 0;
	}

	public static String valueOfGraphic(String value) {
		return value.replace("  ", "　");
	}

	public static boolean isZero(String value) {
		if (value == null) {
			return false;
		}

		return value.trim().replaceAll("0", "").length() == 0;
	}

	public static String space(int number) {
		return String.format("%" + number + "s", "");
	}


	public static String spaceFullsize(int length) {
		return spaceHaft2Full(space(length));
	}

	public static String nul(int number) {
		return space(number).replaceAll(" ", "\0");
	}

	public static String chars(int number, String chr) {
		return space(number).replaceAll(" ", chr);
	}

	public static boolean isNul(String value) {
		if (value == null) {
			return true;
		}

		return value.replaceAll("\0", "").length() == 0;
	}

	public static boolean isNullOrEmpty(String value) {
		return value == null || value.trim().length() == 0;
	}

	public static double getNumber(String value) {
		value = value.replaceAll("\\D+", "");
		return Double.parseDouble(value);
	}

	public static boolean checkOnlyRegex(String value, String input) {
		String regex = "[" + input + "]+";
		return value.matches(regex);
	}

	public static boolean isNumeric(String strNum) {
		Pattern pattern = Pattern.compile("-?\\d+(\\.\\d+)?");
		if (strNum == null) {
			return false;
		}
		return pattern.matcher(strNum).matches();
	}

	public static String getDateWithFomat(String format) {
		DateFormat dateFormat = new SimpleDateFormat(format);
		Date date = new Date();
		return dateFormat.format(date);
	}

	public static String getValueFromBinary(byte[] value) {
		for (int i = 0; i < value.length; i++) {
			byte b = value[i];
			if (b == -4) {
				value[i] = 32;
			}
		}
		try {
			return new String(value, Constant.ENCODING);
		} catch (UnsupportedEncodingException e) {
			return Constant.BLANK;
		}
	}

	public static String spaceHaft2Full(String value) {
		return value.replaceAll(Constant.SPACE, Constant.SPACE_FULLSIZE);
	}
	
	public static String obj2Str(Object obj) {
		try {
			return new String(String.valueOf(obj).getBytes("Shift-JIS"), Constant.ENCODING);
		} catch (UnsupportedEncodingException e) {
			return Constant.BLANK;
		}
	}

	private static boolean isHanAscii(char c) {
		return (0x0020 <= c && c < 0x007f);
	}

	private static boolean isHanKataKana(char c) {
		return (0xff60 < c && c <= 0xff9f);
	}

	public static boolean isHalfWidth(String checkString) {
		char charAtI;
		if (!isSpace(checkString)) {
			for (int i = 0; i < checkString.length(); i++) {
				charAtI = checkString.charAt(i);
				if (!(isHanAscii(charAtI) || isHanKataKana(charAtI))) {
					return false;
				}
			}
		}
		return true;
	}

	public static boolean isFullWidth(String checkString) {
		char charAtI;
		if (!isSpace(checkString)) {
			for (int i = 0; i < checkString.length(); i++) {
				charAtI = checkString.charAt(i);
				if (isHanAscii(charAtI) || isHanKataKana(charAtI)) {
					return false;
				}
			}
		}
		return true;
	}
}