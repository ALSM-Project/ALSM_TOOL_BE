package jp.co.alsm.common;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;

public class Record implements CharSequence, Serializable {
	static final long serialVersionUID = 1L;

	private int offset;
	private int end;
	private final byte[] datas;

	private String format;
	private int scale;
	private String option;
	private int signed = 1;

	public Record(byte[] datas) {
		this.datas = datas;
		this.offset = 0;
		this.end = this.datas.length;
	}

	public char charAt(int index) {
		int ix = index + offset;
		if (ix >= end) {
			throw new StringIndexOutOfBoundsException("Invalid index " + index + " length " + length());
		}
		return (char) (datas[ix] & 0xff);
	}

	public int length() {
		return end - offset;
	}

	public Record subSequence(int start, int end) {
		return (new Record(datas)).withOffset(start + offset).withEnd(end + offset).withFormat(format)
				.withOption(option).withSigned(signed);
	}

	public void set(String str, int start, int end) {
		try {
			set(str.getBytes(Constant.ENCODING), start, end);
		} catch (UnsupportedEncodingException e) {
			set(str.getBytes(), start, end);
		}
	}

	public void set(int value, int start, int end) {
		if ("COMP".equals(option)) {
			set(compPacked(truncated(value), end - start), start, end);
		} else if ("COMP-3".equals(option)) {
			set(comp3Packed(truncated(value), end - start, format.indexOf("S") > -1), start, end);
		} else {
			setDisplay(BigDecimal.valueOf(value), start, end);
		}
	}

	public void set(long value, int start, int end) {
		if ("COMP".equals(option)) {
			set(compPacked(truncated(value), end - start), start, end);
		} else if ("COMP-3".equals(option)) {
			set(comp3Packed(truncated(value), end - start, format.indexOf("S") > -1), start, end);
		} else {
			setDisplay(BigDecimal.valueOf(value), start, end);
		}
	}

	public void set(BigDecimal value, int start, int end) {
		if ("COMP-3".equals(option)) {
			String data = String.format("%."+ scale +"f", value);
			set(comp3Packed(Long.parseLong(data.replace(".", "")), end - start,
					format.indexOf("S") > -1), start, end);
		} else if ("COMP".equals(option)) {
			set(value.movePointRight(scale).longValue(), start, end);
		} else {
			setDisplay(value, start, end);
		}
	}

	/**
	 * Writes a number as DISPLAY digits: scaled by the picture's decimal places (the V is implied,
	 * so no point is stored), truncated rather than rounded, zero-filled to the field length with
	 * the high-order digits dropped on overflow. A negative value in a signed (S) picture carries
	 * its sign as an overpunch on the last digit.
	 */
	private void setDisplay(BigDecimal value, int start, int end) {
		if (isEdited(format)) {
			set(new jp.co.alsm.helper.FieldFormat("record", format).format(value), start, end);
			return;
		}
		int length = end - start;
		java.math.BigInteger scaled = value.movePointRight(scale).setScale(0, java.math.RoundingMode.DOWN)
				.toBigIntegerExact();
		String digits = scaled.abs().toString();
		StringBuilder text = new StringBuilder();
		for (int i = digits.length(); i < length; i++) {
			text.append('0');
		}
		text.append(digits);
		String fitted = text.substring(text.length() - length);
		if (scaled.signum() < 0 && format != null && format.indexOf('S') > -1) {
			char last = fitted.charAt(length - 1);
			fitted = fitted.substring(0, length - 1) + (char) ('p' + (last - '0'));
		}
		set(fitted, start, end);
	}

	/** A numeric-edited picture (999999.99, ZZ9.99, $$,$$9 ...) carries characters that are printed. */
	private static boolean isEdited(String picture) {
		if (picture == null) {
			return false;
		}
		String p = picture.toUpperCase();
		if (p.startsWith("X") || p.startsWith("A")) {
			return false;
		}
		return p.indexOf('.') > -1 || p.indexOf('Z') > -1 || p.indexOf('*') > -1 || p.indexOf('$') > -1
				|| p.indexOf('+') > -1 || p.indexOf('-') > -1 || p.indexOf(',') > -1 || p.indexOf('B') > -1
				|| p.indexOf('/') > -1 || p.contains("CR") || p.contains("DB");
	}

	/** Text of the field with an overpunched sign turned back into a leading minus. */
	private String numericText() {
		String text = toString().trim();
		if (!text.isEmpty()) {
			char last = text.charAt(text.length() - 1);
			if (last >= 'p' && last <= 'y') {
				text = "-" + text.substring(0, text.length() - 1) + (char) ('0' + (last - 'p'));
			}
		}
		return text;
	}

	/** A buffer of n blanks: a group starts out as spaces, not as zero bytes. */
	public static byte[] spaces(int n) {
		byte[] b = new byte[n];
		java.util.Arrays.fill(b, (byte) ' ');
		return b;
	}

	public void set(byte[] data, int start, int end) {
		int size = end - start;
		for (int i = 0; i < size; i++) {
			if (i >= data.length) {
				datas[i + start] = 0x20;
				continue;
			}
			datas[i + start] = data[i];
		}
	}

	public String toString() {
		try {
			String value = new String(datas, offset, end - offset, Constant.ENCODING);
			return scale > 0 ? formatScale(value) : value;
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException("Unexpected: " + Constant.ENCODING + " not supported");
		}
	}

	public String toString(int start, int length) {
		try {
			String value = new String(datas, offset + start, length, Constant.ENCODING);
			return scale > 0 ? formatScale(value) : value;
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException("Unexpected: " + Constant.ENCODING + " not supported");
		}
	}

	public int toInt() {
		if ("COMP".equals(option)) {
			return (int) unCompPacked(datas, offset, end);
		}
		if ("COMP-3".equals(option)) {
			return (int) unComp3Packed(datas, offset, end);
		}
		try {
			return Integer.parseInt(numericText());
		} catch (Exception e) {
			return 0;
		}
	}

	public long toLong() {
		if ("COMP".equals(option)) {
			return unCompPacked(datas, offset, end);
		}
		if ("COMP-3".equals(option)) {
			return unComp3Packed(datas, offset, end);
		}
		try {
			return Long.parseLong(numericText());
		} catch (Exception e) {
			return 0;
		}
	}

	public BigDecimal toBigDecimal() {
		try {
			String data = "";
			if ("COMP-3".equals(option)) {
				data = Long.toString(unComp3Packed(datas, offset, end));
				data = scale > 0 ? formatScale(data) : data;
			} else {
				data = numericText();
			}
			if (data.isEmpty()) {
				return new BigDecimal(0);
			}
			return new BigDecimal(data);
		} catch (Exception e) {
			return new BigDecimal(0);
		}
	}

	public Record withOffset(int offset) {
		this.offset = offset;
		return this;
	}

	public Record withEnd(int end) {
		this.end = end;
		return this;
	}

	public Record withFormat(String fmt) {
		this.format = fmt;
		this.scale = countScale(fmt);
		return this;
	}

	public Record withOption(String opt) {
		this.option = opt;
		return this;
	}

	private Record withSigned(int signed) {
		this.signed = signed;
		return this;
	}

	public int getOffset() {
		return offset;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public int getEnd() {
		return end;
	}

	public void setEnd(int end) {
		this.end = end;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public String getOption() {
		return option;
	}

	public void setOption(String option) {
		this.option = option;
	}

	public byte[] getDatas() {
		return datas;
	}

	private static long unCompPacked(byte[] datas, int offset, int end) {
		int length = end - offset;
		byte[] data = new byte[length];
		System.arraycopy(datas, offset, data, 0, length);
		BigInteger parser = new BigInteger(data);
		return parser.longValue();
	}

	/**
	 * A binary or packed item keeps only the digits of its picture (COBOL's TRUNC(STD)): 12345 moved
	 * to PIC 9(4) COMP is 2345. Without it a growing counter overflowed the field's bytes.
	 */
	private static final java.util.regex.Pattern DIGITS = java.util.regex.Pattern.compile("9\\((\\d+)\\)|9");
	/** 10^digits of the picture, 0 when it does not limit the value; computed once per format. */
	private long digitLimit = -1;
	private String limitFormat;

	private long truncated(long value) {
		if (format == null)
			return value;
		if (digitLimit < 0 || !format.equals(limitFormat)) {
			int digits = 0;
			java.util.regex.Matcher m = DIGITS.matcher(format);
			while (m.find())
				digits += m.group(1) == null ? 1 : Integer.parseInt(m.group(1));
			long limit = 0;
			if (digits > 0 && digits < 19) {
				limit = 1;
				for (int i = 0; i < digits; i++)
					limit *= 10;
			}
			digitLimit = limit;
			limitFormat = format;
		}
		return digitLimit == 0 ? value : value % digitLimit;
	}

	private static byte[] compPacked(long number, int byteCount) {
		BigInteger parser = new BigInteger(String.valueOf(number));
		byte[] full = parser.toByteArray();
		// Keep the low-order bytes when the number is wider than the field.
		byte[] real = full.length > byteCount ? java.util.Arrays.copyOfRange(full, full.length - byteCount, full.length) : full;
		byte[] data = new byte[byteCount];
		int shift = data.length - real.length;
		for (int i = 0; i < real.length; i++) {
			data[i + shift] = real[i];
		}
		return data;
	}

	private static final int NoneSign = 0x0F;
	private static final int PlusSign = 0x0C;
	private static final int MinusSign = 0x0D;
	private static final int DropHO = 0xFF;
	private static final int GetLO = 0x0F;

	private static long unComp3Packed(byte[] datas, int offset, int end) {
		long val = 0;

		for (int i = offset; i < end; i++) {
			int aByte = datas[i] & DropHO;
			if (i == end - 1) {
				int digit = aByte >> 4;
				val = val * 10 + digit;
				int sign = aByte & GetLO;
				if (sign == MinusSign) {
					val = -val;
				}
			} else {
				int digit = aByte >> 4;
				val = val * 10 + digit;
				digit = aByte & GetLO;
				val = val * 10 + digit;
			}
		}
		return val;
	}

	private static byte[] comp3Packed(long number, int byteCount, boolean isPlusSign) {
		byte[] bytes = new byte[byteCount];
		String data = Long.toString(number);
		boolean isNegative = false;

		if (data.charAt(0) == '-') {
			isNegative = true;
			data = data.substring(1);
		}

		int maxLength = byteCount * 2 - 1;
		if (data.length() > maxLength) {
			data = data.substring(data.length() - maxLength, data.length());
		}

		int length = data.length();
		if (length % 2 == 0) {
			data = "0" + data;
			length++;
		}

		int neededBytes = (int) (((length + 1) / 2f) + 0.5f);

		int extraBytes = neededBytes - byteCount;
		if (extraBytes < 0) {
			for (int i = 0; i < -extraBytes; i++) {
				bytes[i] = 0x00;
			}
		} else if (extraBytes > 0) {
			data = data.substring(extraBytes);
			length -= extraBytes;
			extraBytes = 0;
		}

		for (int pos = 0; pos <= length - 1; pos++) {
			String digit = data.substring(pos, pos + 1);
			int now = (pos / 2) - extraBytes;

			if (pos % 2 == 0) {
				bytes[now] = (byte) (Byte.valueOf(digit) << 4);
			} else {
				bytes[now] = (byte) (bytes[now] | (Byte.valueOf(digit) & 0x0f));
			}
		}

		if (isNegative) {
			bytes[byteCount - 1] = (byte) (bytes[byteCount - 1] | MinusSign);
		} else if (isPlusSign) {
			bytes[byteCount - 1] = (byte) (bytes[byteCount - 1] | PlusSign);
		} else {
			bytes[byteCount - 1] = (byte) (bytes[byteCount - 1] | NoneSign);
		}

		return bytes;
	}

	public static String asHexString(String value) throws UnsupportedEncodingException {
		return asString(value.getBytes(Constant.ENCODING));
	}

	public static String asString(byte[] b) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < b.length; i++) {
			int curByte = b[i] & 0xFF;
			if (curByte <= 0x0F) {
				sb.append("0");
			}
			sb.append(Integer.toString(curByte, 16));
		}
		return sb.toString().trim();
	}

	public static byte[] asBytes(String s) {
		String tmp;
		byte[] b = new byte[s.length() / 2];
		int i;
		for (i = 0; i < s.length() / 2; i++) {
			tmp = s.substring(i * 2, i * 2 + 2);
			b[i] = (byte) (Integer.parseInt(tmp, 16) & 0xff);
		}
		return b;
	}

	public int countScale(String format) {
		if (format == null || format.indexOf("V") == -1) {
			return 0;
		}
		String formatScale = format.substring(format.indexOf("V") + 1);
		if (formatScale.indexOf("(") == -1) {
			return formatScale.length();
		}
		return Integer.parseInt(formatScale.replace("9(", "").replace(")", ""));
	}

	public String formatScale(String value) {
		int idx = value.length() - scale;
		return value.substring(0, idx) + "." + value.substring(idx);
	}

	public static String highValue(int length) {
		byte[] b = new byte[length];
		for (int i = 0; i < length; i++) {
			b[i] = (byte) 0xFF;
		}

		return new String(b);
	}

	public static String lowValue(int length) {
		byte[] b = new byte[length];
		for (int i = 0; i < length; i++) {
			b[i] = (byte) 0x00;
		}

		return new String(b);
	}

}