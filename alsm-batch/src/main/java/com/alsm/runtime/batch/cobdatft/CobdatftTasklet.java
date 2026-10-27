package com.alsm.runtime.batch.cobdatft;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;

/**
 * Stand-in for the mainframe date format routine COBDATFT, which works on the CODATECN-REC
 * record: input type (1 byte), input date (20), output type (1), output date (20), error text.
 * Type "1" is YYYYMMDD and type "2" is YYYY-MM-DD.
 */
public class CobdatftTasklet {

	private static final int IN_TYPE = 0;
	private static final int IN_DATE = 1;
	private static final int OUT_TYPE = 21;
	private static final int OUT_DATE = 22;
	private static final int DATE_LENGTH = 20;

	/** Takes the CODATECN record and returns it with the output date filled in. */
	public Object execute(Object record) throws Exception {
		Method getDatas = record.getClass().getMethod("getDatas");
		byte[] bytes = ((byte[]) getDatas.invoke(record)).clone();
		if (bytes.length < OUT_DATE + DATE_LENGTH) {
			return new String(bytes, StandardCharsets.ISO_8859_1);
		}

		String input = new String(bytes, IN_DATE, DATE_LENGTH, StandardCharsets.ISO_8859_1).trim();
		String digits = input.replace("-", "");
		String output = input;
		if (digits.length() >= 8) {
			String year = digits.substring(0, 4);
			String month = digits.substring(4, 6);
			String day = digits.substring(6, 8);
			char outType = (char) bytes[OUT_TYPE];
			output = outType == '2' ? year + "-" + month + "-" + day : year + month + day;
		}
		byte[] out = String.format("%-" + DATE_LENGTH + "s", output).getBytes(StandardCharsets.ISO_8859_1);
		System.arraycopy(out, 0, bytes, OUT_DATE, DATE_LENGTH);
		return new String(bytes, StandardCharsets.ISO_8859_1);
	}
}
