package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/** CicsNormalizer writes IF x = ( a OR b ) out as separate comparisons and leaves what it cannot be sure of. */
class ParenthesizedComparisonTest {

	private static final String HEAD = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. P.\n"
			+ "       PROCEDURE DIVISION.\n";

	private static String normalize(String procedure) {
		return CicsNormalizer.normalize(HEAD + procedure).source;
	}

	private static String flat(String source) {
		return source.replaceAll("\\s+", " ");
	}

	private static int lines(String text) {
		return text.split("\n", -1).length;
	}

	@Test
	void aVariableComparedWithAListOfObjects() {
		String in = "           IF WSV2-NOSCD  =  (  SPACE OR '00'  )\n              MOVE 1 TO X.\n";
		String out = normalize(in);
		assertTrue(flat(out).contains("IF (WSV2-NOSCD = SPACE OR WSV2-NOSCD = '00') MOVE 1 TO X."), out);
		assertEquals(lines(HEAD + in), lines(out));
	}

	@Test
	void aFigurativeSubjectGoesOnTheRight() {
		String in = "           IF ZERO = ( A AND B AND\n                       C )\n              GO TO FIN.\n";
		String out = normalize(in);
		assertTrue(flat(out).contains("IF (A = ZERO AND B = ZERO AND C = ZERO) GO TO FIN."), out);
		assertEquals(lines(HEAD + in), lines(out));
	}

	@Test
	void everyLineStaysInTheCodeArea() {
		// Twelve objects, three to a line as a programmer would write them (a line ends at column 72).
		StringBuilder in = new StringBuilder("           IF ZERO = ( WKD-FIELD-1 AND WKD-FIELD-2 AND WKD-FIELD-3 AND\n");
		for (int i = 4; i <= 12; i += 3)
			in.append("                       WKD-FIELD-").append(i).append(" AND WKD-FIELD-").append(i + 1)
					.append(i + 2 <= 12 ? " AND WKD-FIELD-" + (i + 2) + (i + 2 < 12 ? " AND\n" : " )\n") : "\n");
		String out = normalize(in + "              GO TO FIN.\n");
		for (String line : out.split("\n"))
			assertTrue(line.length() <= 72, line);
		assertTrue(flat(out).contains("WKD-FIELD-12 = ZERO)"), out);
	}

	@Test
	void notEqualAndLaterOperandsOfAnAndAreRewrittenToo() {
		String out = normalize("           IF A = 1 AND B NOT = ( 2 OR 3 )\n              MOVE 1 TO X.\n");
		assertTrue(flat(out).contains("IF A = 1 AND (B NOT = 2 OR B NOT = 3) MOVE 1 TO X."), out);
	}

	@Test
	void whatIsNotPlainIsLeftAlone() {
		String[] untouched = {
				"           IF A = ( B (I) OR C )\n              MOVE 1 TO X.\n",
				"           IF A = ( B + 1 )\n              MOVE 1 TO X.\n",
				"           IF A = ( NOT B OR C )\n              MOVE 1 TO X.\n",
				"           IF A = B OR C\n              MOVE 1 TO X.\n",
				"           IF A > ( B OR C )\n              MOVE 1 TO X.\n" };
		for (String in : untouched)
			assertEquals(HEAD + in, normalize(in));
	}

	@Test
	void notInTheProcedureDivisionNothingChanges() {
		String in = "       WORKING-STORAGE SECTION.\n       01  X PIC X VALUE 'A'.\n";
		String out = CicsNormalizer.normalize("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. P.\n" + in).source;
		assertEquals("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. P.\n" + in, out);
	}
}
