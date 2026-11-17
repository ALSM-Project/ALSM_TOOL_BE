package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.DocumentContext;

/** Free-format source (>>SOURCE FORMAT FREE) is written as the fixed format the scanner reads. */
class FreeFormatTest {

	@TempDir
	Path temp;

	private static String join(String... lines) {
		return String.join("\n", lines);
	}

	private static String[] lines(String text) {
		return text.split("\n", -1);
	}

	@Test
	void aFileWithoutTheDirectiveIsLeftAlone() {
		String text = join("       IDENTIFICATION DIVISION.", "       PROGRAM-ID. P.", "");
		FreeFormat.Result result = FreeFormat.toFixed(text);
		assertSame(text, result.source);
		assertEquals(0, result.changedLines);
	}

	@Test
	void codeMovesIntoAreaBAndTheLineCountHolds() {
		String text = join(">>SOURCE FORMAT FREE", "identification division.", "program-id. P.", "",
				"procedure division.", "MAIN.", "    display \"x\"", "    stop run.", "");
		String[] out = lines(FreeFormat.toFixed(text).source);
		assertEquals(lines(text).length, out.length);
		assertEquals("      *>>SOURCE FORMAT FREE", out[0]);
		assertEquals("       identification division.", out[1]);
		assertEquals("", out[3]);
		assertEquals("       MAIN.", out[5]);
		assertEquals("           display \"x\"", out[6]);
	}

	@Test
	void commentsBecomeFixedFormatComments() {
		String text = join(">>SOURCE FORMAT IS FREE", "*> a whole line", "    move a to b *> and a tail", "");
		String[] out = lines(FreeFormat.toFixed(text).source);
		assertEquals("      * a whole line", out[1]);
		assertEquals("           move a to b", out[2]);
	}

	@Test
	void aCommentMarkerInsideALiteralIsText() {
		String text = join(">>SOURCE FORMAT FREE", "    display \"a *> b\"", "");
		assertEquals("           display \"a *> b\"", lines(FreeFormat.toFixed(text).source)[1]);
	}

	@Test
	void theFormatCanSwitchBack() {
		String text = join(">>SOURCE FORMAT FREE", "move a to b", ">>SOURCE FORMAT FIXED",
				"           MOVE C TO D.", "");
		String[] out = lines(FreeFormat.toFixed(text).source);
		assertEquals("       move a to b", out[1]);
		assertEquals("           MOVE C TO D.", out[3]);
	}

	@Test
	void aLongLineIsBrokenBetweenWordsWithinColumn72() {
		String text = join(">>SOURCE FORMAT FREE",
				"    move function upper-case(ws-input-choice) to transaction-type-of-the-record-in-use",
				"");
		String[] out = lines(FreeFormat.toFixed(text).source);
		assertTrue(out.length > 3, "the line was not broken");
		StringBuilder words = new StringBuilder();
		for (int i = 1; i < out.length - 1; i++) {
			assertTrue(out[i].length() <= 72, "past column 72: " + out[i]);
			assertEquals("       ", out[i].substring(0, 7));
			words.append(out[i].trim()).append(' ');
		}
		assertEquals("move function upper-case(ws-input-choice) to transaction-type-of-the-record-in-use",
				words.toString().trim());
	}

	@Test
	void aLiteralLongerThanALineIsContinuedWithAHyphen() {
		String literal = "ERROR: this message is far longer than one line of fixed format source can hold, "
				+ "so it has to be continued on the next line.";
		String text = join(">>SOURCE FORMAT FREE", "    display \"" + literal + "\".", "");
		String[] out = lines(FreeFormat.toFixed(text).source);
		assertTrue(out.length > 3);
		StringBuilder rebuilt = new StringBuilder();
		for (int i = 1; i < out.length - 1; i++) {
			assertTrue(out[i].length() <= 72, "past column 72: " + out[i]);
			if (out[i].charAt(6) == '-') {
				String body = out[i].substring(7).trim();
				assertTrue(body.startsWith("\""), out[i]);
				rebuilt.append(body.substring(1));
			} else {
				if (rebuilt.length() > 0)
					rebuilt.append(' ');
				rebuilt.append(out[i].trim());
			}
		}
		assertTrue(out.length > 4, "the literal was not continued");
		assertEquals("display \"" + literal + "\".", rebuilt.toString().trim());
	}

	@Test
	void aFunctionNameIsNotSeparatedFromTheWordFunction() {
		String text = join(">>SOURCE FORMAT FREE",
				"        display \"Customer          : \" function trim(ws-customer-name)", "");
		for (String line : lines(FreeFormat.toFixed(text).source))
			assertFalse(line.toLowerCase().trim().endsWith("function"), line);
	}

	@Test
	void aFreeFormatProgramConvertsEndToEnd() throws Exception {
		String message = "ERROR: this message is far longer than one line of fixed format source can hold";
		String program = join(">>SOURCE FORMAT FREE",
				"identification division.",
				"program-id. FREEPRG.",
				"data division.",
				"working-storage section.",
				"01 ws-name pic x(10).",
				"procedure division.",
				"main-process.",
				"    accept ws-name",
				"    display \"" + message + "\"",
				"    stop run.",
				"");
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("freeprg.cbl"), program.getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class),
				String.valueOf((Object) report.read("$.failures", Object.class)));
		try (Stream<Path> files = Files.walk(temp.resolve("out"))) {
			Path tasklet = files.filter(f -> f.getFileName().toString().equals("FreeprgTasklet.java")).findFirst()
					.orElseThrow(() -> new AssertionError("no tasklet generated"));
			String java = new String(Files.readAllBytes(tasklet), StandardCharsets.UTF_8);
			assertTrue(java.contains(message), "the long literal did not survive:\n" + java);
			assertFalse(java.contains(">>"), java);
		}
	}
}
