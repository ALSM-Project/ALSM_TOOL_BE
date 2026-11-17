package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.DocumentContext;

/** The scanner has no TRIM among its function names; the normalizer writes it as REVERSE with a marker. */
class TrimFunctionTest {

	@TempDir
	Path temp;

	private static final String HEAD = String.join("\n",
			"       IDENTIFICATION DIVISION.",
			"       PROGRAM-ID. TRIMMER.",
			"       DATA DIVISION.",
			"       WORKING-STORAGE SECTION.",
			"       01  WS-NAME           PIC X(30).",
			"       01  WS-OTHER          PIC X(30).",
			"       PROCEDURE DIVISION.",
			"");

	private String convert(String statements) throws Exception {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("trimmer.cob"),
				(HEAD + statements + "           STOP RUN.\n").getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class), String.valueOf((Object) report.read("$.failures", Object.class)));
		try (Stream<Path> files = Files.walk(temp.resolve("out"))) {
			Path tasklet = files.filter(f -> f.getFileName().toString().equals("TrimmerTasklet.java")).findFirst()
					.orElseThrow(() -> new AssertionError("no tasklet generated"));
			return new String(Files.readAllBytes(tasklet), StandardCharsets.UTF_8);
		}
	}

	@Test
	void trimInADisplayBecomesATrimCall() throws Exception {
		String java = convert("           DISPLAY \"Name: \" FUNCTION TRIM(WS-NAME).\n");
		assertTrue(java.contains("CobolFunction.trim("), java);
		assertFalse(java.contains("reverse"), "the marker leaked into the Java:\n" + java);
		assertFalse(java.contains("ALSM$TRIM"), java);
	}

	@Test
	void trimInAMoveBecomesATrimCall() throws Exception {
		String java = convert("           MOVE FUNCTION TRIM(WS-NAME) TO WS-OTHER.\n");
		assertTrue(java.contains("CobolFunction.trim("), java);
	}

	@Test
	void aRealReverseIsStillAReverse() throws Exception {
		String java = convert("           MOVE FUNCTION REVERSE(WS-NAME) TO WS-OTHER.\n");
		assertTrue(java.contains("CobolFunction.reverse("), java);
		assertFalse(java.contains("CobolFunction.trim("), java);
	}

	/** The rewrite is longer than the original; a line it pushes past column 72 must not be cut. */
	@Test
	void aLineThatGrowsPastColumn72IsLaidOutAgain() throws Exception {
		String java = convert("           DISPLAY \"A fairly long literal text: \" FUNCTION TRIM(WS-NAME)\n"
				+ "               \" and another one \" FUNCTION TRIM(WS-OTHER).\n");
		int calls = java.split("CobolFunction\\.trim\\(", -1).length - 1;
		assertEquals(2, calls, java);
	}

	@Test
	void trimWithLeadingOrTrailingIsStillRefusedWithAReason() throws Exception {
		Path project = temp.resolve("project2");
		Files.createDirectories(project);
		Files.write(project.resolve("trimmer.cob"),
				(HEAD + "           DISPLAY FUNCTION TRIM(WS-NAME TRAILING).\n           STOP RUN.\n")
						.getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, Files.createDirectories(temp.resolve("w2")));
		assertEquals(Integer.valueOf(0), report.read("$.converted", Integer.class));
	}
}
