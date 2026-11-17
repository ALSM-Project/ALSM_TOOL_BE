package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.DocumentContext;

/**
 * ACCEPT into a one-character PIC X item reads a char. Java widens a char to int when no setter takes
 * a char, so 'D' reached the field as "68" and every comparison with "D" failed.
 */
class AcceptCharTest {

	@TempDir
	Path temp;

	private static final String PROGRAM = String.join("\n",
			"       IDENTIFICATION DIVISION.",
			"       PROGRAM-ID. ACCCHR.",
			"       DATA DIVISION.",
			"       WORKING-STORAGE SECTION.",
			"       01  WS-CHOICE         PIC X.",
			"       01  WS-NAME           PIC X(10).",
			"       PROCEDURE DIVISION.",
			"           ACCEPT WS-CHOICE.",
			"           ACCEPT WS-NAME.",
			"           IF WS-CHOICE = \"D\"",
			"               DISPLAY \"DEPOSIT\"",
			"           END-IF.",
			"           STOP RUN.",
			"");

	@Test
	void aOneCharacterFieldCanTakeTheCharThatAcceptReads() throws Exception {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("accchr.cob"), PROGRAM.getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class));

		String model = generated("ACCCHR", "Model");
		assertTrue(model.contains("setWsChoice(char value)"), "no char setter for WS-CHOICE in:\n" + model);
		assertTrue(model.contains("String.valueOf(value)"), model);
		assertTrue(generated("ACCCHR", "Tasklet").contains("setWsChoice(Console.readChar())"),
				"the ACCEPT no longer goes through readChar");
	}

	/** A program with a group takes the other Model writer; it must give the char setter too. */
	@Test
	void theSameHoldsWhenTheProgramAlsoDeclaresAGroup() throws Exception {
		String program = String.join("\n",
				"       IDENTIFICATION DIVISION.",
				"       PROGRAM-ID. ACCGRP.",
				"       DATA DIVISION.",
				"       WORKING-STORAGE SECTION.",
				"       01  REC.",
				"           05  REC-TYPE      PIC X.",
				"       01  WS-CHOICE         PIC X.",
				"       PROCEDURE DIVISION.",
				"           ACCEPT WS-CHOICE.",
				"           MOVE WS-CHOICE TO REC-TYPE.",
				"           STOP RUN.",
				"");
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("accgrp.cob"), program.getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class));
		String model = generated("ACCGRP", "Model");
		assertTrue(model.contains("setWsChoice(char value)"), "no char setter for WS-CHOICE in:\n" + model);
	}

	private String generated(String program, String kind) throws IOException {
		try (Stream<Path> files = Files.walk(temp.resolve("out"))) {
			Path file = files.filter(f -> f.getFileName().toString().equalsIgnoreCase(program + kind + ".java"))
					.findFirst().orElseThrow(() -> new AssertionError("no " + program + kind + ".java generated"));
			return new String(Files.readAllBytes(file), StandardCharsets.UTF_8);
		}
	}
}
