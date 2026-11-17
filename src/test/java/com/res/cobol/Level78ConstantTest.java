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

/** 78 NAME VALUE literal. (a constant) is read as an elementary item holding the value. */
class Level78ConstantTest {

	@TempDir
	Path temp;

	private static String normalized(String... lines) {
		return CicsNormalizer.normalize(String.join("\n", lines) + "\n").source;
	}

	@Test
	void aTextConstantBecomesAnAlphanumericItem() {
		String out = normalized("       78 TYPE-DEPOSIT        VALUE \"D\".");
		assertTrue(out.contains("01 TYPE-DEPOSIT PIC X(1) VALUE \"D\"."), out);
		assertFalse(out.contains("78 "), out);
	}

	@Test
	void theLengthOfTheLiteralSetsThePicture() {
		assertTrue(normalized("       78 GREETING VALUE 'HELLO'.").contains("PIC X(5) VALUE 'HELLO'."));
	}

	@Test
	void numericConstantsKeepTheirDigits() {
		assertTrue(normalized("       78 LIMIT-A VALUE 100.").contains("PIC 9(3) VALUE 100."));
		assertTrue(normalized("       78 RATE VALUE 1.25.").contains("PIC 9(1)V9(2) VALUE 1.25."));
		assertTrue(normalized("       78 DELTA VALUE -5.").contains("PIC S9(1) VALUE -5."));
	}

	@Test
	void aLineStaysWithinColumn72() {
		String out = normalized("       78 A-VERY-LONG-CONSTANT-NAME-FOR-A-LONG-TEXT VALUE",
				"           \"some text that is rather long\".");
		for (String line : out.split("\n"))
			assertTrue(line.length() <= 72, line);
	}

	@Test
	void anotherLevelIsNotTouched() {
		String line = "       77 WS-COUNT PIC 9 VALUE 1.";
		assertTrue(normalized(line).contains(line));
	}

	@Test
	void aProgramWithConstantsConvertsToCompilableModel() throws Exception {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("consts.cpy"),
				String.join("\n", "       78 TYPE-DEPOSIT        VALUE \"D\".",
						"       78 TYPE-WITHDRAWAL     VALUE \"W\".", "").getBytes(StandardCharsets.US_ASCII));
		Files.write(project.resolve("usesconst.cob"), String.join("\n",
				"       IDENTIFICATION DIVISION.",
				"       PROGRAM-ID. USESCONST.",
				"       DATA DIVISION.",
				"       WORKING-STORAGE SECTION.",
				"       COPY CONSTS.",
				"       01  REC.",
				"           05  REC-X         PIC X.",
				"       PROCEDURE DIVISION.",
				"           MOVE TYPE-DEPOSIT TO REC-X.",
				"           IF REC-X = TYPE-DEPOSIT",
				"               DISPLAY \"DEPOSIT\"",
				"           END-IF.",
				"           STOP RUN.",
				"").getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class),
				String.valueOf((Object) report.read("$.failures", Object.class)));
		try (Stream<Path> files = Files.walk(temp.resolve("out"))) {
			Path model = files.filter(f -> f.getFileName().toString().equals("UsesconstModel.java")).findFirst()
					.orElseThrow(() -> new AssertionError("no model generated"));
			String java = new String(Files.readAllBytes(model), StandardCharsets.UTF_8);
			assertFalse(java.contains("class TypeDeposit"), "the constant became a class:\n" + java);
			assertTrue(java.contains("typeDeposit"), java);
		}
	}
}
