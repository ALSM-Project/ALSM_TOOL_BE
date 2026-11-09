package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.DocumentContext;

/** A statement the converter cannot translate must fail the program with a reason, never vanish from the Java. */
class UnsupportedStatementsTest {

	@TempDir
	Path temp;

	private static final String HEAD = String.join("\n",
			"       IDENTIFICATION DIVISION.",
			"       PROGRAM-ID. UNSUP.",
			"       ENVIRONMENT DIVISION.",
			"       INPUT-OUTPUT SECTION.",
			"       FILE-CONTROL.",
			"           SELECT IN-FILE   ASSIGN TO INFILE",
			"                  ORGANIZATION IS LINE SEQUENTIAL.",
			"           SELECT OUT-FILE  ASSIGN TO OUTFILE",
			"                  ORGANIZATION IS LINE SEQUENTIAL.",
			"           SELECT SORT-FILE ASSIGN TO SORTWK.",
			"       DATA DIVISION.",
			"       FILE SECTION.",
			"       FD  IN-FILE.",
			"       01  IN-REC            PIC X(20).",
			"       FD  OUT-FILE.",
			"       01  OUT-REC           PIC X(20).",
			"       SD  SORT-FILE.",
			"       01  SORT-REC.",
			"           05  SORT-KEY      PIC X(5).",
			"           05  SORT-DATA     PIC X(15).",
			"       PROCEDURE DIVISION.",
			"");

	private String failureOf(String statements) throws Exception {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("unsup.cob"), (HEAD + statements + "           STOP RUN.\n").getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		assertEquals(Integer.valueOf(0), report.read("$.converted", Integer.class));
		return report.read("$.failures[0].reason", String.class);
	}

	@Test
	void sortIsNotDroppedQuietly() throws Exception {
		String reason = failureOf("           SORT SORT-FILE ON ASCENDING KEY SORT-KEY\n"
				+ "                USING IN-FILE GIVING OUT-FILE.\n");
		assertTrue(reason.contains("SORT is not supported"), reason);
	}

	@Test
	void mergeIsNotDroppedQuietly() throws Exception {
		String reason = failureOf("           MERGE SORT-FILE ON ASCENDING KEY SORT-KEY\n"
				+ "                USING IN-FILE IN-FILE GIVING OUT-FILE.\n");
		assertTrue(reason.contains("MERGE is not supported"), reason);
	}

	@Test
	void releaseAndReturnAreNotDroppedQuietly() throws Exception {
		assertTrue(failureOf("           RELEASE SORT-REC.\n").contains("RELEASE is not supported"));
		assertTrue(failureOf("           RETURN SORT-FILE AT END MOVE SPACE TO OUT-REC.\n")
				.contains("RETURN is not supported"));
	}
}
