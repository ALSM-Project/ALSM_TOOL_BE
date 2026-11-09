package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.DocumentContext;

/**
 * EXEC SQL INCLUDE of a DCLGEN member is expanded when the member is in the project; when it is
 * not, no item of it exists and the program cannot convert, which the report says by name.
 */
class SqlIncludeTest {

	@TempDir
	Path temp;

	private Path projectWithProgram() throws IOException {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		try (InputStream in = getClass().getResourceAsStream("/cobol/dclgen-missing.cob")) {
			Files.copy(in, project.resolve("dcltest.cob"), StandardCopyOption.REPLACE_EXISTING);
		}
		return project;
	}

	@Test
	void anIncludedDclgenThatIsInTheProjectIsExpanded() throws Exception {
		Path project = projectWithProgram();
		Files.write(project.resolve("VDCLTMF.cpy"),
				"       01  DCLTMF.\n           10 A0   PIC X(4).\n".getBytes(StandardCharsets.US_ASCII));
		DocumentContext report = ConverterRun.convert(project, temp);
		List<?> failures = report.read("$.failures", List.class);
		assertEquals(0, failures.size(), failures.toString());
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class));
	}

	@Test
	void aMissingDclgenIsNamedInTheFailure() throws Exception {
		DocumentContext report = ConverterRun.convert(projectWithProgram(), temp);
		assertEquals(Integer.valueOf(0), report.read("$.converted", Integer.class));
		String reason = report.read("$.failures[0].reason", String.class);
		assertTrue(reason.startsWith("Unknown Symbol : DCLTMF"), reason);
		assertTrue(reason.endsWith("(copybook not found: VDCLTMF)"), reason);
	}
}
