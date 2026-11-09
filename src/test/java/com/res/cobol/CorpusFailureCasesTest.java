package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;

import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.jayway.jsonpath.DocumentContext;

/**
 * Each file in src/test/resources/cobol is the smallest program that reproduced one of the
 * failures of the cobol/ corpus; it must convert whole.
 */
class CorpusFailureCasesTest {

	@TempDir
	Path temp;

	@ParameterizedTest(name = "{0}")
	@ValueSource(strings = { "skpch01.cob", "with-ur.cob" })
	void converts(String resource) throws Exception {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		try (InputStream in = getClass().getResourceAsStream("/cobol/" + resource)) {
			Files.copy(in, project.resolve(resource), StandardCopyOption.REPLACE_EXISTING);
		}
		DocumentContext report = ConverterRun.convert(project, temp);
		List<?> failures = report.read("$.failures", List.class);
		assertEquals(0, failures.size(), failures.toString());
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class));
	}
}
