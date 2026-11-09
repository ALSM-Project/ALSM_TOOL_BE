package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.JsonPath;

/**
 * Converts the whole cobol/ corpus (minutes: every program is translated and the result is
 * compiled) and requires at least as many converted programs as benchmark/baseline.json.
 * Not part of a plain mvn test; run it with: mvn test -Pcorpus
 * After an improvement, record the new mark with scripts/baseline.ps1 -Save.
 */
@Tag("corpus")
class CorpusConversionTest {

	private static final long TIMEOUT_MINUTES = 45;

	@Test
	void convertsAtLeastAsManyProgramsAsTheBaseline(@TempDir Path output) throws Exception {
		Baseline baseline = new Baseline();
		Path log = Paths.get("target", "corpus-conversion.log");
		Files.createDirectories(log.getParent());

		// A separate JVM: the converter keeps its state in singletons and may call System.exit.
		List<String> command = new ArrayList<String>(Arrays.asList(
				Paths.get(System.getProperty("java.home"), "bin", "java").toString(), "-cp",
				Paths.get("target", "classes").toAbsolutePath().toString(), "com.res.cobol.Main"));
		command.addAll(Arrays.asList(baseline.command().split(" ")));
		command.addAll(Arrays.asList("-odir", output.toString()));
		Process process = new ProcessBuilder(command).redirectErrorStream(true).redirectOutput(log.toFile()).start();
		if (!process.waitFor(TIMEOUT_MINUTES, TimeUnit.MINUTES)) {
			process.destroyForcibly();
			throw new AssertionError("the conversion took more than " + TIMEOUT_MINUTES + " minutes, see " + log);
		}
		assertTrue(process.exitValue() == 0, "the converter exited with " + process.exitValue() + ", see " + log);

		// Without a finished compile check, programs whose Java does not compile count as converted.
		String text = new String(Files.readAllBytes(log), StandardCharsets.UTF_8);
		boolean verified = text.contains("BUILD SUCCESS") && !text.contains("skipped: Maven")
				&& !text.contains("the build failed outside") && !text.contains("the build still fails after");
		assertTrue(verified, "the compile check did not finish (is mvn on the PATH?), see " + log);

		File report = output.resolve("conversion_report.json").toFile();
		int converted = JsonPath.parse(report).read("$.converted", Integer.class);
		List<String> now = Baseline.names(JsonPath.parse(report).read("$.succeeded[*].programId", List.class));
		List<String> lost = baseline.convertedPrograms();
		lost.removeAll(now);
		assertTrue(converted >= baseline.converted(), "converted " + converted + " of " + baseline.programs()
				+ ", the baseline is " + baseline.converted() + "; no longer converted: " + lost);
	}
}
