package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

/** Runs the converter on a project folder in its own JVM (it keeps its state in singletons) and reads the report. */
final class ConverterRun {

	private ConverterRun() {
	}

	/** @return the parsed conversion_report.json; fails the test when the converter does not exit with 0 */
	static DocumentContext convert(Path project, Path work, String... extra) throws IOException, InterruptedException {
		Path out = work.resolve("out");
		List<String> command = new ArrayList<String>(Arrays.asList(
				Paths.get(System.getProperty("java.home"), "bin", "java").toString(), "-cp",
				Paths.get("target", "classes").toAbsolutePath().toString(), "com.res.cobol.Main", "-project",
				project.toString(), "-odir", out.toString(), "-basePackage", "t.p"));
		command.addAll(Arrays.asList(extra));
		File log = work.resolve("run.log").toFile();
		Process process = new ProcessBuilder(command).redirectErrorStream(true).redirectOutput(log).start();
		assertTrue(process.waitFor(2, TimeUnit.MINUTES), "the converter did not finish");
		String output = new String(Files.readAllBytes(log.toPath()), StandardCharsets.UTF_8);
		assertEquals(0, process.exitValue(), output);
		return JsonPath.parse(out.resolve("conversion_report.json").toFile());
	}
}
