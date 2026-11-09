package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.jayway.jsonpath.JsonPath;

/**
 * A project with a Shift-JIS, a UTF-8 and an ASCII program converts whole without -encoding, and
 * the report says which file was read as MS932.
 */
class ProjectEncodingTest {

	private static final Charset MS932 = Charset.forName("MS932");
	/** "nihongo": three kanji, six bytes in MS932 that are not valid UTF-8. */
	private static final String JAPANESE = new String(new char[] { 0x65e5, 0x672c, 0x8a9e });

	@TempDir
	Path temp;

	/**
	 * Thirty characters: in Shift-JIS the literal below ends before column 72, read as UTF-8 each
	 * kanji turns into two U+FFFD, the line gets longer and its closing quote is cut off.
	 */
	private static final String LONG_JAPANESE = JAPANESE + JAPANESE + JAPANESE + JAPANESE + JAPANESE + JAPANESE
			+ JAPANESE + JAPANESE + JAPANESE + JAPANESE;

	private static String program(String id, String comment) {
		StringBuilder out = new StringBuilder();
		for (String line : new String[] {
				"       IDENTIFICATION DIVISION.",
				"       PROGRAM-ID. " + id + ".",
				"      * " + comment,
				"       DATA DIVISION.",
				"       WORKING-STORAGE SECTION.",
				"       01  WS-MSG  PIC X(60) VALUE '" + comment + "'.",
				"       PROCEDURE DIVISION.",
				"           DISPLAY WS-MSG.",
				"           STOP RUN." })
			out.append(line).append('\n');
		return out.toString();
	}

	private Path project() throws IOException {
		Path project = temp.resolve("project");
		Files.createDirectories(project);
		Files.write(project.resolve("sjis.cob"), program("SJISPROG", LONG_JAPANESE).getBytes(MS932));
		Files.write(project.resolve("utf8.cob"), program("UTF8PROG", LONG_JAPANESE).getBytes(StandardCharsets.UTF_8));
		Files.write(project.resolve("ascii.cob"), program("ASCIIPROG", "PLAIN TEXT").getBytes(StandardCharsets.US_ASCII));
		return project;
	}

	/** Runs the converter in its own JVM (it keeps its state in singletons) and returns the report. */
	private com.jayway.jsonpath.DocumentContext convert(Path project, String... extra) throws Exception {
		Path out = temp.resolve("out" + extra.length);
		List<String> command = new ArrayList<String>(Arrays.asList(
				Paths.get(System.getProperty("java.home"), "bin", "java").toString(), "-cp",
				Paths.get("target", "classes").toAbsolutePath().toString(), "com.res.cobol.Main", "-project",
				project.toString(), "-odir", out.toString(), "-basePackage", "t.p"));
		command.addAll(Arrays.asList(extra));
		File log = temp.resolve("run" + extra.length + ".log").toFile();
		Process process = new ProcessBuilder(command).redirectErrorStream(true).redirectOutput(log).start();
		assertTrue(process.waitFor(2, TimeUnit.MINUTES), "the converter did not finish");
		String output = new String(Files.readAllBytes(log.toPath()), StandardCharsets.UTF_8);
		assertEquals(0, process.exitValue(), output);
		return JsonPath.parse(out.resolve("conversion_report.json").toFile());
	}

	@Test
	void withoutEncodingEveryProgramConvertsAndOnlyTheShiftJisFileIsNoted() throws Exception {
		com.jayway.jsonpath.DocumentContext report = convert(project());
		assertEquals(Integer.valueOf(3), report.read("$.converted", Integer.class));
		assertEquals(Integer.valueOf(0), report.read("$.failed", Integer.class));
		List<?> warnings = report.read("$.warnings", List.class);
		assertEquals(Arrays.asList("sjis.cob: decoded as MS932"), warnings);
	}

	@Test
	void aByteNoCharsetCanReadIsReportedWithItsLine() throws Exception {
		// 0xFD is not valid in UTF-8 nor in MS932: no choice of encoding helps, the report says where it is.
		Path project = temp.resolve("broken");
		Files.createDirectories(project);
		byte[] source = program("BADPROG", "PLAIN TEXT").replace("DISPLAY WS-MSG.", "DISPLAY WS-MSG?.")
				.getBytes(StandardCharsets.US_ASCII);
		for (int i = 0; i < source.length; i++)
			if (source[i] == '?')
				source[i] = (byte) 0xFD;
		Files.write(project.resolve("broken.cob"), source);
		com.jayway.jsonpath.DocumentContext report = convert(project);
		List<?> warnings = report.read("$.warnings", List.class);
		assertTrue(warnings.contains("broken.cob: decoded as MS932"), warnings.toString());
		assertTrue(warnings.contains("broken.cob:8: has a byte that is not valid MS932 (read as U+FFFD)"),
				warnings.toString());
	}

	@Test
	void withAnEncodingTheProjectIsReadAsTold() throws Exception {
		// Only the Shift-JIS program is valid in MS932, the other two are ASCII-compatible at the
		// bytes the parser looks at; nothing is detected or noted.
		Path project = temp.resolve("sjisonly");
		Files.createDirectories(project);
		Files.write(project.resolve("sjis.cob"), program("SJISPROG", LONG_JAPANESE).getBytes(MS932));
		com.jayway.jsonpath.DocumentContext report = convert(project, "-encoding", "MS932");
		assertEquals(Integer.valueOf(1), report.read("$.converted", Integer.class));
		assertTrue(report.read("$.warnings", List.class).isEmpty());
	}
}
