package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** ProjectIndex tells a program from a copybook by what the file holds, not by its extension. */
class ProjectIndexClassifyTest {

	private static final Charset MS932 = Charset.forName("MS932");

	@TempDir
	Path root;

	private static String text(String... lines) {
		StringBuilder out = new StringBuilder();
		for (String line : lines)
			out.append(line).append('\n');
		return out.toString();
	}

	private Path put(String name, String content, Charset charset) throws IOException {
		Path file = root.resolve(name);
		Files.createDirectories(file.getParent());
		Files.write(file, content.getBytes(charset));
		return file;
	}

	private Path put(String name, String content) throws IOException {
		return put(name, content, StandardCharsets.UTF_8);
	}

	private static final String PROGRAM = text(
			"       IDENTIFICATION DIVISION.",
			"       PROGRAM-ID. HELLO.",
			"       PROCEDURE DIVISION.",
			"           STOP RUN.");

	private static final String COPYBOOK = text(
			"       01  REC.",
			"           02  REC-ID    PIC X(8).");

	/** Relative names, to compare with. */
	private Set<String> names(List<Path> files) {
		Set<String> names = new TreeSet<String>();
		for (Path file : files)
			names.add(root.relativize(file.toAbsolutePath().normalize()).toString().replace('\\', '/'));
		return names;
	}

	private ProjectIndex scan(Charset charset) throws IOException {
		return ProjectIndex.scan(root, charset);
	}

	@Test
	void cobFileInACopybookFolderIsACopybook() throws IOException {
		put("COPY/VFILABC.COB", COPYBOOK);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertTrue(index.getPrograms().isEmpty());
		assertTrue(index.hasCopybook("VFILABC"));
		assertTrue(index.hasCopybook("VFILABC.COB"));
		assertEquals(new TreeSet<String>(Arrays.asList("COPY/VFILABC.COB")), names(index.getCopybookFiles()));
	}

	@Test
	void cpyFileIsACopybook() throws IOException {
		put("shared.cpy", COPYBOOK);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertTrue(index.getPrograms().isEmpty());
		assertTrue(index.hasCopybook("shared"));
	}

	@Test
	void fileWithoutExtensionInACopybookFolderIsACopybook() throws IOException {
		put("copybooks/NOEXT", COPYBOOK);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertTrue(index.getPrograms().isEmpty());
		assertTrue(index.hasCopybook("NOEXT"));
	}

	@Test
	void ordinaryCobFileWithAProgramIdIsAProgram() throws IOException {
		put("src/hello.cob", PROGRAM);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertEquals(new TreeSet<String>(Arrays.asList("HELLO")), index.getPrograms().keySet());
		assertTrue(index.getCopybookFiles().isEmpty());
	}

	@Test
	void fileWithOnlyLevel01ItemsIsACopybookWhateverItIsCalledAndWhereverItIs() throws IOException {
		put("src/layout.cob", COPYBOOK);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertTrue(index.getPrograms().isEmpty());
		assertTrue(index.hasCopybook("layout"));
		// Against both its name and its folder: worth a note in the report.
		assertTrue(index.getWarnings().toString().contains("src/layout.cob is indexed as a copybook"),
				index.getWarnings().toString());
	}

	@Test
	void copybookInACopybookFolderNeedsNoWarning() throws IOException {
		put("COPY/VFILABC.COB", COPYBOOK);
		assertTrue(scan(StandardCharsets.UTF_8).getWarnings().isEmpty());
	}

	@Test
	void programIdInACommentDoesNotMakeAProgram() throws IOException {
		put("COPY/FAKE.COB", text(
				"      * PROGRAM-ID. FAKE.",
				"      * IDENTIFICATION DIVISION.",
				"      / PROGRAM-ID. FAKE.",
				"000100* PROGRAM-ID. FAKE.",
				"       01  REC.",
				"           02  REC-ID    PIC X(8)."));
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertTrue(index.getPrograms().isEmpty());
		assertTrue(index.hasCopybook("FAKE"));
	}

	@Test
	void programIdWithoutAnIdentificationDivisionIsAProgramEvenInACopybookFolder() throws IOException {
		put("COPY/ODD.COB", text(
				"       PROGRAM-ID. ODD.",
				"       PROCEDURE DIVISION.",
				"           STOP RUN."));
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertEquals(new TreeSet<String>(Arrays.asList("ODD")), index.getPrograms().keySet());
		assertFalse(index.hasCopybook("ODD"));
	}

	@Test
	void idDivisionAloneMakesAProgram() throws IOException {
		put("short.cbl", text(
				"000100 ID DIVISION.",
				"000200 PROGRAM-ID. SHORTY.",
				"000300 PROCEDURE DIVISION.",
				"000400     STOP RUN."));
		assertEquals(new TreeSet<String>(Arrays.asList("SHORTY")), scan(StandardCharsets.UTF_8).getPrograms().keySet());
	}

	@Test
	void freeFormatProgramStartingInColumn1IsAProgram() throws IOException {
		put("free.cob", text(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-ID. FREEPROG.",
				"PROCEDURE DIVISION.",
				"    STOP RUN."));
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertEquals(1, index.getProgramFiles().size());
		assertTrue(index.getCopybookFiles().isEmpty());
	}

	@Test
	void dataItemCalledProgramIdDoesNotMakeAProgram() throws IOException {
		put("COPY/ITEMS.cpy", text(
				"       01  CALLER-INFO.",
				"           05  PROGRAM-ID   PIC X(8).",
				"           05  PROGRAM-ID-2 PIC X(8)."));
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertTrue(index.getPrograms().isEmpty());
		assertTrue(index.hasCopybook("ITEMS"));
	}

	@Test
	void cpyFileHoldingAProgramIsAProgram() throws IOException {
		put("odd.cpy", PROGRAM);
		assertEquals(new TreeSet<String>(Arrays.asList("HELLO")), scan(StandardCharsets.UTF_8).getPrograms().keySet());
	}

	@Test
	void shiftJisFilesAreClassifiedWithTheEncodingOfTheRun() throws IOException {
		String japanese = new String(new char[] { 0x65e5, 0x672c, 0x8a9e }); // nihongo
		put("src/jp.cob", text(
				"      * " + japanese,
				"       IDENTIFICATION DIVISION.",
				"       PROGRAM-ID. JPPROG.",
				"       PROCEDURE DIVISION.",
				"           DISPLAY '" + japanese + "'.",
				"           STOP RUN."), MS932);
		put("COPY/JPCOPY.COB", text(
				"      * " + japanese,
				"       01  REC.",
				"           02  REC-NAME  PIC X(8) VALUE '" + japanese + "'."), MS932);
		ProjectIndex index = scan(MS932);
		assertEquals(new TreeSet<String>(Arrays.asList("JPPROG")), index.getPrograms().keySet());
		assertTrue(index.hasCopybook("JPCOPY"));
		assertTrue(index.getWarnings().isEmpty(), index.getWarnings().toString());
	}

	@Test
	void aByteThatDoesNotDecodeDoesNotDecideWhatTheFileIs() throws IOException {
		// Read as UTF-8 the Shift-JIS comment holds bytes that are not UTF-8; it is still a program.
		String japanese = new String(new char[] { 0x65e5, 0x672c, 0x8a9e });
		put("src/jp.cob", text(
				"      * " + japanese,
				"       IDENTIFICATION DIVISION.",
				"       PROGRAM-ID. JPPROG.",
				"       PROCEDURE DIVISION.",
				"           STOP RUN."), MS932);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertEquals(1, index.getProgramFiles().size());
		assertTrue(index.getCopybookFiles().isEmpty());
	}

	@Test
	void extensionsAreMatchedWithoutRegardToCase() throws IOException {
		put("COPY/UPPER.CPY", COPYBOOK);
		put("prog.COB", PROGRAM);
		ProjectIndex index = scan(StandardCharsets.UTF_8);
		assertEquals(new TreeSet<String>(Arrays.asList("prog.COB")), names(index.getProgramFiles()));
		assertTrue(index.hasCopybook("UPPER"));
	}
}
