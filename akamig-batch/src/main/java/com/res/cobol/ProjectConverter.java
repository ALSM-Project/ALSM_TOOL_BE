package com.res.cobol;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.res.common.CobolTranslationException;
import com.res.common.RESConfig;

/**
 * Converts a whole COBOL project. Pass 1 ({@link ProjectIndex}) indexes programs, copybooks and
 * calls; pass 2 translates each program on its own. A program that cannot be translated is
 * recorded and replaced by a stub that fails with the reason when it is run, so one broken
 * program never stops the rest. The outcome is written to conversion_report.json and .md.
 */
public class ProjectConverter {

	/** What happened to one program. */
	static class Outcome {
		String programId;
		String file;
		boolean success;
		boolean subprogram;
		int javaFiles;
		String message;
		int line = -1;
	}

	private final Main main;
	private final Path projectRoot;
	private final Path outputDir;
	private final String basePackage;
	private final boolean scaffold;

	ProjectConverter(Main main, String projectRoot, String outputDir, String basePackage, boolean scaffold) {
		this.main = main;
		this.projectRoot = Paths.get(projectRoot).toAbsolutePath().normalize();
		this.outputDir = outputDir == null || outputDir.trim().isEmpty() ? null
				: Paths.get(outputDir.trim()).toAbsolutePath().normalize();
		this.basePackage = basePackage;
		this.scaffold = scaffold;
	}

	/** Returns 0 when the run completed (even if some programs failed), 1 when it could not start. */
	int run() throws IOException {
		if (!Files.isDirectory(projectRoot))
			throw new CobolTranslationException("Project folder not found: " + projectRoot);
		if (outputDir == null)
			throw new CobolTranslationException("-odir <output folder> is required with -project");

		System.out.println("Pass 1: indexing " + projectRoot);
		ProjectIndex index = ProjectIndex.scan(projectRoot, Main.getSourceCharset());
		Main.setProjectIndex(index);
		System.out.println("  " + index.getProgramFiles().size() + " program file(s), " + index.getCopybookFiles().size()
				+ " copybook(s), " + index.getPrograms().size() + " PROGRAM-ID(s)");
		for (String warning : index.getWarnings())
			System.out.println("  warning: " + warning);

		RESConfig config = RESConfig.getInstance();
		String programPackage = basePackage + ".jobs";
		config.setProgramPackage(programPackage);
		config.setDataPackage(programPackage);
		config.setOverwriteJavaFiles(true);

		Path javaRoot = outputDir.resolve("src").resolve("main").resolve("java");
		Files.createDirectories(javaRoot);

		System.out.println("Pass 2: translating");
		List<Outcome> outcomes = new ArrayList<Outcome>();
		for (Map.Entry<String, ProjectIndex.ProgramInfo> entry : index.getPrograms().entrySet())
			outcomes.add(translate(entry.getValue(), javaRoot, programPackage));

		int ok = 0;
		for (Outcome outcome : outcomes)
			if (outcome.success)
				ok++;
		writeReport(index, outcomes);
		System.out.println("Converted " + ok + " of " + outcomes.size() + " program(s). Report: "
				+ outputDir.resolve("conversion_report.md"));
		if (scaffold)
			System.out.println("Note: -scaffold is not available in this build; the Java sources were written to " + javaRoot);
		return 0;
	}

	private static String folderName(String programId) {
		return programId.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9_]", "");
	}

	private Outcome translate(ProjectIndex.ProgramInfo info, Path javaRoot, String programPackage) {
		Outcome outcome = new Outcome();
		outcome.programId = info.programId;
		outcome.file = projectRoot.relativize(info.file.toAbsolutePath().normalize()).toString().replace('\\', '/');
		outcome.subprogram = info.isSubprogram();
		Path programDir = javaRoot.resolve(programPackage.replace('.', File.separatorChar)).resolve(folderName(info.programId));
		System.out.println("  " + info.programId + " (" + outcome.file + ")");
		try {
			deleteQuietly(programDir);
			Files.createDirectories(programDir);
			// The translator writes into whatever output directory is set when a class file is opened.
			RESConfig.getInstance().setOutputDir(programDir.toString() + File.separator);
			main.translateProgram(info.file.toFile());
			outcome.javaFiles = countJava(programDir);
			if (outcome.javaFiles == 0)
				throw new CobolTranslationException("No Java files were generated", info.file.toString(), -1);
			outcome.success = true;
		} catch (CobolTranslationException e) {
			fail(outcome, programDir, programPackage, info, e.getMessage(), e.getLine());
		} catch (Throwable t) {
			// A translator bug (null pointer, stack overflow) is one program's failure, not the project's.
			fail(outcome, programDir, programPackage, info, t.getClass().getSimpleName() + ": " + t.getMessage(), -1);
		} finally {
			RESConfig.getInstance().setOutputDir("");
		}
		return outcome;
	}

	private void fail(Outcome outcome, Path programDir, String programPackage, ProjectIndex.ProgramInfo info,
			String message, int line) {
		outcome.success = false;
		outcome.message = message == null ? "unknown error" : message;
		outcome.line = line;
		System.out.println("    FAILED: " + outcome.message + (line > 0 ? " (line " + line + ")" : ""));
		try {
			deleteQuietly(programDir);
			Files.createDirectories(programDir);
			writeStub(programDir, programPackage, info, outcome);
			outcome.javaFiles = countJava(programDir);
		} catch (IOException e) {
			outcome.message += " (stub not written: " + e.getMessage() + ")";
		}
	}

	private static int countJava(Path dir) throws IOException {
		int[] count = { 0 };
		if (!Files.isDirectory(dir))
			return 0;
		try (java.util.stream.Stream<Path> walk = Files.walk(dir)) {
			walk.filter(p -> p.toString().endsWith(".java")).forEach(p -> count[0]++);
		}
		return count[0];
	}

	private static void deleteQuietly(Path dir) {
		if (!Files.isDirectory(dir))
			return;
		try (java.util.stream.Stream<Path> walk = Files.walk(dir)) {
			walk.sorted(java.util.Comparator.reverseOrder()).forEach(p -> p.toFile().delete());
		} catch (IOException ignored) {
			// leftovers are overwritten by the next write
		}
	}

	private static String className(String programId) {
		StringBuilder name = new StringBuilder();
		for (String part : programId.split("[^A-Za-z0-9]+"))
			if (!part.isEmpty())
				name.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1).toLowerCase(Locale.ROOT));
		return name.length() == 0 ? "Program" : name.toString();
	}

	private static String javaString(String text) {
		return text.replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", " ").replace("\n", " ");
	}

	/**
	 * A job and tasklet for a program that could not be translated. The job exists under the program's
	 * name, so callers and job lists still see it, and running it fails with the reason.
	 */
	private void writeStub(Path programDir, String programPackage, ProjectIndex.ProgramInfo info, Outcome outcome)
			throws IOException {
		String cls = className(info.programId);
		String pkg = programPackage + "." + folderName(info.programId);
		String reason = javaString(outcome.message) + (outcome.line > 0 ? " (line " + outcome.line + ")" : "");
		String tasklet = "package " + pkg + ";\n\n"
				+ "import org.springframework.batch.core.StepContribution;\n"
				+ "import org.springframework.batch.core.scope.context.ChunkContext;\n"
				+ "import org.springframework.batch.core.step.tasklet.Tasklet;\n"
				+ "import org.springframework.batch.repeat.RepeatStatus;\n"
				+ "import org.springframework.stereotype.Component;\n\n"
				+ "/**\n * Stand-in for the COBOL program " + info.programId + ", which could not be converted.\n"
				+ " * Source: " + outcome.file + "\n * Reason: " + reason.replace("*/", "* /") + "\n */\n"
				+ "@Component\n"
				+ "public class " + cls + "Tasklet implements Tasklet {\n\n"
				+ "  @Override\n"
				+ "  public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {\n"
				+ "    throw new UnsupportedOperationException(\"COBOL program " + info.programId
				+ " was not converted: " + reason + "\");\n"
				+ "  }\n\n"
				+ "  public Object execute(Object... arguments) {\n"
				+ "    throw new UnsupportedOperationException(\"COBOL program " + info.programId
				+ " was not converted: " + reason + "\");\n"
				+ "  }\n"
				+ "}\n";
		String job = "package " + pkg + ";\n\n"
				+ "import org.springframework.batch.core.Job;\n"
				+ "import org.springframework.batch.core.Step;\n"
				+ "import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;\n"
				+ "import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;\n"
				+ "import org.springframework.batch.core.launch.support.RunIdIncrementer;\n"
				+ "import org.springframework.beans.factory.annotation.Autowired;\n"
				+ "import org.springframework.context.annotation.Bean;\n"
				+ "import org.springframework.context.annotation.Configuration;\n\n"
				+ "@Configuration\n"
				+ "public class " + cls + "Job {\n\n"
				+ "  @Autowired\n  private JobBuilderFactory jobs;\n\n"
				+ "  @Autowired\n  private StepBuilderFactory steps;\n\n"
				+ "  @Bean(name = \"" + cls + "\")\n"
				+ "  public Job " + Character.toLowerCase(cls.charAt(0)) + cls.substring(1) + "(" + cls + "Tasklet tasklet) {\n"
				+ "    Step step = steps.get(\"" + cls + "Step\").tasklet(tasklet).build();\n"
				+ "    return jobs.get(\"" + cls + "\").incrementer(new RunIdIncrementer()).start(step).build();\n"
				+ "  }\n"
				+ "}\n";
		Files.write(programDir.resolve(cls + "Tasklet.java"), tasklet.getBytes(StandardCharsets.UTF_8));
		Files.write(programDir.resolve(cls + "Job.java"), job.getBytes(StandardCharsets.UTF_8));
	}

	// ------------------------------------------------------------------ report

	private static String json(String text) {
		if (text == null)
			return "null";
		StringBuilder out = new StringBuilder("\"");
		for (char c : text.toCharArray()) {
			switch (c) {
			case '"':
				out.append("\\\"");
				break;
			case '\\':
				out.append("\\\\");
				break;
			case '\n':
				out.append("\\n");
				break;
			case '\r':
				out.append("\\r");
				break;
			case '\t':
				out.append("\\t");
				break;
			default:
				if (c < 0x20)
					out.append(String.format("\\u%04x", (int) c));
				else
					out.append(c);
			}
		}
		return out.append('"').toString();
	}

	private void writeReport(ProjectIndex index, List<Outcome> outcomes) throws IOException {
		int ok = 0;
		for (Outcome o : outcomes)
			if (o.success)
				ok++;

		StringBuilder j = new StringBuilder("{\n");
		j.append("  \"projectRoot\": ").append(json(projectRoot.toString())).append(",\n");
		j.append("  \"basePackage\": ").append(json(basePackage)).append(",\n");
		j.append("  \"programFiles\": ").append(index.getProgramFiles().size()).append(",\n");
		j.append("  \"copybooks\": ").append(index.getCopybookFiles().size()).append(",\n");
		j.append("  \"programs\": ").append(outcomes.size()).append(",\n");
		j.append("  \"converted\": ").append(ok).append(",\n");
		j.append("  \"failed\": ").append(outcomes.size() - ok).append(",\n");
		j.append("  \"succeeded\": [");
		boolean first = true;
		for (Outcome o : outcomes) {
			if (!o.success)
				continue;
			j.append(first ? "\n" : ",\n").append("    {\"programId\": ").append(json(o.programId)).append(", \"file\": ")
					.append(json(o.file)).append(", \"role\": ").append(json(o.subprogram ? "subprogram" : "main"))
					.append(", \"javaFiles\": ").append(o.javaFiles).append("}");
			first = false;
		}
		j.append(first ? "],\n" : "\n  ],\n");
		j.append("  \"failures\": [");
		first = true;
		for (Outcome o : outcomes) {
			if (o.success)
				continue;
			j.append(first ? "\n" : ",\n").append("    {\"programId\": ").append(json(o.programId)).append(", \"file\": ")
					.append(json(o.file)).append(", \"line\": ").append(o.line).append(", \"reason\": ")
					.append(json(o.message)).append("}");
			first = false;
		}
		j.append(first ? "],\n" : "\n  ],\n");
		j.append("  \"unresolvedCalls\": [");
		first = true;
		for (String name : index.unresolvedCalls()) {
			j.append(first ? "" : ", ").append(json(name));
			first = false;
		}
		j.append("],\n");
		j.append("  \"warnings\": [");
		first = true;
		for (String w : index.getWarnings()) {
			j.append(first ? "" : ", ").append(json(w));
			first = false;
		}
		j.append("]\n}\n");
		Files.write(outputDir.resolve("conversion_report.json"), j.toString().getBytes(StandardCharsets.UTF_8));

		StringBuilder m = new StringBuilder("# COBOL conversion report\n\n");
		m.append("- Project: `").append(projectRoot).append("`\n");
		m.append("- Program files scanned: ").append(index.getProgramFiles().size()).append("\n");
		m.append("- Copybooks indexed: ").append(index.getCopybookFiles().size()).append("\n");
		m.append("- Converted completely: **").append(ok).append(" of ").append(outcomes.size()).append("**\n");
		m.append("- Failed (a stub was written for each): ").append(outcomes.size() - ok).append("\n\n");
		m.append("## Converted programs\n\n");
		if (ok == 0)
			m.append("None.\n");
		else {
			m.append("| Program | Role | Source | Java files |\n|---|---|---|---|\n");
			for (Outcome o : outcomes)
				if (o.success)
					m.append("| ").append(o.programId).append(" | ").append(o.subprogram ? "subprogram" : "main job")
							.append(" | ").append(o.file).append(" | ").append(o.javaFiles).append(" |\n");
		}
		m.append("\n## Failed programs\n\n");
		if (ok == outcomes.size())
			m.append("None.\n");
		else {
			m.append("| Program | Source | Line | Reason |\n|---|---|---|---|\n");
			for (Outcome o : outcomes)
				if (!o.success)
					m.append("| ").append(o.programId).append(" | ").append(o.file).append(" | ")
							.append(o.line > 0 ? String.valueOf(o.line) : "-").append(" | ")
							.append(o.message.replace("|", "\\|").replace("\n", " ")).append(" |\n");
		}
		if (!index.unresolvedCalls().isEmpty()) {
			m.append("\n## Called but not in the project\n\n");
			for (String name : index.unresolvedCalls())
				m.append("- ").append(name).append("\n");
		}
		if (!index.getWarnings().isEmpty()) {
			m.append("\n## Warnings\n\n");
			for (String w : index.getWarnings())
				m.append("- ").append(w).append("\n");
		}
		Files.write(outputDir.resolve("conversion_report.md"), m.toString().getBytes(StandardCharsets.UTF_8));
	}
}
