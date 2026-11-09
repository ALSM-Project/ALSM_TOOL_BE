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
	private final boolean verifyBuild;
	/** Lower-case names (PROGRAM-ID, file name with or without extension) of the only programs to translate; empty = all. */
	private java.util.Set<String> only = new java.util.HashSet<String>();

	ProjectConverter(Main main, String projectRoot, String outputDir, String basePackage, boolean scaffold,
			boolean verifyBuild) {
		this.main = main;
		this.projectRoot = Paths.get(projectRoot).toAbsolutePath().normalize();
		this.outputDir = outputDir == null || outputDir.trim().isEmpty() ? null
				: Paths.get(outputDir.trim()).toAbsolutePath().normalize();
		this.basePackage = basePackage;
		this.scaffold = scaffold;
		this.verifyBuild = verifyBuild;
	}

	/**
	 * Restricts pass 2 to the named programs. The whole project is still indexed, so copybooks and
	 * called programs resolve exactly as in a full run; only the translation is skipped for the rest.
	 */
	void setOnly(java.util.Collection<String> names) {
		only = new java.util.HashSet<String>();
		for (String name : names)
			if (name != null && !name.trim().isEmpty())
				only.add(name.trim().toLowerCase(Locale.ROOT));
	}

	private boolean selected(ProjectIndex.ProgramInfo info) {
		if (only.isEmpty())
			return true;
		String file = info.file.getFileName().toString().toLowerCase(Locale.ROOT);
		int dot = file.lastIndexOf('.');
		return only.contains(info.programId.toLowerCase(Locale.ROOT)) || only.contains(file)
				|| (dot > 0 && only.contains(file.substring(0, dot)));
	}

	/** Returns 0 when the run completed (even if some programs failed), 1 when it could not start. */
	int run() throws IOException {
		if (!Files.isDirectory(projectRoot))
			throw new CobolTranslationException("Project folder not found: " + projectRoot);
		if (outputDir == null)
			throw new CobolTranslationException("-odir <output folder> is required with -project");

		// The sources are converted from a normalized copy (CICS blocks rewritten, see
		// CicsNormalizer); the project folder itself is never modified.
		Path sourceRoot = Files.createTempDirectory("alsm-project-");
		try {
			return run(sourceRoot);
		} finally {
			deleteQuietly(sourceRoot);
		}
	}

	/** Copies the COBOL files of the project into workRoot, normalizing each one. */
	private int normalizeInto(Path workRoot) throws IOException {
		final java.nio.charset.Charset cs = Main.getSourceCharset();
		final int[] rewritten = { 0 };
		Files.walkFileTree(projectRoot, new java.nio.file.SimpleFileVisitor<Path>() {
			@Override
			public java.nio.file.FileVisitResult preVisitDirectory(Path dir, java.nio.file.attribute.BasicFileAttributes a) {
				String name = dir.getFileName() == null ? "" : dir.getFileName().toString();
				if (!dir.equals(projectRoot) && (name.equals(".git") || name.equals("target") || name.equals("node_modules")))
					return java.nio.file.FileVisitResult.SKIP_SUBTREE;
				return java.nio.file.FileVisitResult.CONTINUE;
			}

			@Override
			public java.nio.file.FileVisitResult visitFile(Path file, java.nio.file.attribute.BasicFileAttributes a)
					throws IOException {
				Path target = workRoot.resolve(projectRoot.relativize(file).toString());
				Files.createDirectories(target.getParent());
				String name = file.getFileName().toString();
				int dot = name.lastIndexOf('.');
				String ext = dot < 0 ? "" : name.substring(dot + 1).toLowerCase(Locale.ROOT);
				if (ProjectIndex.PROGRAM_EXTENSIONS.contains(ext) || ProjectIndex.COPYBOOK_EXTENSIONS.contains(ext)) {
					String text = new String(Files.readAllBytes(file), cs);
					CicsNormalizer.Result result = CicsNormalizer.normalize(text);
					hoists.putAll(result.hoists);
					needsSupport |= result.needsSupportCopybook;
					if (!result.changes.isEmpty()) {
						rewritten[0]++;
						normalizationChanges.put(projectRoot.relativize(file).toString().replace('\\', '/'),
								result.changes.size());
					}
					Files.write(target, result.source.getBytes(cs));
				} else if (dot < 0 && a.size() < 1024 * 1024) {
					// An extensionless file may be a copybook in an include folder.
					Files.copy(file, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
				}
				return java.nio.file.FileVisitResult.CONTINUE;
			}
		});
		return rewritten[0];
	}

	private final Map<String, String> hoists = new java.util.TreeMap<String, String>();
	private boolean needsSupport;
	private final Map<String, Integer> normalizationChanges = new java.util.TreeMap<String, Integer>();
	private final List<String> systemCopybooks = new ArrayList<String>();
	private Path sourceRoot;

	private int run(Path workRoot) throws IOException {
		this.sourceRoot = workRoot;
		int rewritten = normalizeInto(workRoot);
		System.out.println("Normalized " + rewritten + " file(s) (CICS blocks, separators, empty sentences)");

		System.out.println("Pass 1: indexing " + projectRoot);
		ProjectIndex index = ProjectIndex.scan(workRoot, Main.getSourceCharset());
		// Stand-ins for system copybooks a project never ships (CICS, DB2); a real one wins.
		Path systemDir = workRoot.resolve(".alsm-system-copybooks");
		Files.createDirectories(systemDir);
		for (Map.Entry<String, String> stub : CicsNormalizer.systemStubs(hoists, needsSupport).entrySet()) {
			if (index.hasCopybook(stub.getKey()))
				continue;
			Path file = systemDir.resolve(stub.getKey() + ".cpy");
			Files.write(file, stub.getValue().getBytes(Main.getSourceCharset()));
			index.addSystemCopybook(stub.getKey(), file);
			index.addSystemCopybook(stub.getKey() + ".cpy", file);
			systemCopybooks.add(stub.getKey());
		}
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
			if (selected(entry.getValue()))
				outcomes.add(translate(entry.getValue(), javaRoot, programPackage));
		if (!only.isEmpty() && outcomes.isEmpty())
			throw new CobolTranslationException("No program of the project matches -only " + only);

		if (scaffold) {
			writeScaffold(outcomes);
			if (verifyBuild && verifyBuild(index, outcomes, javaRoot, programPackage))
				writeScaffold(outcomes);// the default job may have become a stub
		}
		int ok = 0;
		for (Outcome outcome : outcomes)
			if (outcome.success)
				ok++;
		writeReport(index, outcomes);
		System.out.println("Converted " + ok + " of " + outcomes.size() + " program(s). Report: "
				+ outputDir.resolve("conversion_report.md"));
		if (scaffold)
			System.out.println("Maven project written to " + outputDir + " - build it with: mvn clean package -DskipTests");
		return 0;
	}

	private void writeScaffold(List<Outcome> outcomes) throws IOException {
		List<String> jobs = new ArrayList<String>();
		String defaultJob = null;
		for (Outcome outcome : outcomes) {
			jobs.add(className(outcome.programId));
			if (defaultJob == null && outcome.success && !outcome.subprogram)
				defaultJob = className(outcome.programId);
		}
		String projectName = projectRoot.getFileName() == null ? "cobol-project" : projectRoot.getFileName().toString();
		ProjectScaffold.write(outputDir, projectName, basePackage, jobs, defaultJob);
	}

	/**
	 * Compiles the written Maven project and replaces each program whose generated Java does not
	 * compile with a stub, until the project compiles (a few rounds at most: one broken program
	 * can hide errors of another). Returns true when a program was stubbed. Without Maven on the
	 * PATH the check is skipped with a note.
	 */
	private boolean verifyBuild(ProjectIndex index, List<Outcome> outcomes, Path javaRoot, String programPackage) {
		boolean windows = System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
		String packagePath = programPackage.replace('.', '/') + "/";
		java.util.regex.Pattern error = java.util.regex.Pattern.compile(
				"\\[ERROR\\].*?" + java.util.regex.Pattern.quote(packagePath) + "([a-z0-9_]+)/[\\w$]+\\.java:\\[\\d+,\\d+\\]\\s*(.*)");
		boolean changed = false;
		for (int round = 1; round <= 5; round++) {
			System.out.println("Build check " + round + ": mvn compile");
			List<String> output = new ArrayList<String>();
			int code;
			try {
				ProcessBuilder pb = new ProcessBuilder(windows ? "mvn.cmd" : "mvn", "-B", "-q", "clean", "compile")
						.directory(outputDir.toFile()).redirectErrorStream(true);
				Process process = pb.start();
				try (java.io.BufferedReader reader = new java.io.BufferedReader(
						new java.io.InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
					String line;
					while ((line = reader.readLine()) != null)
						output.add(line);
				}
				code = process.waitFor();
			} catch (IOException e) {
				System.out.println("  skipped: Maven (mvn) was not found on the PATH");
				return changed;
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
				return changed;
			}
			if (code == 0) {
				System.out.println("  BUILD SUCCESS");
				return changed;
			}
			Map<String, String> broken = new java.util.LinkedHashMap<String, String>();
			for (String line : output) {
				java.util.regex.Matcher m = error.matcher(line.replace('\\', '/'));
				if (m.find() && !broken.containsKey(m.group(1)))
					broken.put(m.group(1), m.group(2).trim());
			}
			if (broken.isEmpty()) {
				System.out.println("  the build failed outside the converted programs:");
				for (int i = 0; i < output.size() && i < 30; i++)
					System.out.println("  " + output.get(i));
				return changed;
			}
			for (Outcome outcome : outcomes) {
				String folder = folderName(outcome.programId);
				if (!broken.containsKey(folder))
					continue;
				ProjectIndex.ProgramInfo info = index.getPrograms().get(outcome.programId);
				if (info == null)
					for (ProjectIndex.ProgramInfo candidate : index.getPrograms().values())
						if (candidate.programId.equals(outcome.programId))
							info = candidate;
				if (info == null)
					continue;
				Path programDir = javaRoot.resolve(programPackage.replace('.', File.separatorChar)).resolve(folder);
				fail(outcome, programDir, programPackage, info,
						"the generated Java does not compile: " + broken.get(folder), -1);
				changed = true;
			}
		}
		System.out.println("  the build still fails after 5 rounds; see mvn compile in " + outputDir);
		return changed;
	}

	private static String folderName(String programId) {
		return programId.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9_]", "");
	}

	private Outcome translate(ProjectIndex.ProgramInfo info, Path javaRoot, String programPackage) {
		Outcome outcome = new Outcome();
		outcome.programId = info.programId;
		// Paths in the report are the project's own: the normalized copy keeps the same layout.
		outcome.file = sourceRoot.toAbsolutePath().normalize().relativize(info.file.toAbsolutePath().normalize())
				.toString().replace('\\', '/');
		outcome.subprogram = info.isSubprogram();
		Path programDir = javaRoot.resolve(programPackage.replace('.', File.separatorChar)).resolve(folderName(info.programId));
		System.out.println("  " + info.programId + " (" + outcome.file + ")");
		try {
			deleteQuietly(programDir);
			Files.createDirectories(programDir);
			// The translator writes into whatever output directory is set when a class file is opened.
			RESConfig.getInstance().setOutputDir(programDir.toString() + File.separator);
			main.translateProgram(info.file.toFile());
			completeOutput(programDir, info);
			outcome.javaFiles = countJava(programDir);
			if (outcome.javaFiles == 0)
				throw new CobolTranslationException("No Java files were generated", info.file.toString(), -1);
			outcome.success = true;
		} catch (CobolTranslationException e) {
			// A wrapped Java exception is a translator bug; its trace says where.
			if (e.getCause() != null && !(e.getCause() instanceof com.res.cobol.parser.ParseException))
				e.getCause().printStackTrace();
			fail(outcome, programDir, programPackage, info, e.getMessage(), e.getLine());
		} catch (Throwable t) {
			// A translator bug (null pointer, stack overflow) is one program's failure, not the project's.
			// The trace goes to stderr: it points at the translator, not at the COBOL.
			t.printStackTrace();
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

	private static final java.util.regex.Pattern MODEL_FIELD = java.util.regex.Pattern
			.compile("private\\s+(\\w+Model)\\s+model\\s*;");
	private static final java.util.regex.Pattern PACKAGE = java.util.regex.Pattern
			.compile("(?m)^\\s*package\\s+([\\w.]+)\\s*;");

	/**
	 * A translation that stopped part-way can leave a Job without its Tasklet; that program has not
	 * been converted. A program without WORKING-STORAGE has no Model although its Tasklet still
	 * injects one, so an empty Model is supplied.
	 */
	private static void completeOutput(Path programDir, ProjectIndex.ProgramInfo info) throws IOException {
		Path job = null;
		Path tasklet = null;
		try (java.util.stream.Stream<Path> files = Files.list(programDir)) {
			for (Path p : (Iterable<Path>) files::iterator) {
				String name = p.getFileName().toString();
				if (name.endsWith("Job.java"))
					job = p;
				else if (name.endsWith("Tasklet.java"))
					tasklet = p;
			}
		}
		if (tasklet == null || job == null)
			throw new CobolTranslationException("Incomplete output: " + (tasklet == null ? "no Tasklet" : "no Job")
					+ " was generated", info.file.toString(), -1);
		String source = new String(Files.readAllBytes(tasklet), StandardCharsets.UTF_8);
		java.util.regex.Matcher model = MODEL_FIELD.matcher(source);
		if (model.find() && !Files.exists(programDir.resolve(model.group(1) + ".java"))) {
			java.util.regex.Matcher pkg = PACKAGE.matcher(source);
			String generated = (pkg.find() ? "package " + pkg.group(1) + ";\n\n" : "")
					+ "import java.math.BigDecimal;\n\n"
					+ "import org.springframework.stereotype.Component;\n\n"
					+ "import lombok.Getter;\nimport lombok.Setter;\n\n"
					+ "/**\n * Data of " + info.programId + ". The translator only writes a Model for programs that\n"
					+ " * declare a group; this one has only elementary 01/77 items, listed here.\n */\n"
					+ "@Component\n@Getter\n@Setter\npublic class " + model.group(1) + " {\n"
					+ elementaryFields(info.file) + "}\n";
			Files.write(programDir.resolve(model.group(1) + ".java"), generated.getBytes(StandardCharsets.UTF_8));
		}
	}

	private static final java.util.regex.Pattern ELEMENTARY = java.util.regex.Pattern.compile(
			"(?i)^\\s*(01|77)\\s+([A-Z0-9][A-Z0-9-]*)\\s+PIC(?:TURE)?\\s+(?:IS\\s+)?(\\S+?)\\.?(\\s.*)?$");
	private static final java.util.regex.Pattern VALUE = java.util.regex.Pattern
			.compile("(?i)\\bVALUE\\s+(?:IS\\s+)?('[^']*'|\"[^\"]*\"|[+-]?\\d+(?:\\.\\d+)?|ZEROS?|ZEROES|SPACES?)");

	/** Lombok fields for the 01/77 elementary items of WORKING-STORAGE and LINKAGE. */
	private static String elementaryFields(Path cobol) throws IOException {
		StringBuilder fields = new StringBuilder();
		boolean inData = false;
		for (String raw : Files.readAllLines(cobol, Main.getSourceCharset())) {
			if (raw.length() > 6 && (raw.charAt(6) == '*' || raw.charAt(6) == '/'))
				continue;
			String line = raw.length() > 72 ? raw.substring(0, 72) : raw;
			line = line.length() > 7 ? line.substring(7) : "";
			String upper = line.trim().toUpperCase(Locale.ROOT);
			if (upper.startsWith("WORKING-STORAGE SECTION") || upper.startsWith("LINKAGE SECTION")
					|| upper.startsWith("LOCAL-STORAGE SECTION"))
				inData = true;
			else if (upper.startsWith("PROCEDURE DIVISION"))
				break;
			if (!inData)
				continue;
			java.util.regex.Matcher m = ELEMENTARY.matcher(line);
			if (!m.matches())
				continue;
			String pic = m.group(3).toUpperCase(Locale.ROOT);
			String type;
			if (pic.contains("X") || pic.contains("A"))
				type = "String";
			else if (pic.contains("V"))
				type = "BigDecimal";
			else
				type = digits(pic) > 9 ? "long" : "int";
			String name = javaName(m.group(2));
			String init = "";
			java.util.regex.Matcher v = VALUE.matcher(m.group(4) == null ? "" : m.group(4));
			if (v.find()) {
				String value = v.group(1);
				String upperValue = value.toUpperCase(Locale.ROOT);
				if (type.equals("String"))
					init = upperValue.startsWith("SPACE") ? " = \" \""
							: value.startsWith("'") || value.startsWith("\"")
									? " = \"" + value.substring(1, value.length() - 1).replace("\\", "\\\\").replace("\"", "\\\"") + "\""
									: "";
				else if (upperValue.startsWith("ZERO"))
					init = type.equals("BigDecimal") ? " = BigDecimal.ZERO" : " = 0";
				else if (value.matches("[+-]?\\d+(\\.\\d+)?"))
					init = type.equals("BigDecimal") ? " = new BigDecimal(\"" + value + "\")"
							: " = " + value + (type.equals("long") ? "L" : "");
			} else if (type.equals("BigDecimal"))
				init = " = BigDecimal.ZERO";
			else if (type.equals("String"))
				init = " = \"\"";
			fields.append("\n  private ").append(type).append(' ').append(name).append(init).append(";\n");
			if (!type.equals("String"))
				fields.append(numericSetters(type, name));
		}
		return fields.toString();
	}

	/**
	 * COBOL moves text into numbers (MOVE PARM-VALUE TO WAIT-TIME), so a numeric item also takes a
	 * String and the other numeric types. Lombok writes no setter once one of that name exists, so
	 * all of them are written here.
	 */
	private static String numericSetters(String type, String name) {
		String setter = "set" + Character.toUpperCase(name.charAt(0)) + name.substring(1);
		String fromDecimal = type.equals("int") ? "value.intValue()" : type.equals("long") ? "value.longValue()" : "value";
		StringBuilder s = new StringBuilder();
		s.append("\n  public void ").append(setter).append("(BigDecimal value) {\n    this.").append(name).append(" = ")
				.append(fromDecimal).append(";\n  }\n");
		s.append("\n  public void ").append(setter).append("(String value) {\n    String text = value == null ? \"\" : value.trim();\n    ")
				.append(setter).append("(text.isEmpty() || !text.matches(\"[+-]?\\\\d*(\\\\.\\\\d+)?\") ? BigDecimal.ZERO : new BigDecimal(text));\n  }\n");
		s.append("\n  public void ").append(setter).append("(long value) {\n    ").append(setter)
				.append("(BigDecimal.valueOf(value));\n  }\n");
		s.append("\n  public void ").append(setter).append("(double value) {\n    ").append(setter)
				.append("(BigDecimal.valueOf(value));\n  }\n");
		return s.toString();
	}

	/** Number of digit positions in a numeric picture: 9(8) is 8, S999V99 is 5. */
	private static int digits(String pic) {
		int count = 0;
		java.util.regex.Matcher m = java.util.regex.Pattern.compile("9\\((\\d+)\\)|9").matcher(pic);
		while (m.find())
			count += m.group(1) == null ? 1 : Integer.parseInt(m.group(1));
		return count;
	}

	/** MVSWAIT-TIME -> mvswaitTime, the field name the translator's accessors use. */
	private static String javaName(String cobolName) {
		StringBuilder name = new StringBuilder();
		for (String part : cobolName.toLowerCase(Locale.ROOT).split("-")) {
			if (part.isEmpty())
				continue;
			name.append(name.length() == 0 ? part : Character.toUpperCase(part.charAt(0)) + part.substring(1));
		}
		return name.toString();
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
	 * A job, tasklet and model for a program that could not be translated. The job exists under the
	 * program's name, so callers and job lists still see it; running or CALLing it logs a warning
	 * with the reason and does nothing, so the rest of the project builds and runs.
	 */
	private void writeStub(Path programDir, String programPackage, ProjectIndex.ProgramInfo info, Outcome outcome)
			throws IOException {
		String cls = className(info.programId);
		String pkg = programPackage + "." + folderName(info.programId);
		String reason = javaString(outcome.message) + (outcome.line > 0 ? " (line " + outcome.line + ")" : "");
		String warning = "\"COBOL program " + info.programId + " was not converted, nothing was done: " + reason + "\"";
		String tasklet = "package " + pkg + ";\n\n"
				+ "import org.slf4j.Logger;\n"
				+ "import org.slf4j.LoggerFactory;\n"
				+ "import org.springframework.batch.core.StepContribution;\n"
				+ "import org.springframework.batch.core.scope.context.ChunkContext;\n"
				+ "import org.springframework.batch.core.step.tasklet.Tasklet;\n"
				+ "import org.springframework.batch.repeat.RepeatStatus;\n"
				+ "import org.springframework.stereotype.Component;\n\n"
				+ "/**\n * Stand-in for the COBOL program " + info.programId + ", which could not be converted.\n"
				+ " * Source: " + outcome.file + "\n * Reason: " + reason.replace("*/", "* /") + "\n */\n"
				+ "@Component\n"
				+ "public class " + cls + "Tasklet implements Tasklet {\n\n"
				+ "  private static final Logger LOG = LoggerFactory.getLogger(" + cls + "Tasklet.class);\n\n"
				+ "  @Override\n"
				+ "  public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {\n"
				+ "    LOG.warn(" + warning + ");\n"
				+ "    return RepeatStatus.FINISHED;\n"
				+ "  }\n\n"
				+ "  /** CALL entry point (see CallSupport): the arguments are left as they are. */\n"
				+ "  public void call__(Object[] arguments) {\n"
				+ "    LOG.warn(" + warning + ");\n"
				+ "  }\n"
				+ "}\n";
		String model = "package " + pkg + ";\n\n"
				+ "import java.util.HashMap;\n"
				+ "import java.util.Map;\n\n"
				+ "import org.springframework.stereotype.Component;\n\n"
				+ "/** Data of the unconverted program " + info.programId + ": any item by name, unset ones read as null. */\n"
				+ "@Component\n"
				+ "public class " + cls + "Model {\n\n"
				+ "  private final Map<String, Object> items = new HashMap<String, Object>();\n\n"
				+ "  public Object get(String name) {\n"
				+ "    return items.get(name);\n"
				+ "  }\n\n"
				+ "  public void set(String name, Object value) {\n"
				+ "    items.put(name, value);\n"
				+ "  }\n"
				+ "}\n";
		Files.write(programDir.resolve(cls + "Model.java"), model.getBytes(StandardCharsets.UTF_8));
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
		j.append("  \"systemCopybookStandIns\": [");
		for (int i = 0; i < systemCopybooks.size(); i++)
			j.append(i == 0 ? "" : ", ").append(json(systemCopybooks.get(i)));
		j.append("],\n");
		j.append("  \"normalizedFiles\": {");
		boolean firstChange = true;
		for (Map.Entry<String, Integer> c : normalizationChanges.entrySet()) {
			j.append(firstChange ? "" : ", ").append(json(c.getKey())).append(": ").append(c.getValue());
			firstChange = false;
		}
		j.append("},\n");
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
		if (!systemCopybooks.isEmpty())
			m.append("- System copybooks supplied as stand-ins: ").append(String.join(", ", systemCopybooks)).append("\n");
		m.append("- Files normalized before parsing (CICS blocks, separators): ").append(normalizationChanges.size())
				.append("; the original statements stay as comments in the normalized source\n");
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
