package com.res.cobol;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Pass 1 of a project conversion: walks the whole project tree once and records what is in it.
 *
 * <ul>
 * <li>every COBOL program (.cbl .cob .cobol .pco .sqb, any case);</li>
 * <li>every copybook (.cpy .cpb .inc .copy, or a file without an extension that sits in a folder
 * called include, copybooks, copy ...), in a registry keyed by name so a COPY statement resolves
 * wherever the copybook lives;</li>
 * <li>the PROGRAM-ID of each program and the programs it CALLs, from which the roles follow: a
 * program nobody calls is a main (batch) program, the others are subprograms.</li>
 * </ul>
 */
public class ProjectIndex {

	public static final Set<String> PROGRAM_EXTENSIONS = set("cbl", "cob", "cobol", "pco", "sqb");
	public static final Set<String> COPYBOOK_EXTENSIONS = set("cpy", "cpb", "inc", "copy");
	private static final Set<String> COPYBOOK_FOLDERS = set("include", "includes", "copybook", "copybooks", "copy",
			"copylib", "cpy", "inc");

	private static final Pattern PROGRAM_ID = Pattern.compile("(?i)\\bPROGRAM-ID\\s*\\.?\\s+([A-Za-z0-9][A-Za-z0-9_\\-]*)");
	private static final Pattern STATIC_CALL = Pattern.compile("(?i)\\bCALL\\s+['\"]([^'\"]+)['\"]");
	private static final Pattern DYNAMIC_CALL = Pattern.compile("(?i)\\bCALL\\s+([A-Za-z][A-Za-z0-9_\\-]*)");

	/** One program found in the project. */
	public static class ProgramInfo {
		public final Path file;
		public final String programId;
		public final Set<String> calls = new LinkedHashSet<String>();
		public boolean dynamicCall;
		public boolean calledByOthers;

		ProgramInfo(Path file, String programId) {
			this.file = file;
			this.programId = programId;
		}

		public boolean isSubprogram() {
			return calledByOthers;
		}
	}

	private final Path root;
	private final Charset charset;
	private final List<Path> programFiles = new ArrayList<Path>();
	private final List<Path> copybookFiles = new ArrayList<Path>();
	/** name (lower case, with and without extension) to file. */
	private final Map<String, Path> copybooks = new LinkedHashMap<String, Path>();
	/** programs found by file name, used when a COPY names a source file rather than a copybook. */
	private final Map<String, Path> sourceByName = new LinkedHashMap<String, Path>();
	private final Map<String, ProgramInfo> programs = new TreeMap<String, ProgramInfo>();
	private final List<String> warnings = new ArrayList<String>();

	private ProjectIndex(Path root, Charset charset) {
		this.root = root;
		this.charset = charset;
	}

	private static Set<String> set(String... values) {
		return Collections.unmodifiableSet(new HashSet<String>(Arrays.asList(values)));
	}

	/** Walks the tree below root without limit on depth and builds the index. */
	public static ProjectIndex scan(Path root, Charset charset) throws IOException {
		final ProjectIndex index = new ProjectIndex(root.toAbsolutePath().normalize(), charset);
		Files.walkFileTree(index.root, new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
				String name = dir.getFileName() == null ? "" : dir.getFileName().toString();
				// Build output and VCS folders hold no COBOL of the project.
				if (!dir.equals(index.root) && (name.equals(".git") || name.equals("target") || name.equals("node_modules")))
					return FileVisitResult.SKIP_SUBTREE;
				return FileVisitResult.CONTINUE;
			}

			@Override
			public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
				index.classify(file);
				return FileVisitResult.CONTINUE;
			}

			@Override
			public FileVisitResult visitFileFailed(Path file, IOException exc) {
				index.warnings.add("Cannot read " + file + ": " + exc.getMessage());
				return FileVisitResult.CONTINUE;
			}
		});
		Collections.sort(index.programFiles);
		for (Path program : index.programFiles)
			index.analyse(program);
		index.linkCalls();
		return index;
	}

	private static String extensionOf(Path file) {
		String name = file.getFileName().toString();
		int dot = name.lastIndexOf('.');
		return dot < 0 ? "" : name.substring(dot + 1).toLowerCase(Locale.ROOT);
	}

	private static String baseOf(Path file) {
		String name = file.getFileName().toString();
		int dot = name.lastIndexOf('.');
		return dot < 0 ? name : name.substring(0, dot);
	}

	private boolean inCopybookFolder(Path file) {
		Path folder = root.relativize(file.toAbsolutePath().normalize()).getParent();
		if (folder == null)
			return false;
		for (Path part : folder)
			if (COPYBOOK_FOLDERS.contains(part.toString().toLowerCase(Locale.ROOT)))
				return true;
		return false;
	}

	private void classify(Path file) {
		String ext = extensionOf(file);
		if (PROGRAM_EXTENSIONS.contains(ext)) {
			programFiles.add(file);
			register(sourceByName, baseOf(file), file);
			register(sourceByName, file.getFileName().toString(), file);
		} else if (COPYBOOK_EXTENSIONS.contains(ext) || (ext.isEmpty() && inCopybookFolder(file))) {
			copybookFiles.add(file);
			register(copybooks, baseOf(file), file);
			register(copybooks, file.getFileName().toString(), file);
		}
	}

	private void register(Map<String, Path> map, String name, Path file) {
		String key = name.toLowerCase(Locale.ROOT);
		Path existing = map.get(key);
		if (existing != null && !existing.equals(file)) {
			// First one found stays; the shorter path wins so a project-level copybook beats a
			// stray copy deeper in the tree.
			if (root.relativize(file).getNameCount() >= root.relativize(existing).getNameCount()) {
				warnings.add("Copybook " + name + " exists twice: using " + root.relativize(existing) + ", ignoring "
						+ root.relativize(file));
				return;
			}
			warnings.add("Copybook " + name + " exists twice: using " + root.relativize(file) + ", ignoring "
					+ root.relativize(existing));
		}
		map.put(key, file);
	}

	/** Source text without comment lines, so a commented-out CALL is not counted. */
	private String readActiveSource(Path file) throws IOException {
		StringBuilder out = new StringBuilder();
		for (String line : Files.readAllLines(file, charset)) {
			if (line.length() > 6 && (line.charAt(6) == '*' || line.charAt(6) == '/'))
				continue;
			// Fixed format: columns 1-6 and 73-80 are sequence numbers, not code. Without this a
			// PROGRAM-ID written on its own line picked up the number of that line (00220000).
			if (line.length() > 72)
				line = line.substring(0, 72);
			if (line.length() > 6)
				line = "      " + line.substring(6);
			String trimmed = line.trim();
			if (trimmed.startsWith("*>"))
				continue;
			int free = line.indexOf("*>");
			out.append(free >= 0 ? line.substring(0, free) : line).append('\n');
		}
		return out.toString();
	}

	private void analyse(Path file) {
		try {
			String source = readActiveSource(file);
			Matcher id = PROGRAM_ID.matcher(source);
			String programId = id.find() ? id.group(1) : baseOf(file);
			if (programs.containsKey(programId.toUpperCase(Locale.ROOT)))
				warnings.add("PROGRAM-ID " + programId + " is defined in " + root.relativize(programs.get(programId.toUpperCase(Locale.ROOT)).file)
						+ " and again in " + root.relativize(file));
			ProgramInfo info = new ProgramInfo(file, programId);
			Matcher call = STATIC_CALL.matcher(source);
			while (call.find())
				info.calls.add(call.group(1).trim().toUpperCase(Locale.ROOT));
			Matcher dynamic = DYNAMIC_CALL.matcher(source);
			while (dynamic.find()) {
				String word = dynamic.group(1).toUpperCase(Locale.ROOT);
				if (!word.equals("STATIC") && !word.equals("DYNAMIC"))
					info.dynamicCall = true;
			}
			programs.put(programId.toUpperCase(Locale.ROOT), info);
		} catch (IOException e) {
			warnings.add("Cannot read " + file + ": " + e.getMessage());
		}
	}

	private void linkCalls() {
		for (ProgramInfo caller : programs.values())
			for (String target : caller.calls) {
				ProgramInfo callee = programs.get(target);
				if (callee != null && callee != caller)
					callee.calledByOthers = true;
			}
	}

	public Path getRoot() {
		return root;
	}

	public List<Path> getProgramFiles() {
		return Collections.unmodifiableList(programFiles);
	}

	public List<Path> getCopybookFiles() {
		return Collections.unmodifiableList(copybookFiles);
	}

	public Map<String, ProgramInfo> getPrograms() {
		return Collections.unmodifiableMap(programs);
	}

	public List<String> getWarnings() {
		return Collections.unmodifiableList(warnings);
	}

	/** Program entry for a source file, or null. */
	public ProgramInfo programOf(Path file) {
		Path normal = file.toAbsolutePath().normalize();
		for (ProgramInfo info : programs.values())
			if (info.file.toAbsolutePath().normalize().equals(normal))
				return info;
		return null;
	}

	/**
	 * Resolves the name of a COPY statement. The name may carry quotes or an extension; the match
	 * ignores case. Falls back to a program source file of that name.
	 */
	public Path findCopybook(String name) {
		if (name == null)
			return null;
		String key = name.trim().replace("\"", "").replace("'", "").toLowerCase(Locale.ROOT);
		Path found = copybooks.get(key);
		if (found == null) {
			// COPY name may be written with a path prefix.
			int slash = Math.max(key.lastIndexOf('/'), key.lastIndexOf('\\'));
			if (slash >= 0)
				found = copybooks.get(key.substring(slash + 1));
		}
		if (found == null)
			found = sourceByName.get(key);
		return found;
	}

	/** True when the project itself has a copybook of that name. */
	public boolean hasCopybook(String name) {
		return copybooks.containsKey(name.toLowerCase(Locale.ROOT));
	}

	/** Registers a stand-in for a system copybook (DFHAID, SQLCA ...) the project does not ship. */
	public void addSystemCopybook(String name, Path file) {
		String key = name.toLowerCase(Locale.ROOT);
		if (!copybooks.containsKey(key))
			copybooks.put(key, file);
	}

	/** Programs called by name somewhere in the project that are not part of it. */
	public Set<String> unresolvedCalls() {
		Set<String> missing = new LinkedHashSet<String>();
		for (ProgramInfo info : programs.values())
			for (String target : info.calls)
				if (!programs.containsKey(target))
					missing.add(target);
		return missing;
	}
}
