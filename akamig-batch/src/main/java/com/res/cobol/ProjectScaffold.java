package com.res.cobol;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import com.res.common.CobolTranslationException;

/**
 * Writes the Maven project around the converted programs (-scaffold): pom.xml, application.yml,
 * Config.properties, the BatchApplication launcher, the job listener and the runtime library the
 * generated code imports (jp.co.alsm.common / helper / config / logging).
 *
 * <p>The files come from the alsm-scaffold resources, which are exported from the platform's own
 * export scaffold, so a project converted here builds and runs exactly like one exported there.
 */
final class ProjectScaffold {

	private static final String RESOURCES = "/alsm-scaffold/";
	private static final String PLACEHOLDER = "ALSMPLACEHOLDERNAME";
	private static final String PLACEHOLDER_ARTIFACT = "alsmplaceholdername";
	private static final String LAUNCHER = "src/main/java/jp/co/alsm/BatchApplication.java";

	private ProjectScaffold() {
	}

	static String artifactId(String projectName) {
		String slug = projectName.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "-").replaceAll("^-+|-+$", "");
		return slug.isEmpty() ? "modernized-app" : slug;
	}

	private static String resource(String path) throws IOException {
		try (InputStream in = ProjectScaffold.class.getResourceAsStream(RESOURCES + path)) {
			if (in == null)
				throw new CobolTranslationException("Scaffold resource missing from the converter: " + path);
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			byte[] buffer = new byte[8192];
			for (int n; (n = in.read(buffer)) > 0;)
				out.write(buffer, 0, n);
			return new String(out.toByteArray(), StandardCharsets.UTF_8);
		}
	}

	/** Writes the project files; the jobs are already under src/main/java/<basePackage>/jobs. */
	static void write(Path outputDir, String projectName, String basePackage, List<String> jobNames, String defaultJob)
			throws IOException {
		String artifact = artifactId(projectName);
		List<String> paths = new ArrayList<String>();
		for (String line : resource("index.txt").split("\n"))
			if (!line.trim().isEmpty())
				paths.add(line.trim());

		for (String path : paths) {
			String content = resource(path);
			// Maven drops dot-files from resources, so they are stored as dot-<name>.
			Path target = outputDir.resolve(path.startsWith("dot-") ? "." + path.substring(4) : path);
			if (path.equals("pom.xml")) {
				content = content.replace(PLACEHOLDER_ARTIFACT, artifact).replace(PLACEHOLDER, xml(projectName));
				content = withDatabaseDrivers(content);
			} else if (path.equals("src/main/resources/application.yml")) {
				content = content.replace(PLACEHOLDER_ARTIFACT, artifact)
						+ (defaultJob == null ? "" : "\n# Job run when java -jar gets no job name: the first main (not CALLed) program.\nalsm:\n  default-job: " + defaultJob + "\n")
						+ DATABASE_EXAMPLES;
			} else if (path.equals(LAUNCHER)) {
				// The launcher lives in the base package and scans it as well as the runtime.
				content = content.replace("package jp.co.alsm;", "package " + basePackage + ";")
						.replace("scanBasePackages = {\"jp.co.alsm\"}", "scanBasePackages = {" + scanList(basePackage) + "}")
						.replace("jobs.keySet().iterator().next()",
								"findJob(jobs, context.getEnvironment().getProperty(\"alsm.default-job\", jobs.keySet().iterator().next())) != null"
										+ " ? findJob(jobs, context.getEnvironment().getProperty(\"alsm.default-job\", jobs.keySet().iterator().next()))"
										+ " : jobs.keySet().iterator().next()");
				target = outputDir.resolve("src/main/java/" + basePackage.replace('.', '/') + "/BatchApplication.java");
			}
			Files.createDirectories(target.getParent());
			Files.write(target, content.getBytes(StandardCharsets.UTF_8));
		}
		Files.write(outputDir.resolve("README.md"), readme(projectName, artifact, basePackage, jobNames)
				.getBytes(StandardCharsets.UTF_8));
	}

	private static String scanList(String basePackage) {
		if (basePackage.equals("jp.co.alsm") || basePackage.startsWith("jp.co.alsm."))
			return "\"jp.co.alsm\"";
		return "\"" + basePackage + "\", \"jp.co.alsm\"";
	}

	private static String xml(String text) {
		return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
	}

	/** MySQL and PostgreSQL drivers, so switching off the in-memory H2 is a configuration change. */
	private static String withDatabaseDrivers(String pom) {
		String drivers = "\n    <!-- Drivers for the databases the converted jobs may use instead of the in-memory H2;\n"
				+ "         choose one in application.yml (examples at its end). -->\n"
				+ "    <dependency>\n      <groupId>com.mysql</groupId>\n      <artifactId>mysql-connector-j</artifactId>\n"
				+ "      <scope>runtime</scope>\n    </dependency>\n"
				+ "    <dependency>\n      <groupId>org.postgresql</groupId>\n      <artifactId>postgresql</artifactId>\n"
				+ "      <scope>runtime</scope>\n    </dependency>\n"
				+ "    <dependency>\n      <groupId>org.springframework.boot</groupId>\n      <artifactId>spring-boot-starter-jdbc</artifactId>\n"
				+ "    </dependency>\n";
		int at = pom.indexOf("  </dependencies>");
		return at < 0 ? pom : pom.substring(0, at) + drivers.substring(1) + pom.substring(at);
	}

	private static final String DATABASE_EXAMPLES = "\n# To keep the job history (and the data of embedded SQL) in a real database, replace the\n"
			+ "# datasource above, for example:\n"
			+ "#  datasource:\n#    url: jdbc:mysql://localhost:3306/batch\n#    username: batch\n#    password: <password>\n"
			+ "#  datasource:\n#    url: jdbc:postgresql://localhost:5432/batch\n#    username: batch\n#    password: <password>\n";

	private static String readme(String projectName, String artifact, String basePackage, List<String> jobNames) {
		StringBuilder jobs = new StringBuilder();
		for (String job : jobNames)
			jobs.append("- `").append(job).append("`\n");
		String example = jobNames.isEmpty() ? "<JobName>" : jobNames.get(0);
		return "# " + projectName + "\n\n"
				+ "Spring Boot 2.7 / Spring Batch project generated by the ALSM COBOL converter. Each COBOL program is a\n"
				+ "job under `src/main/java/" + basePackage.replace('.', '/') + "/jobs/<program>/` (Job, Tasklet, Model);\n"
				+ "the runtime it uses is under `src/main/java/jp/co/alsm/`. See `conversion_report.md` for what was\n"
				+ "converted and why the rest was not (those programs are stubs that fail with the reason).\n\n"
				+ "## Build\n\n```bash\nmvn clean package -DskipTests\n```\n\n"
				+ "## Run\n\n```bash\njava -jar target/" + artifact + ".jar            # runs the first job\n"
				+ "java -jar target/" + artifact + ".jar " + example + "\n```\n\n"
				+ "In PowerShell, quote Maven arguments: `mvn spring-boot:run \"-Dspring-boot.run.arguments=" + example + "\"`.\n\n"
				+ "## Jobs\n\n" + jobs
				+ "\n## Good to know\n\n"
				+ "- Job history is kept in an in-memory H2 database; MySQL and PostgreSQL drivers are included, see the\n"
				+ "  end of `src/main/resources/application.yml`.\n"
				+ "- `Config.properties` holds the conversion settings (encoding MS932, database type); a copy in the\n"
				+ "  folder you run from overrides the one in the jar.\n"
				+ "- A CALL between programs goes through `jp.co.alsm.config.CallSupport`: the called program is found by\n"
				+ "  its PROGRAM-ID and its LINKAGE items are passed by reference.\n";
	}
}
