package com.res.cobol;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

/**
 * benchmark/baseline.json: what the converter managed on the cobol/ corpus when it was last
 * measured with scripts/baseline.ps1 -Save. Later runs must not fall below it.
 */
final class Baseline {

	private static final File FILE = new File("benchmark/baseline.json");

	private final DocumentContext json;

	Baseline() throws IOException {
		json = JsonPath.parse(FILE);
	}

	/** The converter arguments the measurement used, without -odir. */
	String command() {
		return json.read("$.command", String.class);
	}

	String compileVerify() {
		return json.read("$.compileVerify", String.class);
	}

	int programs() {
		return json.read("$.programs", Integer.class);
	}

	int converted() {
		return json.read("$.converted", Integer.class);
	}

	int failed() {
		return json.read("$.failed", Integer.class);
	}

	int stubbedAfterVerify() {
		return json.read("$.stubbedAfterVerify", Integer.class);
	}

	List<String> convertedPrograms() {
		return names(json.read("$.convertedPrograms", List.class));
	}

	static List<String> names(List<?> values) {
		List<String> names = new ArrayList<String>();
		for (Object value : values)
			names.add(String.valueOf(value));
		return names;
	}
}
