package com.res.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;

import org.junit.jupiter.api.Test;

/** The recorded baseline must be a complete measurement, or comparing against it means nothing. */
class BaselineFileTest {

	@Test
	void programsAddUp() throws IOException {
		Baseline baseline = new Baseline();
		assertEquals(baseline.programs(), baseline.converted() + baseline.failed());
		assertEquals(baseline.converted(), baseline.convertedPrograms().size());
		assertTrue(baseline.stubbedAfterVerify() <= baseline.failed(), "a stubbed program is one of the failed ones");
	}

	@Test
	void wasMeasuredWithAFinishedCompileVerify() throws IOException {
		assertEquals("success", new Baseline().compileVerify());
	}
}
