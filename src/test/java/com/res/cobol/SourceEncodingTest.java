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
import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** Without -encoding a source is UTF-8 when its bytes are, MS932 when they are not. */
class SourceEncodingTest {

	private static final Charset MS932 = Charset.forName("MS932");
	/** What a byte that does not decode becomes. */
	private static final char REPLACEMENT = (char) 0xFFFD;
	/** "nihongo": three kanji, six bytes in MS932 that are not valid UTF-8. */
	private static final String JAPANESE = new String(new char[] { 0x65e5, 0x672c, 0x8a9e });

	@TempDir
	Path dir;

	@BeforeEach
	void forgetEarlierFiles() {
		SourceEncoding.takeFallbacks();
	}

	@Test
	void asciiIsUtf8() {
		SourceEncoding.Decoded decoded = SourceEncoding.decode("MOVE A TO B.".getBytes(StandardCharsets.US_ASCII), null);
		assertEquals("MOVE A TO B.", decoded.text);
		assertEquals(StandardCharsets.UTF_8, decoded.charset);
		assertFalse(decoded.fellBack);
	}

	@Test
	void utf8WithJapaneseIsUtf8() {
		SourceEncoding.Decoded decoded = SourceEncoding.decode(("* " + JAPANESE).getBytes(StandardCharsets.UTF_8), null);
		assertEquals("* " + JAPANESE, decoded.text);
		assertEquals(StandardCharsets.UTF_8, decoded.charset);
		assertFalse(decoded.fellBack);
	}

	@Test
	void ms932WithJapaneseFallsBackToMs932() {
		SourceEncoding.Decoded decoded = SourceEncoding.decode(("* " + JAPANESE).getBytes(MS932), null);
		assertEquals("* " + JAPANESE, decoded.text);
		assertEquals(MS932, decoded.charset);
		assertTrue(decoded.fellBack);
		assertFalse(decoded.text.indexOf(REPLACEMENT) >= 0);
	}

	@Test
	void anExplicitCharsetIsNeverSecondGuessed() {
		byte[] ms932 = ("* " + JAPANESE).getBytes(MS932);
		SourceEncoding.Decoded asTold = SourceEncoding.decode(ms932, MS932);
		assertEquals("* " + JAPANESE, asTold.text);
		assertFalse(asTold.fellBack);
		// Wrong on purpose: the bytes become U+FFFD, they do not throw and do not switch charsets.
		SourceEncoding.Decoded wrong = SourceEncoding.decode(ms932, StandardCharsets.UTF_8);
		assertTrue(wrong.text.indexOf(REPLACEMENT) >= 0);
		assertEquals(StandardCharsets.UTF_8, wrong.charset);
		assertFalse(wrong.fellBack);
	}

	@Test
	void aFileThatFellBackIsRememberedOnceWithItsAbsolutePath() throws IOException {
		Path sjis = dir.resolve("jp.cob");
		Files.write(sjis, JAPANESE.getBytes(MS932));
		Path utf8 = dir.resolve("utf8.cob");
		Files.write(utf8, JAPANESE.getBytes(StandardCharsets.UTF_8));
		Path ascii = dir.resolve("ascii.cob");
		Files.write(ascii, "ABC".getBytes(StandardCharsets.US_ASCII));

		assertEquals(JAPANESE, SourceEncoding.read(sjis, null));
		assertEquals(JAPANESE, SourceEncoding.read(sjis, null));
		assertEquals(JAPANESE, SourceEncoding.read(utf8, null));
		assertEquals("ABC", SourceEncoding.read(ascii, null));

		assertEquals(Collections.singletonList(sjis.toAbsolutePath().normalize().toString()), SourceEncoding.takeFallbacks());
		assertTrue(SourceEncoding.takeFallbacks().isEmpty());
	}

	@Test
	void aFileReadWithAnExplicitCharsetIsNotRemembered() throws IOException {
		Path sjis = dir.resolve("jp.cob");
		Files.write(sjis, JAPANESE.getBytes(MS932));
		assertEquals(JAPANESE, SourceEncoding.read(sjis, MS932));
		assertTrue(SourceEncoding.takeFallbacks().isEmpty());
	}

	@Test
	void linesAreTheOnesReadAllLinesGives() throws IOException {
		Path file = dir.resolve("lines.cob");
		Files.write(file, ("a" + JAPANESE + "\r\nb\n\nc").getBytes(MS932));
		assertEquals(Arrays.asList("a" + JAPANESE, "b", "", "c"), SourceEncoding.lines(file, null));
	}

	@Test
	void aByteNoCharsetCanReadIsFoundInCodeLinesOnly() {
		// The question marks are 0xFD bytes: not valid in UTF-8 and not in MS932. One sits in a comment.
		byte[] bad = "      * comment ?\n       MOVE A TO B?.\n       MOVE C TO D.\n".getBytes(StandardCharsets.US_ASCII);
		for (int i = 0; i < bad.length; i++)
			if (bad[i] == (byte) 63)
				bad[i] = (byte) 0xFD;
		SourceEncoding.Decoded decoded = SourceEncoding.decode(bad, null);
		assertEquals(Collections.singletonList("x.cob:2: has a byte that is not valid MS932 (read as U+FFFD)"),
				SourceEncoding.undecodable(decoded, "x.cob"));
	}

	@Test
	void theReportNamesTheFileAndTheCharset() {
		assertEquals("COPY/ABC.COB: decoded as MS932", SourceEncoding.warning("COPY/ABC.COB"));
	}
}
