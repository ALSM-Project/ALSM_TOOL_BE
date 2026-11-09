package com.res.cobol;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Turns the bytes of a COBOL source or copybook into text.
 *
 * <p>With an explicit charset (-encoding) the bytes are decoded as told. Without one a file is
 * UTF-8 when its bytes are valid UTF-8 (ASCII always is) and MS932 (Shift-JIS) when they are
 * not; decoding Shift-JIS text as UTF-8 yields U+FFFD, which the parser then stops at. The
 * decision is made per file, and every file that fell back is remembered so the report can say
 * which ones were read as MS932.
 *
 * <p>A Shift-JIS file whose bytes happen to be valid UTF-8 is read as UTF-8; pass -encoding for
 * a project that mixes the two in ways this cannot tell apart.
 */
public final class SourceEncoding {

	/** What a file that is not valid UTF-8 is read as. */
	public static final String FALLBACK_NAME = "MS932";
	public static final Charset FALLBACK = Charset.forName(FALLBACK_NAME);

	/** Absolute paths of the files read through here as the fallback since the last takeFallbacks(). */
	private static final Set<String> fallbacks = new LinkedHashSet<String>();

	private SourceEncoding() {
	}

	/** The text of a file and how it was decoded. */
	public static final class Decoded {
		public final String text;
		public final Charset charset;
		/** True when UTF-8 was tried first and failed, so the fallback charset was used. */
		public final boolean fellBack;

		Decoded(String text, Charset charset, boolean fellBack) {
			this.text = text;
			this.charset = charset;
			this.fellBack = fellBack;
		}
	}

	/**
	 * @param explicit the charset of -encoding, or null to detect. An explicit charset is lenient
	 *                 (a byte that does not decode becomes U+FFFD) so one stray byte never stops a run.
	 */
	public static Decoded decode(byte[] bytes, Charset explicit) {
		if (explicit != null)
			return new Decoded(new String(bytes, explicit), explicit, false);
		try {
			String text = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString();
			return new Decoded(text, StandardCharsets.UTF_8, false);
		} catch (CharacterCodingException e) {
			return new Decoded(new String(bytes, FALLBACK), FALLBACK, true);
		}
	}

	/** Reads a file; a file that fell back is remembered under its absolute path. */
	public static String read(Path file, Charset explicit) throws IOException {
		Decoded decoded = decode(Files.readAllBytes(file), explicit);
		if (decoded.fellBack)
			remember(file);
		return decoded.text;
	}

	/** The lines of a file, as Files.readAllLines would give them. */
	public static List<String> lines(Path file, Charset explicit) throws IOException {
		List<String> lines = new ArrayList<String>();
		BufferedReader reader = new BufferedReader(new StringReader(read(file, explicit)));
		String line;
		while ((line = reader.readLine()) != null)
			lines.add(line);
		return lines;
	}

	public static Reader reader(File file, Charset explicit) throws IOException {
		return new StringReader(read(file.toPath(), explicit));
	}

	private static void remember(Path file) {
		synchronized (fallbacks) {
			fallbacks.add(file.toAbsolutePath().normalize().toString());
		}
	}

	/** The name a charset goes by in the report. */
	private static String charsetName(Charset charset) {
		return charset.equals(FALLBACK) ? FALLBACK_NAME : charset.name();
	}

	/**
	 * Notes for the code lines of a decoded file that still hold U+FFFD: a byte that no charset
	 * tried can read, which the parser then stops at. Comment lines are left out, they do no harm.
	 */
	public static List<String> undecodable(Decoded decoded, String name) {
		List<String> notes = new ArrayList<String>();
		String[] lines = decoded.text.split("\r\n|\r|\n", -1);
		for (int i = 0; i < lines.length && notes.size() < 10; i++) {
			String line = lines[i];
			boolean comment = line.length() > 6 && (line.charAt(6) == '*' || line.charAt(6) == '/');
			if (!comment && line.indexOf((char) 0xFFFD) >= 0)
				notes.add(name + ":" + (i + 1) + ": has a byte that is not valid " + charsetName(decoded.charset)
						+ " (read as U+FFFD)");
		}
		return notes;
	}

	/** The report's note for a file that fell back. */
	public static String warning(String file) {
		return file + ": decoded as " + FALLBACK_NAME;
	}

	/** The files that fell back since the last call, in the order they were first read. */
	public static List<String> takeFallbacks() {
		synchronized (fallbacks) {
			List<String> taken = new ArrayList<String>(fallbacks);
			fallbacks.clear();
			return taken;
		}
	}
}
