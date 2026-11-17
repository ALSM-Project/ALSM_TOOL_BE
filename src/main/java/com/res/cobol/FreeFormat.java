package com.res.cobol;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Writes free-format COBOL (introduced by the directive {@code >>SOURCE FORMAT FREE}) as the fixed
 * format the scanner reads: code in columns 8 to 72, a comment marked in column 7, a continued
 * literal marked with a hyphen in column 7.
 *
 * <p>The text is changed line for line wherever it can be: a directive or comment line stays a
 * (comment) line, so line numbers in error messages stay close to the original. Only a line whose
 * code does not fit in columns 8-72 grows extra lines. A file without the directive is returned as
 * it is.
 */
final class FreeFormat {

	private FreeFormat() {
	}

	private static final int CODE_START = 7;
	private static final int WIDTH = 72 - CODE_START;
	private static final int TAB = 4;

	private static final Pattern FORMAT_DIRECTIVE = Pattern
			.compile("(?i)^\\s*>>\\s*SOURCE\\s+(?:FORMAT\\s+)?(?:IS\\s+)?(FREE|FIXED)\\b.*$");
	private static final Pattern ANY_DIRECTIVE = Pattern.compile("^\\s*>>.*$");

	/** Number of lines of the source that were written differently, filled by toFixed. */
	static final class Result {
		final String source;
		final int changedLines;

		Result(String source, int changedLines) {
			this.source = source;
			this.changedLines = changedLines;
		}
	}

	static boolean isFree(String text) {
		for (String line : text.split("\r?\n", -1)) {
			Matcher m = FORMAT_DIRECTIVE.matcher(line);
			if (m.matches())
				return m.group(1).equalsIgnoreCase("FREE");
		}
		return false;
	}

	static Result toFixed(String text) {
		boolean anyFree = false;
		for (String line : text.split("\r?\n", -1)) {
			Matcher m = FORMAT_DIRECTIVE.matcher(line);
			if (m.matches() && m.group(1).equalsIgnoreCase("FREE")) {
				anyFree = true;
				break;
			}
		}
		if (!anyFree)
			return new Result(text, 0);

		String newline = text.contains("\r\n") ? "\r\n" : "\n";
		String[] lines = text.split("\r?\n", -1);
		List<String> out = new ArrayList<String>();
		boolean free = false;
		int changed = 0;
		for (String line : lines) {
			Matcher directive = FORMAT_DIRECTIVE.matcher(line);
			if (directive.matches()) {
				free = directive.group(1).equalsIgnoreCase("FREE");
				out.add("      *" + line.trim());
				changed++;
				continue;
			}
			if (!free) {
				out.add(line);
				continue;
			}
			List<String> converted = convertLine(line);
			if (converted.size() != 1 || !converted.get(0).equals(line))
				changed++;
			out.addAll(converted);
		}
		StringBuilder joined = new StringBuilder();
		for (int i = 0; i < out.size(); i++) {
			if (i > 0)
				joined.append(newline);
			joined.append(out.get(i));
		}
		return new Result(joined.toString(), changed);
	}

	private static List<String> convertLine(String raw) {
		List<String> result = new ArrayList<String>();
		String line = expandTabs(raw);
		String trimmed = line.trim();
		if (trimmed.isEmpty()) {
			result.add("");
			return result;
		}
		if (ANY_DIRECTIVE.matcher(line).matches() || trimmed.startsWith("*>")) {
			result.add("      *" + (trimmed.startsWith("*>") ? trimmed.substring(2) : trimmed));
			return result;
		}
		int comment = commentStart(line);
		if (comment >= 0)
			line = line.substring(0, comment);
		line = rtrim(line);
		if (line.trim().isEmpty()) {
			result.add("");
			return result;
		}
		int indent = 0;
		while (indent < line.length() && line.charAt(indent) == ' ')
			indent++;
		if (line.length() <= WIDTH) {
			result.add(spaces(CODE_START) + line);
			return result;
		}
		result.addAll(wrap(line.substring(indent), indent));
		return result;
	}

	/** Index of an inline comment marker {@code *>} outside a literal, or -1. */
	private static int commentStart(String line) {
		char quote = 0;
		for (int i = 0; i < line.length(); i++) {
			char ch = line.charAt(i);
			if (quote != 0) {
				if (ch == quote)
					quote = 0;
			} else if (ch == '\'' || ch == '"')
				quote = ch;
			else if (ch == '*' && i + 1 < line.length() && line.charAt(i + 1) == '>')
				return i;
		}
		return -1;
	}

	/** Splits on blanks outside literals. */
	private static List<String> tokens(String text) {
		List<String> tokens = new ArrayList<String>();
		StringBuilder token = new StringBuilder();
		char quote = 0;
		for (int i = 0; i < text.length(); i++) {
			char ch = text.charAt(i);
			if (quote != 0) {
				token.append(ch);
				if (ch == quote)
					quote = 0;
			} else if (ch == '\'' || ch == '"') {
				quote = ch;
				token.append(ch);
			} else if (Character.isWhitespace(ch)) {
				if (token.length() > 0) {
					tokens.add(token.toString());
					token.setLength(0);
				}
			} else
				token.append(ch);
		}
		if (token.length() > 0)
			tokens.add(token.toString());
		return tokens;
	}

	/**
	 * FUNCTION and the name after it stay on one line: the normalizer that rewrites FUNCTION TRIM
	 * reads them as one phrase on one line.
	 */
	private static List<String> glueFunctionNames(List<String> words) {
		List<String> glued = new ArrayList<String>();
		for (int i = 0; i < words.size(); i++) {
			if (words.get(i).equalsIgnoreCase("FUNCTION") && i + 1 < words.size()) {
				glued.add(words.get(i) + " " + words.get(i + 1));
				i++;
			} else
				glued.add(words.get(i));
		}
		return glued;
	}

	/** Lays the words of one statement line out in columns 8-72, continuing a literal that is too long. */
	private static List<String> wrap(String code, int indent) {
		List<String> lines = new ArrayList<String>();
		int continuationIndent = Math.max(indent, 4) + 4;
		StringBuilder current = new StringBuilder(spaces(Math.max(indent, 1)));
		boolean empty = true;
		for (String token : glueFunctionNames(tokens(code))) {
			int needed = (empty ? 0 : 1) + token.length();
			if (current.length() + needed > WIDTH && !empty) {
				lines.add(spaces(CODE_START) + current);
				current = new StringBuilder(spaces(continuationIndent));
				empty = true;
			}
			if (current.length() + token.length() <= WIDTH) {
				if (!empty)
					current.append(' ');
				current.append(token);
				empty = false;
				continue;
			}
			// A literal longer than a line: fill the line, continue it with a hyphen in column 7.
			char quote = token.indexOf('"') >= 0 && (token.indexOf('\'') < 0 || token.indexOf('"') < token.indexOf('\''))
					? '"' : '\'';
			int room = WIDTH - current.length() - (empty ? 0 : 1);
			if (token.indexOf(quote) < 0 || room < token.indexOf(quote) + 2) {
				// not a literal, or no room to start one here: put it on a line of its own
				if (!empty)
					lines.add(spaces(CODE_START) + current);
				current = new StringBuilder(spaces(continuationIndent));
				room = WIDTH - current.length();
				if (token.indexOf(quote) < 0 || token.length() <= room) {
					current.append(token);
					empty = false;
					continue;
				}
			} else if (!empty) {
				current.append(' ');
			}
			int cut = Math.min(room, token.length());
			while (cut > token.indexOf(quote) + 1 && token.charAt(cut - 1) == quote)
				cut--;
			current.append(token, 0, cut);
			lines.add(spaces(CODE_START) + current);
			String rest = token.substring(cut);
			int chunk = WIDTH - 4 - 1;
			while (true) {
				String piece = rest.length() <= chunk ? rest : rest.substring(0, chunk);
				if (piece.length() < rest.length())
					while (piece.length() > 1 && piece.charAt(piece.length() - 1) == quote)
						piece = piece.substring(0, piece.length() - 1);
				rest = rest.substring(piece.length());
				String line = "      -" + spaces(4) + quote + piece;
				if (rest.isEmpty()) {
					lines.add(line);
					break;
				}
				lines.add(line);
			}
			// whatever follows the literal starts a new line
			current = new StringBuilder(spaces(continuationIndent));
		}
		if (current.toString().trim().length() > 0)
			lines.add(spaces(CODE_START) + current);
		return lines;
	}

	private static String expandTabs(String line) {
		if (line.indexOf('\t') < 0)
			return line;
		StringBuilder b = new StringBuilder();
		for (char ch : line.toCharArray()) {
			if (ch == '\t')
				do
					b.append(' ');
				while (b.length() % TAB != 0);
			else
				b.append(ch);
		}
		return b.toString();
	}

	private static String rtrim(String s) {
		int end = s.length();
		while (end > 0 && Character.isWhitespace(s.charAt(end - 1)))
			end--;
		return s.substring(0, end);
	}

	private static String spaces(int n) {
		StringBuilder b = new StringBuilder();
		for (int i = 0; i < n; i++)
			b.append(' ');
		return b.toString();
	}
}
