package com.res.cobol;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Rewrites EXEC CICS blocks of fixed-format COBOL into the narrow dialect the parser accepts, and
 * smooths a few other constructs it rejects, so CICS programs convert instead of failing on the
 * first syntax error. Java port of the backend's cics-source-normalizer.ts, so a project converted
 * with -project gets the same treatment as an upload through the platform.
 *
 * <p>The grammar wants every option of a command in one fixed order, each argument a single name
 * or literal, and knows only part of CICS. The rewrite is lossy by necessity: RESP/RESP2/NOHANDLE
 * are dropped (DFHRESP(x) becomes its numeric code), complex arguments are moved into temps, and
 * commands the grammar lacks (STARTBR, READNEXT, WRITE, ASKTIME, RETRIEVE ...) become CONTINUE.
 * The original block stays in the source as a comment and every change is listed.
 */
public final class CicsNormalizer {

	/** One change made to the source; line is 1-based in the original. */
	public static final class Change {
		public final String kind;
		public final int line;
		public final String detail;

		Change(String kind, int line, String detail) {
			this.kind = kind;
			this.line = line;
			this.detail = detail;
		}
	}

	/** Normalized source of one file. */
	public static final class Result {
		public final String source;
		public final List<Change> changes;
		public final boolean needsSupportCopybook;
		public final Map<String, String> hoists;

		Result(String source, List<Change> changes, boolean needsSupportCopybook, Map<String, String> hoists) {
			this.source = source;
			this.changes = changes;
			this.needsSupportCopybook = needsSupportCopybook;
			this.hoists = hoists;
		}
	}

	public static final String SUPPORT_COPYBOOK_NAME = "ALSMCICS";

	private enum Arg {
		NONE, WORD_OR_LITERAL, WORD, WORD_OR_INTEGER
	}

	private static final class OptSpec {
		final String name;
		final Arg arg;
		final boolean required;
		final String hoistPic;

		OptSpec(String name, Arg arg, boolean required, String hoistPic) {
			this.name = name;
			this.arg = arg;
			this.required = required;
			this.hoistPic = hoistPic;
		}
	}

	private static final class CommandSpec {
		final List<String> prefix;
		final List<String> flagChoice;
		final String flagFallback;
		final List<OptSpec> options;

		CommandSpec(List<String> prefix, List<String> flagChoice, String flagFallback, OptSpec... options) {
			this.prefix = prefix;
			this.flagChoice = flagChoice;
			this.flagFallback = flagFallback;
			this.options = Arrays.asList(options);
		}
	}

	private static final String NAME8 = "X(8)";

	private static OptSpec opt(String name) {
		return new OptSpec(name, Arg.WORD_OR_LITERAL, false, null);
	}

	private static OptSpec opt(String name, Arg arg) {
		return new OptSpec(name, arg, false, null);
	}

	private static OptSpec hoisted(String name, Arg arg, boolean required, String pic) {
		return new OptSpec(name, arg, required, pic);
	}

	private static OptSpec flag(String name) {
		return new OptSpec(name, Arg.NONE, false, null);
	}

	private static List<String> words(String... w) {
		return Arrays.asList(w);
	}

	/** Option order mirrors the grammar exactly; anything not listed is dropped. */
	private static final Map<String, CommandSpec> COMMANDS = new HashMap<String, CommandSpec>();
	static {
		COMMANDS.put("SEND_MAP", new CommandSpec(words("SEND"), null, null,
				hoisted("MAP", Arg.WORD_OR_LITERAL, true, NAME8), hoisted("MAPSET", Arg.WORD_OR_LITERAL, true, NAME8),
				opt("FROM"), opt("LENGTH"), flag("DATAONLY"), flag("MAPONLY"), flag("CURSOR"), flag("FORMFEED"),
				flag("ERASE"), flag("ERASEAUP"), flag("PRINT"), flag("FREEKB"), flag("ALARM"), flag("FRSET")));
		COMMANDS.put("SEND_TEXT", new CommandSpec(words("SEND", "TEXT"), null, null, opt("FROM"), opt("LENGTH"),
				flag("CURSOR"), flag("FORMFEED"), flag("ERASE"), flag("PRINT"), flag("FREEKB"), flag("ALARM"),
				flag("NLEOM")));
		COMMANDS.put("SEND_CONTROL", new CommandSpec(words("SEND", "CONTROL"), null, null, opt("CURSOR"),
				flag("ERASE"), flag("ERASEAUP"), flag("PRINT"), flag("FREEKB"), flag("ALARM"), flag("FRSET")));
		COMMANDS.put("RECEIVE_MAP", new CommandSpec(words("RECEIVE"), null, null,
				hoisted("MAP", Arg.WORD_OR_LITERAL, true, NAME8), opt("MAPSET"), opt("INTO"), opt("SET"),
				flag("TERMINAL")));
		COMMANDS.put("READQ", new CommandSpec(words("READQ"), words("TS", "TD"), "TS",
				hoisted("QUEUE", Arg.WORD, false, "X(16)"), opt("INTO", Arg.WORD), opt("LENGTH", Arg.WORD),
				opt("NUMITEMS", Arg.WORD), opt("SYSID", Arg.WORD)));
		COMMANDS.put("WRITEQ", new CommandSpec(words("WRITEQ"), words("TD", "TS"), "TS",
				hoisted("QUEUE", Arg.WORD, false, "X(16)"), opt("FROM", Arg.WORD),
				opt("LENGTH", Arg.WORD_OR_INTEGER), opt("SYSID", Arg.WORD)));
		COMMANDS.put("DELETEQ", new CommandSpec(words("DELETEQ"), words("TS", "TD"), "TS",
				hoisted("QUEUE", Arg.WORD, false, "X(16)"), opt("SYSID", Arg.WORD)));
		COMMANDS.put("RETURN", new CommandSpec(words("RETURN"), null, null,
				hoisted("TRANSID", Arg.WORD_OR_LITERAL, false, "X(4)"), opt("COMMAREA"), flag("IMMEDIATE"),
				opt("LENGTH")));
		COMMANDS.put("XCTL", new CommandSpec(words("XCTL"), null, null,
				hoisted("PROGRAM", Arg.WORD_OR_LITERAL, true, NAME8), opt("COMMAREA")));
		COMMANDS.put("LINK", new CommandSpec(words("LINK"), null, null,
				hoisted("PROGRAM", Arg.WORD_OR_LITERAL, true, NAME8), opt("COMMAREA", Arg.WORD), opt("LENGTH"),
				opt("DATALENGTH"), opt("SYSID"), flag("SYNCONRETURN"), opt("TRANSID")));
		COMMANDS.put("READ", new CommandSpec(words("READ"), null, null,
				hoisted("FILE", Arg.WORD_OR_LITERAL, true, NAME8), flag("UNCOMMITTED"), flag("UPDATE"), opt("INTO"),
				opt("SET"), opt("KEYLENGTH"), opt("SYSID"), opt("LENGTH"), flag("RBA"), flag("RRN"), flag("EQUAL"),
				flag("GTEQ"), flag("NOSUSPEND")));
		COMMANDS.put("READPREV", new CommandSpec(words("READPREV"), null, null,
				hoisted("FILE", Arg.WORD_OR_LITERAL, true, NAME8), opt("INTO"), opt("SET"), opt("RIDFLD"),
				flag("UPDATE"), opt("KEYLENGTH"), opt("REQID"), opt("SYSID"), opt("LENGTH"), flag("NOSUSPEND")));
		COMMANDS.put("DELETE", new CommandSpec(words("DELETE"), null, null,
				hoisted("FILE", Arg.WORD_OR_LITERAL, true, NAME8), opt("RIDFLD"), opt("KEYLENGTH"), opt("NUMREC"),
				opt("SYSID"), flag("NOSUSPEND")));
		COMMANDS.put("ENDBR", new CommandSpec(words("ENDBR"), null, null,
				hoisted("FILE", Arg.WORD_OR_LITERAL, true, NAME8), opt("REQID"), opt("SYSID")));
		COMMANDS.put("START", new CommandSpec(words("START"), null, null,
				hoisted("TRANSID", Arg.WORD_OR_LITERAL, true, "X(4)"), opt("INTERVAL"), opt("FROM"), opt("LENGTH")));
		COMMANDS.put("HANDLE_AID", new CommandSpec(words("HANDLE", "AID"), null, null, opt("PA1"), opt("PA2"),
				opt("PF1"), opt("ENTER"), opt("CLEAR"), opt("ANYKEY")));
		COMMANDS.put("HANDLE_CONDITION", new CommandSpec(words("HANDLE", "CONDITION"), null, null, opt("QIDERR")));
	}

	/** Option spellings CICS accepts that the grammar only knows under another name. */
	private static final Map<String, String> OPTION_ALIASES = new HashMap<String, String>();
	static {
		OPTION_ALIASES.put("DATASET", "FILE");
		OPTION_ALIASES.put("QNAME", "QUEUE");
	}

	/** EIBRESP values for DFHRESP(...), from the CICS Application Programming Reference. */
	static final Map<String, Integer> DFHRESP_CODES = new HashMap<String, Integer>();
	static {
		Object[][] codes = { { "NORMAL", 0 }, { "ERROR", 1 }, { "RDATT", 2 }, { "WRBRK", 3 }, { "EOF", 4 },
				{ "EODS", 5 }, { "EOC", 6 }, { "INBFMH", 7 }, { "ENDINPT", 8 }, { "NONVAL", 9 }, { "NOSTART", 10 },
				{ "TERMIDERR", 11 }, { "FILENOTFOUND", 12 }, { "DSIDERR", 12 }, { "NOTFND", 13 }, { "DUPREC", 14 },
				{ "DUPKEY", 15 }, { "INVREQ", 16 }, { "IOERR", 17 }, { "NOSPACE", 18 }, { "NOTOPEN", 19 },
				{ "ENDFILE", 20 }, { "ILLOGIC", 21 }, { "LENGERR", 22 }, { "QZERO", 23 }, { "SIGNAL", 24 },
				{ "QBUSY", 25 }, { "ITEMERR", 26 }, { "PGMIDERR", 27 }, { "TRANSIDERR", 28 }, { "ENDDATA", 29 },
				{ "INVTSREQ", 30 }, { "EXPIRED", 31 }, { "RETPAGE", 32 }, { "RTEFAIL", 33 }, { "RTESOME", 34 },
				{ "TSIOERR", 35 }, { "MAPFAIL", 36 }, { "INVERRTERM", 37 }, { "INVMPSZ", 38 }, { "IGREQID", 39 },
				{ "OVERFLOW", 40 }, { "INVLDC", 41 }, { "NOSTG", 42 }, { "JIDERR", 43 }, { "QIDERR", 44 },
				{ "NOJBUFSP", 45 }, { "DSSTAT", 46 }, { "SELNERR", 47 }, { "FUNCERR", 48 }, { "UNEXPIN", 49 },
				{ "NOPASSBKRD", 50 }, { "NOPASSBKWR", 51 }, { "SYSIDERR", 53 }, { "ISCINVREQ", 54 },
				{ "ENQBUSY", 55 }, { "ENVDEFERR", 56 }, { "IGREQCD", 57 }, { "SESSIONERR", 58 }, { "SYSBUSY", 59 },
				{ "SESSBUSY", 60 }, { "NOTALLOC", 61 }, { "CBIDERR", 62 }, { "INVEXITREQ", 63 },
				{ "INVPARTNSET", 64 }, { "INVPARTN", 65 }, { "PARTNFAIL", 66 }, { "USERIDERR", 69 },
				{ "NOTAUTH", 70 }, { "VOLIDERR", 71 }, { "SUPPRESSED", 72 }, { "RESIDERR", 75 }, { "NOSPOOL", 80 },
				{ "TERMERR", 81 }, { "ROLLEDBACK", 82 }, { "END", 83 }, { "DISABLED", 84 }, { "ALLOCERR", 85 },
				{ "STRELERR", 86 }, { "OPENERR", 87 }, { "SPOLBUSY", 88 }, { "SPOLERR", 89 }, { "NODEIDERR", 90 },
				{ "TASKIDERR", 91 }, { "TCIDERR", 92 }, { "DSNNOTFOUND", 93 }, { "LOADING", 94 },
				{ "MODELIDERR", 95 }, { "OUTDESCRERR", 96 }, { "PARTNERIDERR", 97 }, { "PROFILEIDERR", 98 },
				{ "NETNAMEIDERR", 99 }, { "LOCKED", 100 }, { "RECORDBUSY", 101 }, { "UOWNOTFOUND", 102 },
				{ "UOWLNOTFOUND", 103 }, { "LINKABEND", 104 }, { "CHANGED", 105 }, { "PROCESSBUSY", 106 },
				{ "ACTIVITYBUSY", 107 }, { "PROCESSERR", 108 }, { "ACTIVITYERR", 109 }, { "CONTAINERERR", 110 },
				{ "EVENTERR", 111 }, { "TOKENERR", 112 }, { "NOTFINISHED", 113 }, { "POOLERR", 114 },
				{ "TIMERERR", 115 }, { "SYMBOLERR", 116 }, { "TEMPLATERR", 117 }, { "NOTSUPERUSER", 118 },
				{ "CSDERR", 119 }, { "DUPRES", 120 }, { "RESUNAVAIL", 121 }, { "CHANNELERR", 122 },
				{ "CCSIDERR", 123 }, { "TIMEDOUT", 124 }, { "CODEPAGEERR", 125 }, { "INCOMPLETE", 126 },
				{ "APPNOTFOUND", 127 }, { "BUSY", 128 } };
		for (Object[] c : codes)
			DFHRESP_CODES.put((String) c[0], (Integer) c[1]);
	}

	private static final int CODE_START = 7;
	private static final int CODE_END = 72;
	private static final Pattern WORD = Pattern.compile("^[A-Za-z0-9][A-Za-z0-9-]*$");
	private static final Pattern LITERAL = Pattern.compile("^(?:'[^']*'|\"[^\"]*\"|[+-]?\\d+(?:\\.\\d+)?)$");
	private static final Set<String> PASS_THROUGH = new HashSet<String>(Arrays.asList("LOAD", "ADDRESS", "IGNORE"));

	private CicsNormalizer() {
	}

	private static final class Option {
		String name;
		String arg;

		Option(String name, String arg) {
			this.name = name;
			this.arg = arg;
		}
	}

	private static final class Hoist {
		final String temp;
		final String pic;
		final String expr;

		Hoist(String temp, String pic, String expr) {
			this.temp = temp;
			this.pic = pic;
			this.expr = expr;
		}
	}

	private static final class Rewrite {
		String statement;
		final List<Hoist> hoists = new ArrayList<Hoist>();
		final List<String[]> notes = new ArrayList<String[]>();
		boolean replaced;
	}

	private static boolean isComment(String line) {
		return line.length() > 6 && (line.charAt(6) == '*' || line.charAt(6) == '/');
	}

	private static String slice(String s, int from, int to) {
		if (from >= s.length())
			return "";
		return s.substring(from, Math.min(to, s.length()));
	}

	private static String codeArea(String line) {
		return isComment(line) ? "" : slice(line, CODE_START, CODE_END);
	}

	private static String spaces(int n) {
		StringBuilder b = new StringBuilder();
		for (int i = 0; i < n; i++)
			b.append(' ');
		return b.toString();
	}

	private static String padEnd(String s, int n) {
		return s.length() >= n ? s : s + spaces(n - s.length());
	}

	/** Splits an EXEC CICS body into NAME / NAME(arg) options, keeping literals and nesting intact. */
	private static List<Option> parseOptions(String body) {
		List<Option> result = new ArrayList<Option>();
		Pattern name = Pattern.compile("^[A-Za-z0-9][A-Za-z0-9-]*");
		int i = 0;
		while (i < body.length()) {
			while (i < body.length() && Character.isWhitespace(body.charAt(i)))
				i++;
			if (i >= body.length())
				break;
			Matcher m = name.matcher(body.substring(i));
			if (!m.find()) {
				i++;
				continue;
			}
			String optName = m.group().toUpperCase(Locale.ROOT);
			i += m.group().length();
			int j = i;
			while (j < body.length() && Character.isWhitespace(body.charAt(j)))
				j++;
			if (j >= body.length() || body.charAt(j) != '(') {
				result.add(new Option(optName, null));
				continue;
			}
			int depth = 0;
			char quote = 0;
			int k = j;
			for (; k < body.length(); k++) {
				char ch = body.charAt(k);
				if (quote != 0) {
					if (ch == quote)
						quote = 0;
				} else if (ch == '\'' || ch == '"')
					quote = ch;
				else if (ch == '(')
					depth++;
				else if (ch == ')' && --depth == 0)
					break;
			}
			result.add(new Option(optName, body.substring(j + 1, Math.min(k, body.length())).replaceAll("\\s+", " ").trim()));
			i = k + 1;
		}
		return result;
	}

	private static String pickCommand(List<Option> options) {
		Set<String> names = new HashSet<String>();
		for (Option o : options)
			names.add(o.name);
		String first = options.isEmpty() ? null : options.get(0).name;
		if (first == null)
			return null;
		switch (first) {
		case "SEND":
			if (names.contains("MAP"))
				return "SEND_MAP";
			if (names.contains("CONTROL"))
				return "SEND_CONTROL";
			// A bare SEND (terminal write) is closest to SEND TEXT, which the grammar supports.
			return "SEND_TEXT";
		case "RECEIVE":
			return names.contains("MAP") ? "RECEIVE_MAP" : null;
		case "READ":
		case "READPREV":
		case "DELETE":
		case "ENDBR":
			return names.contains("FILE") || names.contains("DATASET") ? first : null;
		case "HANDLE":
			if (names.contains("AID"))
				return "HANDLE_AID";
			if (names.contains("CONDITION"))
				return "HANDLE_CONDITION";
			return null;
		case "READQ":
		case "WRITEQ":
		case "DELETEQ":
		case "RETURN":
		case "XCTL":
		case "LINK":
			return first;
		case "START":
			return names.contains("TRANSID") ? "START" : null;
		default:
			return null;
		}
	}

	private static boolean argAccepted(String arg, Arg kind) {
		if (kind == Arg.NONE)
			return false;
		if (WORD.matcher(arg).matches())
			return true;
		if (kind == Arg.WORD_OR_LITERAL)
			return LITERAL.matcher(arg).matches();
		if (kind == Arg.WORD_OR_INTEGER)
			return arg.matches("^\\d+$");
		return false;
	}

	private static Rewrite rewriteCommand(String body) {
		List<Option> parsed = parseOptions(body);
		for (Option o : parsed)
			if (OPTION_ALIASES.containsKey(o.name))
				o.name = OPTION_ALIASES.get(o.name);
		if (parsed.isEmpty() || PASS_THROUGH.contains(parsed.get(0).name))
			return null;

		String commandId = pickCommand(parsed);
		String describe = parsed.get(0).name + (parsed.size() > 1 ? " " + parsed.get(1).name : "");
		Rewrite r = new Rewrite();
		if (commandId == null) {
			r.statement = "CONTINUE";
			r.replaced = true;
			r.notes.add(new String[] { "REPLACED_WITH_CONTINUE",
					"EXEC CICS " + describe + " is not supported by the parser; replaced with CONTINUE" });
			return r;
		}

		CommandSpec spec = COMMANDS.get(commandId);
		Set<String> consumed = new HashSet<String>(spec.prefix);
		List<String> out = new ArrayList<String>(spec.prefix);
		if (spec.flagChoice != null) {
			String present = null;
			for (String n : spec.flagChoice)
				for (Option o : parsed)
					if (present == null && o.name.equals(n))
						present = n;
			out.add(present != null ? present : spec.flagFallback);
			consumed.addAll(spec.flagChoice);
		}

		Map<String, Option> byName = new LinkedHashMap<String, Option>();
		for (Option o : parsed)
			byName.put(o.name, o);
		// CICS defaults RECEIVE MAP's INTO to the map name + 'I'; the grammar needs it explicit.
		if (commandId.equals("RECEIVE_MAP") && !byName.containsKey("INTO") && !byName.containsKey("SET")) {
			Option map = byName.get("MAP");
			String mapName = map == null || map.arg == null ? null : map.arg.replaceAll("^['\"]|['\"]$", "");
			if (mapName != null && WORD.matcher(mapName).matches()) {
				byName.put("INTO", new Option("INTO", mapName + "I"));
				r.notes.add(new String[] { "DEFAULT_MADE_EXPLICIT", "implicit INTO(" + mapName + "I) made explicit" });
			}
		}
		for (OptSpec os : spec.options) {
			Option value = byName.get(os.name);
			// CICS defaults MAPSET to the map name; the grammar requires it spelled out.
			if (value == null && os.name.equals("MAPSET") && os.required) {
				Option map = byName.get("MAP");
				if (map != null && map.arg != null)
					value = new Option("MAPSET", map.arg);
			}
			if (value == null) {
				if (os.required) {
					r.notes.add(new String[] { "REPLACED_WITH_CONTINUE",
							"EXEC CICS " + describe + " is missing required " + os.name + "; replaced with CONTINUE" });
					r.statement = "CONTINUE";
					r.replaced = true;
					r.hoists.clear();
					return r;
				}
				continue;
			}
			consumed.add(os.name);
			if (os.arg == Arg.NONE) {
				if (value.arg != null)
					r.notes.add(new String[] { "DROPPED_OPTION",
							os.name + "(" + value.arg + ") reduced to " + os.name + "; argument not supported" });
				out.add(os.name);
				continue;
			}
			if (value.arg == null) {
				r.notes.add(new String[] { "DROPPED_OPTION", os.name + " without an argument dropped" });
				continue;
			}
			if (argAccepted(value.arg, os.arg)) {
				out.add(os.name + "(" + value.arg + ")");
				continue;
			}
			if (os.hoistPic != null) {
				String temp = "ALSM-CICS-" + os.name;
				r.hoists.add(new Hoist(temp, os.hoistPic, value.arg));
				out.add(os.name + "(" + temp + ")");
				r.notes.add(new String[] { "HOISTED_ARGUMENT", os.name + "(" + value.arg + ") moved into " + temp });
				continue;
			}
			if (os.required) {
				r.notes.add(new String[] { "REPLACED_WITH_CONTINUE", "EXEC CICS " + describe + ": " + os.name + "("
						+ value.arg + ") cannot be expressed; replaced with CONTINUE" });
				r.statement = "CONTINUE";
				r.replaced = true;
				r.hoists.clear();
				return r;
			}
			r.notes.add(new String[] { "DROPPED_OPTION",
					os.name + "(" + value.arg + ") dropped; only a single name or literal is accepted" });
		}
		for (Option o : parsed)
			if (!consumed.contains(o.name))
				r.notes.add(new String[] { "DROPPED_OPTION", o.name + (o.arg != null ? "(" + o.arg + ")" : "")
						+ " dropped; not supported for " + String.join(" ", spec.prefix) });

		StringBuilder statement = new StringBuilder("EXEC CICS");
		for (String t : out)
			statement.append(' ').append(t);
		r.statement = statement.toString();
		return r;
	}

	/** Splits text on whitespace without breaking quoted literals. */
	private static List<String> splitPreservingLiterals(String text) {
		List<String> pieces = new ArrayList<String>();
		StringBuilder current = new StringBuilder();
		char quote = 0;
		for (char ch : text.toCharArray()) {
			if (quote != 0) {
				current.append(ch);
				if (ch == quote)
					quote = 0;
			} else if (ch == '\'' || ch == '"') {
				quote = ch;
				current.append(ch);
			} else if (Character.isWhitespace(ch)) {
				if (current.length() > 0)
					pieces.add(current.toString());
				current.setLength(0);
			} else
				current.append(ch);
		}
		if (current.length() > 0)
			pieces.add(current.toString());
		return pieces;
	}

	/** Lays code out in area B (cols 8-72), continuing on extra lines as needed. */
	private static List<String> layout(String text, int indent, int firstLineSlack) {
		List<String> lines = new ArrayList<String>();
		int fullWidth = CODE_END - CODE_START;
		int width = fullWidth - firstLineSlack;
		int firstIndent = Math.max(indent, 4);
		String current = spaces(firstIndent);
		for (String piece : splitPreservingLiterals(text)) {
			String candidate = current.trim().isEmpty() ? current + piece : current + " " + piece;
			if (candidate.length() > width && !current.trim().isEmpty()) {
				lines.add(current);
				width = fullWidth;
				current = spaces(firstIndent + 4) + piece;
			} else
				current = candidate;
		}
		if (!current.trim().isEmpty())
			lines.add(current);
		List<String> result = new ArrayList<String>();
		for (String l : lines)
			result.add(spaces(CODE_START) + l);
		return result;
	}

	private static String commentOut(String line) {
		String padded = padEnd(line, 7);
		return padded.substring(0, 6) + "*" + padded.substring(7);
	}

	private static final Pattern DFHRESP = Pattern.compile("(?i)DFHRESP\\s*\\(\\s*([A-Z0-9]+)\\s*\\)");

	private static String inlineDfhresp(String line, int lineNo, List<Change> changes) {
		if (isComment(line))
			return line;
		String area = slice(line, CODE_START, CODE_END);
		Matcher m = DFHRESP.matcher(area);
		StringBuffer replaced = new StringBuffer();
		boolean any = false;
		while (m.find()) {
			Integer code = DFHRESP_CODES.get(m.group(1).toUpperCase(Locale.ROOT));
			if (code == null) {
				m.appendReplacement(replaced, Matcher.quoteReplacement(m.group()));
				continue;
			}
			any = true;
			changes.add(new Change("DFHRESP_INLINED", lineNo, "DFHRESP(" + m.group(1).toUpperCase(Locale.ROOT) + ") -> " + code));
			m.appendReplacement(replaced, Matcher.quoteReplacement(padEnd(String.valueOf(code), m.group().length())));
		}
		if (!any)
			return line;
		m.appendTail(replaced);
		return line.substring(0, Math.min(CODE_START, line.length())) + replaced + slice(line, CODE_END, Integer.MAX_VALUE);
	}

	/** The parser rejects an INPUT-OUTPUT SECTION header with no FILE-CONTROL beneath it. */
	private static void removeEmptyInputOutputSection(List<String> lines, List<Change> changes) {
		for (int i = 0; i < lines.size(); i++) {
			if (!codeArea(lines.get(i)).matches("(?i)^\\s*INPUT-OUTPUT\\s+SECTION\\s*\\.\\s*$"))
				continue;
			int j = i + 1;
			while (j < lines.size() && codeArea(lines.get(j)).trim().isEmpty())
				j++;
			if (j < lines.size() && codeArea(lines.get(j)).matches("(?i)^\\s*DATA\\s+DIVISION\\b.*")) {
				lines.set(i, commentOut(lines.get(i)));
				changes.add(new Change("EMPTY_SECTION_REMOVED", i + 1, "empty INPUT-OUTPUT SECTION commented out"));
			}
		}
	}

	/** ',' / ';' as plain separators (STRING A, B ...) are legal COBOL the grammar rejects. */
	private static String removeSeparatorCommas(String line, int lineNo, List<Change> changes) {
		if (isComment(line))
			return line;
		String area = slice(line, CODE_START, CODE_END);
		StringBuilder out = new StringBuilder();
		char quote = 0;
		int count = 0;
		for (int i = 0; i < area.length(); i++) {
			char ch = area.charAt(i);
			if (quote != 0) {
				if (ch == quote)
					quote = 0;
			} else if (ch == '\'' || ch == '"')
				quote = ch;
			else if ((ch == ',' || ch == ';') && (i + 1 == area.length() || area.charAt(i + 1) == ' ')) {
				out.append(' ');
				count++;
				continue;
			}
			out.append(ch);
		}
		if (count == 0)
			return line;
		changes.add(new Change("SEPARATOR_COMMA_REMOVED", lineNo, count + " separator comma(s) removed"));
		return line.substring(0, Math.min(CODE_START, line.length())) + out + slice(line, CODE_END, Integer.MAX_VALUE);
	}

	/** A lone '.' right after a sentence that already ended with '.' is an empty sentence. */
	private static void removeEmptySentences(List<String> lines, List<Change> changes) {
		boolean previousEndsWithPeriod = false;
		for (int i = 0; i < lines.size(); i++) {
			String code = codeArea(lines.get(i)).trim();
			if (code.isEmpty())
				continue;
			if (code.equals(".") && previousEndsWithPeriod) {
				lines.set(i, commentOut(lines.get(i)));
				changes.add(new Change("EMPTY_SENTENCE_REMOVED", i + 1, "empty sentence (extra period) removed"));
				continue;
			}
			previousEndsWithPeriod = code.endsWith(".");
		}
	}

	private static final Pattern BLOCK = Pattern.compile("(?is)\\bEXEC\\s+CICS\\b(.*?)\\bEND-EXEC\\b");
	private static final Pattern EIB = Pattern.compile("(?i)\\bEIB[A-Z0-9]+\\b");

	public static Result normalize(String source) {
		String eol = source.contains("\r\n") ? "\r\n" : "\n";
		String[] original = source.split("\r?\n", -1);
		List<Change> changes = new ArrayList<Change>();
		List<String> working = new ArrayList<String>();
		for (int i = 0; i < original.length; i++)
			working.add(removeSeparatorCommas(inlineDfhresp(original[i], i + 1, changes), i + 1, changes));
		removeEmptyInputOutputSection(working, changes);

		// Code areas joined with '\n', so a block spanning lines is one match and a stream offset
		// maps straight back to (line, column).
		List<String> areas = new ArrayList<String>();
		int[] lineStart = new int[working.size()];
		int offset = 0;
		StringBuilder stream = new StringBuilder();
		for (int i = 0; i < working.size(); i++) {
			String area = codeArea(working.get(i));
			areas.add(area);
			lineStart[i] = offset;
			offset += area.length() + 1;
			if (i > 0)
				stream.append('\n');
			stream.append(area);
		}

		List<String> output = new ArrayList<String>();
		Map<String, String> hoistDecls = new TreeMap<String, String>();
		int cursor = 0;
		Matcher m = BLOCK.matcher(stream);
		while (m.find()) {
			int[] start = locate(lineStart, m.start());
			int[] end = locate(lineStart, m.end());
			Rewrite rewrite = rewriteCommand(m.group(1));
			if (rewrite == null)
				continue;
			for (; cursor < start[0]; cursor++)
				output.add(working.get(cursor));
			String prefix = slice(areas.get(start[0]), 0, start[1]);
			String suffix = slice(areas.get(end[0]), end[1], Integer.MAX_VALUE);
			for (int l = start[0]; l <= end[0]; l++)
				output.add(commentOut(working.get(l)));

			StringBuilder lead = new StringBuilder(prefix.trim());
			for (Hoist h : rewrite.hoists) {
				hoistDecls.put(h.temp, h.pic);
				if (lead.length() > 0)
					lead.append(' ');
				lead.append("MOVE ").append(h.expr).append(" TO ").append(h.temp);
			}
			int indent = prefix.trim().isEmpty() ? start[1] : prefix.length() - prefix.replaceAll("^\\s+", "").length();
			if (lead.length() > 0)
				output.addAll(layout(lead.toString(), indent, 0));
			String tail = suffix.trim();
			String tailText = tail.startsWith(".") ? tail : tail.isEmpty() ? "" : " " + tail;
			if (rewrite.replaced) {
				output.addAll(layout(rewrite.statement + tailText, indent, 0));
			} else {
				// The preprocessor turns "EXEC" on the EXEC CICS line into "EXEC-CICS" (END-EXEC
				// included) and the line grows by 5 columns, so END-EXEC goes on its own line and the
				// first line keeps room for that growth.
				output.addAll(layout(rewrite.statement, indent, "EXEC-CICS".length() - "EXEC".length()));
				output.addAll(layout("END-EXEC" + tailText, indent, 0));
			}
			for (String[] n : rewrite.notes)
				changes.add(new Change(n[0], start[0] + 1, n[1]));
			cursor = end[0] + 1;
		}
		for (; cursor < working.size(); cursor++)
			output.add(working.get(cursor));
		removeEmptySentences(output, changes);

		boolean usesEib = false;
		for (String a : areas)
			if (EIB.matcher(a).find())
				usesEib = true;
		boolean needsSupport = usesEib || !hoistDecls.isEmpty();
		if (needsSupport) {
			for (int i = 0; i < output.size(); i++) {
				if (codeArea(output.get(i)).matches("(?i)^\\s*WORKING-STORAGE\\s+SECTION\\s*\\..*")) {
					output.add(i + 1, "       COPY " + SUPPORT_COPYBOOK_NAME + ".");
					changes.add(new Change("STUB_INJECTED", i + 1, "COPY " + SUPPORT_COPYBOOK_NAME + " injected"));
					break;
				}
			}
		}
		return new Result(String.join(eol, output), changes, needsSupport, hoistDecls);
	}

	private static int[] locate(int[] lineStart, int pos) {
		int line = lineStart.length - 1;
		while (line > 0 && lineStart[line] > pos)
			line--;
		return new int[] { line, pos - lineStart[line] };
	}

	/** Working-storage support copybook: a DFHEIBLK stand-in plus any hoisted temps. */
	public static String supportCopybook(Map<String, String> hoists) {
		List<String> lines = new ArrayList<String>(Arrays.asList(
				"      * Generated by ALSM: stand-in for the CICS-supplied EIB and temps",
				"      * used to pass arguments the parser cannot accept inline.",
				"       01  DFHEIBLK.",
				"           05  EIBTIME     PIC S9(7) COMP-3 VALUE 0.",
				"           05  EIBDATE     PIC S9(7) COMP-3 VALUE 0.",
				"           05  EIBTRNID    PIC X(4) VALUE SPACES.",
				"           05  EIBTASKN    PIC S9(7) COMP-3 VALUE 0.",
				"           05  EIBTRMID    PIC X(4) VALUE SPACES.",
				"           05  EIBCPOSN    PIC S9(4) COMP VALUE 0.",
				"           05  EIBCALEN    PIC S9(4) COMP VALUE 0.",
				"           05  EIBAID      PIC X VALUE SPACE.",
				"           05  EIBFN       PIC X(2) VALUE SPACES.",
				"           05  EIBRCODE    PIC X(6) VALUE SPACES.",
				"           05  EIBDS       PIC X(8) VALUE SPACES.",
				"           05  EIBREQID    PIC X(8) VALUE SPACES.",
				"           05  EIBRSRCE    PIC X(8) VALUE SPACES.",
				"           05  EIBRESP     PIC S9(8) COMP VALUE 0.",
				"           05  EIBRESP2    PIC S9(8) COMP VALUE 0."));
		if (!hoists.isEmpty()) {
			lines.add("       01  ALSM-CICS-TEMPS.");
			for (Map.Entry<String, String> h : new TreeMap<String, String>(hoists).entrySet())
				lines.add("           05  " + h.getKey() + " PIC " + h.getValue() + ".");
		}
		return String.join("\n", lines) + "\n";
	}

	/** Minimal DFHAID: attention-identifier constants. Values only need to be distinct. */
	public static String dfhaidStub() {
		List<String> lines = new ArrayList<String>(Arrays.asList(
				"      * Generated by ALSM: stand-in for the CICS-supplied DFHAID.",
				"       01  DFHAID.",
				"           02  DFHNULL   PIC X VALUE LOW-VALUE.",
				"           02  DFHENTER  PIC X VALUE QUOTE.",
				"           02  DFHCLEAR  PIC X VALUE '_'.",
				"           02  DFHPEN    PIC X VALUE '='.",
				"           02  DFHOPID   PIC X VALUE 'W'.",
				"           02  DFHMSRE   PIC X VALUE 'X'.",
				"           02  DFHSTRF   PIC X VALUE 'h'.",
				"           02  DFHTRIG   PIC X VALUE '\"'.",
				"           02  DFHPA1    PIC X VALUE '%'.",
				"           02  DFHPA2    PIC X VALUE '>'.",
				"           02  DFHPA3    PIC X VALUE ','."));
		String[] pf = { "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", "#", "@", "A", "B", "C", "D", "E", "F", "G",
				"H", "I", "[", ".", "<" };
		for (int i = 0; i < pf.length; i++)
			lines.add("           02  DFHPF" + padEnd(String.valueOf(i + 1), 5) + " PIC X VALUE '" + pf[i] + "'.");
		return String.join("\n", lines) + "\n";
	}

	/** Minimal DFHBMSCA: BMS attribute / colour constants (opaque flags in converted Java). */
	public static String dfhbmscaStub() {
		String[][] values = { { "DFHBMPEM", "SPACE" }, { "DFHBMPNL", "SPACE" }, { "DFHBMASK", "'0'" },
				{ "DFHBMUNP", "SPACE" }, { "DFHBMUNN", "'&'" }, { "DFHBMPRO", "'-'" }, { "DFHBMBRY", "'H'" },
				{ "DFHBMDAR", "'<'" }, { "DFHBMFSE", "'A'" }, { "DFHBMPRF", "'/'" }, { "DFHBMASF", "'1'" },
				{ "DFHBMASB", "'8'" }, { "DFHBMEOF", "SPACE" }, { "DFHBMCUR", "SPACE" }, { "DFHBMEC", "SPACE" },
				{ "DFHBMFLG", "SPACE" }, { "DFHBMDET", "HIGH-VALUE" }, { "DFHSA", "SPACE" }, { "DFHCOLOR", "SPACE" },
				{ "DFHPS", "SPACE" }, { "DFHHLT", "SPACE" }, { "DFH3270", "SPACE" }, { "DFHVAL", "'A'" },
				{ "DFHOUTLN", "'B'" }, { "DFHBKTRN", "SPACE" }, { "DFHALL", "LOW-VALUE" }, { "DFHERROR", "SPACE" },
				{ "DFHDFT", "HIGH-VALUE" }, { "DFHDFCOL", "LOW-VALUE" }, { "DFHBLUE", "'1'" }, { "DFHRED", "'2'" },
				{ "DFHPINK", "'3'" }, { "DFHGREEN", "'4'" }, { "DFHTURQ", "'5'" }, { "DFHYELLO", "'6'" },
				{ "DFHNEUTR", "'7'" }, { "DFHBASE", "LOW-VALUE" }, { "DFHDFHI", "LOW-VALUE" }, { "DFHBLINK", "'1'" },
				{ "DFHREVRS", "'2'" }, { "DFHUNDLN", "'4'" }, { "DFHMFIL", "SPACE" }, { "DFHMENT", "SPACE" },
				{ "DFHMFE", "SPACE" }, { "DFHUNNOD", "'('" }, { "DFHUNIMD", "'I'" }, { "DFHUNNUM", "'J'" },
				{ "DFHUNNUB", "'Q'" }, { "DFHUNINT", "'R'" }, { "DFHUNNON", "')'" }, { "DFHPROTI", "'Y'" },
				{ "DFHPROTN", "'%'" } };
		List<String> lines = new ArrayList<String>(Arrays.asList(
				"      * Generated by ALSM: stand-in for the CICS-supplied DFHBMSCA.", "       01  DFHBMSCA."));
		for (String[] v : values)
			lines.add("           02  " + padEnd(v[0], 9) + " PIC X VALUE " + v[1] + ".");
		return String.join("\n", lines) + "\n";
	}

	/** Minimal SQLCA, the DB2 communication area every embedded-SQL program includes. */
	public static String sqlcaStub() {
		return String.join("\n", Arrays.asList(
				"      * Generated by ALSM: stand-in for the DB2-supplied SQLCA.",
				"       01  SQLCA.",
				"           05  SQLCAID     PIC X(8) VALUE 'SQLCA'.",
				"           05  SQLCABC     PIC S9(9) COMP VALUE 136.",
				"           05  SQLCODE     PIC S9(9) COMP VALUE 0.",
				"           05  SQLERRM.",
				"               10  SQLERRML PIC S9(4) COMP VALUE 0.",
				"               10  SQLERRMC PIC X(70) VALUE SPACES.",
				"           05  SQLERRP     PIC X(8) VALUE SPACES.",
				"           05  SQLERRD     PIC S9(9) COMP OCCURS 6 TIMES.",
				"           05  SQLWARN.",
				"               10  SQLWARN0 PIC X VALUE SPACE.",
				"               10  SQLWARN1 PIC X VALUE SPACE.",
				"               10  SQLWARN2 PIC X VALUE SPACE.",
				"               10  SQLWARN3 PIC X VALUE SPACE.",
				"               10  SQLWARN4 PIC X VALUE SPACE.",
				"               10  SQLWARN5 PIC X VALUE SPACE.",
				"               10  SQLWARN6 PIC X VALUE SPACE.",
				"               10  SQLWARN7 PIC X VALUE SPACE.",
				"           05  SQLSTATE    PIC X(5) VALUE '00000'.")) + "\n";
	}

	/** Stub copybooks for system members a project never ships, keyed by name. */
	public static Map<String, String> systemStubs(Map<String, String> hoists, boolean needsSupport) {
		Map<String, String> stubs = new LinkedHashMap<String, String>();
		stubs.put("DFHAID", dfhaidStub());
		stubs.put("DFHBMSCA", dfhbmscaStub());
		stubs.put("SQLCA", sqlcaStub());
		if (needsSupport)
			stubs.put(SUPPORT_COPYBOOK_NAME, supportCopybook(hoists));
		return Collections.unmodifiableMap(stubs);
	}
}
