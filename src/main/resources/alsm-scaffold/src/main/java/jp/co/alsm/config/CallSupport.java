package jp.co.alsm.config;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;

import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ClassUtils;

/**
 * Runs CALL 'PROGRAM' USING a b c and CALL WS-NAME USING ... for the converted programs.
 *
 * <p>A called program is the Tasklet bean named after its PROGRAM-ID (CSUTLDTC -> CsutldtcTasklet),
 * found in the Spring context when the call is made, so a dynamic CALL resolves the name held in a
 * data item at run time. The callee's call__(Object[]) entry point copies the arguments into its
 * LINKAGE items, runs, and writes them back into the same array; the caller then copies each value
 * back into its own item, which is how COBOL's call by reference behaves.
 */
public final class CallSupport {

	private CallSupport() {
	}

	public static void call(String program, Object[] arguments) {
		String name = program == null ? "" : program.trim();
		// IBM Language Environment's abend service: the program asked to end abnormally.
		if (name.equalsIgnoreCase("CEE3ABD") || name.equalsIgnoreCase("ILBOABN0"))
			throw new IllegalStateException("COBOL program ended abnormally (CALL " + name.toUpperCase()
					+ (arguments.length > 0 ? ", abend code " + asString(arguments[0]).trim() : "") + ")");
		Object target = find(name);
		if (target == null)
			throw new UnsupportedOperationException("COBOL program " + name + " is not part of this application");
		Method entry;
		try {
			entry = target.getClass().getMethod("call__", Object[].class);
		} catch (NoSuchMethodException e) {
			throw new UnsupportedOperationException("COBOL program " + name + " was not converted, so it cannot be called");
		}
		try {
			entry.invoke(target, (Object) arguments);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException)
				throw (RuntimeException) cause;
			throw new IllegalStateException("CALL " + name + " failed", cause);
		} catch (IllegalAccessException e) {
			throw new IllegalStateException("CALL " + name + " failed", e);
		}
	}

	private static String normalized(String text) {
		return text.replaceAll("[^A-Za-z0-9]", "").toLowerCase();
	}

	private static Object find(String program) {
		ApplicationContext context = ApplicationContextProvider.getInstance().getApplicationContext();
		if (context == null || program.isEmpty())
			return null;
		String wanted = normalized(program) + "tasklet";
		for (String bean : context.getBeanNamesForType(Tasklet.class)) {
			if (bean.startsWith("scopedTarget."))
				continue;
			Class<?> type = context.getType(bean);
			if (type != null && ClassUtils.getUserClass(type).getSimpleName().toLowerCase().equals(wanted))
				return context.getBean(bean);
		}
		return null;
	}

	// ------------------------------------------------- values passed between caller and callee

	public static String asString(Object value) {
		return value == null ? "" : value.toString();
	}

	public static BigDecimal asBigDecimal(Object value) {
		if (value instanceof BigDecimal)
			return (BigDecimal) value;
		if (value instanceof Number)
			return new BigDecimal(value.toString());
		String text = asString(value).trim();
		if (text.isEmpty())
			return BigDecimal.ZERO;
		try {
			return new BigDecimal(text);
		} catch (NumberFormatException e) {
			return BigDecimal.ZERO;
		}
	}

	public static int asInt(Object value) {
		return value instanceof Number ? ((Number) value).intValue() : asBigDecimal(value).intValue();
	}

	public static long asLong(Object value) {
		return value instanceof Number ? ((Number) value).longValue() : asBigDecimal(value).longValue();
	}

	public static double asDouble(Object value) {
		return value instanceof Number ? ((Number) value).doubleValue() : asBigDecimal(value).doubleValue();
	}
}
