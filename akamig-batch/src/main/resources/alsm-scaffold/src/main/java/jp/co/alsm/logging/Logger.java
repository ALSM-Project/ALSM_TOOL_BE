package jp.co.alsm.logging;

import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import lombok.Getter;
import lombok.Setter;

public class Logger {

	private static String DEBUG = "[DEBUG]";
	private static String INFO = "[INFO ]";
	private static String WARN = "[WARN ]";
	private static String FATAL = "[FATAL]";
	private static String ERROR = "[ERROR]";
	private static String START = "[START]";
	private static String END = "[END  ]";

	@Setter
	@Getter
	private org.slf4j.Logger logger = LoggerFactory.getLogger(getClass());

	private Marker info = MarkerFactory.getMarker("INFO");
	private Marker warn = MarkerFactory.getMarker("WANR");
	private Marker error = MarkerFactory.getMarker("ERROR");
	private Marker debug = MarkerFactory.getMarker("DEBUG");
	private Marker fatal = MarkerFactory.getMarker("FATAL");
	private Level defaultPriority;

	public Logger() {
	}

	public Logger(Class<?> cls) {
		setLogger(LoggerFactory.getLogger(cls));
	}

	public Logger(String name) {
		setLogger(LoggerFactory.getLogger(name));
	}

	public static void destroy() {
		LoggerContext loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
		ch.qos.logback.classic.Logger rootLogger = loggerContext
				.getLogger(ch.qos.logback.classic.Logger.ROOT_LOGGER_NAME);
		rootLogger.detachAndStopAllAppenders();
	}

	public void debug(Object obj) {
		getLogger().debug(this.debug, DEBUG + obj);
	}

	public void debug(Object obj, Throwable t) {
		getLogger().debug(this.debug, DEBUG + obj, t);
	}

	public void info(Object obj) {
		getLogger().info(this.info, INFO + obj);
	}

	public void info(Object obj, Throwable t) {
		getLogger().info(this.info, INFO + obj, t);
	}

	public void warn(Object obj) {
		getLogger().warn(this.warn, WARN + obj);
	}

	public void warn(Object obj, Throwable t) {
		getLogger().warn(this.warn, WARN + obj, t);
	}

	public void fatal(Object obj) {
		getLogger().error(this.fatal, FATAL + obj);
	}

	public void fatal(Object obj, Throwable t) {
		getLogger().error(this.fatal, FATAL + obj, t);
	}

	public void error(Object obj) {
		getLogger().error(this.error, ERROR + obj);
	}

	public void error(Object obj, Throwable t) {
		getLogger().error(this.error, ERROR + obj, t);
	}

	public void start(Object obj) {
		getLogger().info(this.info, START + obj);
	}

	public void end(Object obj) {
		getLogger().info(this.info, END + obj);
	}

	public void log(Object obj) {
		getLogger().trace(this.defaultPriority.toString(), obj);
	}

	public void log(Object obj, Throwable t) {
		getLogger().trace(this.defaultPriority.toString(), obj, t);
	}

	public void setInfo() {
		this.defaultPriority = Level.INFO;
	}

	public void setWarn() {
		this.defaultPriority = Level.WARN;
	}

	public void setError() {
		this.defaultPriority = Level.ERROR;
	}

	public void setDebug() {
		this.defaultPriority = Level.DEBUG;
	}

	public void setFatal() {
		this.defaultPriority = Level.ERROR;
	}

	public boolean isDebugEnabled() {
		return getLogger().isDebugEnabled();
	}

	public boolean isInfoEnabled() {
		return getLogger().isInfoEnabled();
	}

}
