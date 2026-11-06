package jp.co.alsm.common;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import jp.co.alsm.config.ApplicationContextProvider;

/**
 * Runs the embedded SQL of a converted program (EXEC SQL ... END-EXEC) with plain JDBC on the
 * application's DataSource (spring.datasource in application.yml).
 *
 * <p>The converted code prepares a statement, binds the host variables in order, executes it and
 * reads the row back: prepareStatement / setInt / setString ... / executeQuery / resultExists /
 * getInt ... ; a cursor is saved under its name at DECLARE, opened with executeStatement(name),
 * read with fetchCursor(name) and closed with close(name). After each statement getSqlCode() is
 * what DB2 would put in SQLCODE: 0, +100 when no row was found, the negative vendor code on error.
 */
public class SqlSupport {

	private Connection connection;
	private PreparedStatement statement;
	private ResultSet current;
	private boolean hasRow;
	private int sqlCode;
	private final Map<String, PreparedStatement> saved = new HashMap<String, PreparedStatement>();
	private final Map<String, ResultSet> cursors = new HashMap<String, ResultSet>();

	private Connection connection() throws SQLException {
		if (connection == null || connection.isClosed()) {
			DataSource dataSource = ApplicationContextProvider.getBean(DataSource.class);
			connection = dataSource.getConnection();
			connection.setAutoCommit(true);
		}
		return connection;
	}

	public int getSqlCode() {
		return sqlCode;
	}

	/** SQLCODE for a failed statement: the driver's (negative) error code, -1 when it has none. */
	public int sqlCodeOf(SQLException e) {
		int code = e.getErrorCode();
		sqlCode = code == 0 ? -1 : -Math.abs(code);
		return sqlCode;
	}

	public void openConnection() throws SQLException {
		connection();
	}

	public boolean prepareStatement(String sql) throws SQLException {
		statement = connection().prepareStatement(sql.trim());
		current = null;
		hasRow = false;
		sqlCode = 0;
		return true;
	}

	public boolean prepareCall(String sql) throws SQLException {
		statement = connection().prepareCall(sql.trim());
		sqlCode = 0;
		return true;
	}

	/** Keeps the prepared statement under a cursor name (DECLARE ... CURSOR FOR). */
	public void saveStatement(String name) {
		saved.put(name.toUpperCase(), statement);
	}

	public void declareDynamicCursor(String cursor, String statementName) {
		PreparedStatement s = saved.get(statementName.toUpperCase());
		if (s != null)
			saved.put(cursor.toUpperCase(), s);
	}

	// ------------------------------------------------------------- host variables

	public void setInt(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : Integer.valueOf(number(value).intValue()));
	}

	public void setShort(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : Short.valueOf(number(value).shortValue()));
	}

	public void setLong(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : Long.valueOf(number(value).longValue()));
	}

	public void setDouble(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : Double.valueOf(number(value).doubleValue()));
	}

	public void setFloat(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : Float.valueOf(number(value).floatValue()));
	}

	public void setBigDecimal(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : number(value));
	}

	/** Text host variables are fixed-length in COBOL; the trailing blanks are not part of the value. */
	public void setString(int index, Object value) throws SQLException {
		statement.setObject(index, value == null ? null : value.toString().replaceAll("\\s+$", ""));
	}

	public void setChar(int index, Object value) throws SQLException {
		setString(index, value);
	}

	public void setObject(int index, Object value) throws SQLException {
		statement.setObject(index, value);
	}

	public void setParameter(int index, Object value) throws SQLException {
		statement.setObject(index, value);
	}

	private static BigDecimal number(Object value) {
		if (value instanceof BigDecimal)
			return (BigDecimal) value;
		String text = value.toString().trim();
		return text.isEmpty() ? BigDecimal.ZERO : new BigDecimal(text);
	}

	// ------------------------------------------------------------- execution

	/** SELECT ... INTO: runs the query and positions on its row. */
	public void executeQuery() throws SQLException {
		current = statement.executeQuery();
		hasRow = current.next();
		sqlCode = hasRow ? 0 : 100;
	}

	/** INSERT / UPDATE / DELETE: +100 when nothing was changed, as DB2 reports it. */
	public int executeUpdate() throws SQLException {
		int count = statement.executeUpdate();
		sqlCode = count == 0 ? 100 : 0;
		return count;
	}

	/**
	 * EXECUTE IMMEDIATE: runs the statement text held in a host variable. A VARCHAR host
	 * variable gives its length (49-level length item), -1 takes the whole text.
	 */
	public void executeImmediate(Object text, int length) throws SQLException {
		String sql = text == null ? "" : text.toString();
		if (length >= 0 && length < sql.length())
			sql = sql.substring(0, length);
		try (java.sql.Statement s = connection().createStatement()) {
			s.execute(sql.trim());
		}
		sqlCode = 0;
	}

	public void executeStatement() throws SQLException {
		statement.execute();
		sqlCode = 0;
	}

	/** OPEN cursor: runs the statement saved under its name. */
	public void executeStatement(String cursor) throws SQLException {
		PreparedStatement s = saved.get(cursor.toUpperCase());
		if (s == null)
			throw new SQLException("Cursor " + cursor + " was not declared", "34000", 504);
		close(cursor);
		cursors.put(cursor.toUpperCase(), s.executeQuery());
		sqlCode = 0;
	}

	public void fetchCursor(String cursor) throws SQLException {
		ResultSet rs = cursors.get(cursor.toUpperCase());
		if (rs == null)
			throw new SQLException("Cursor " + cursor + " is not open", "24501", 501);
		hasRow = rs.next();
		current = hasRow ? rs : null;
		sqlCode = hasRow ? 0 : 100;
	}

	public boolean resultExists() {
		return hasRow && current != null;
	}

	public void close(String cursor) throws SQLException {
		ResultSet rs = cursors.remove(cursor.toUpperCase());
		if (rs != null)
			rs.close();
		sqlCode = 0;
	}

	public void commit() throws SQLException {
		if (!connection().getAutoCommit())
			connection().commit();
		sqlCode = 0;
	}

	public void rollback() throws SQLException {
		if (!connection().getAutoCommit())
			connection().rollback();
		sqlCode = 0;
	}

	public void savePoint() throws SQLException {
		sqlCode = 0;
	}

	// ------------------------------------------------------------- reading the row

	private Object column(int index) throws SQLException {
		if (current == null)
			return null;
		Object value = current.getObject(index);
		return current.wasNull() ? null : value;
	}

	public Integer getInt(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : Integer.valueOf(number(v).intValue());
	}

	public Short getShort(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : Short.valueOf(number(v).shortValue());
	}

	public Long getLong(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : Long.valueOf(number(v).longValue());
	}

	public Double getDouble(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : Double.valueOf(number(v).doubleValue());
	}

	public Float getFloat(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : Float.valueOf(number(v).floatValue());
	}

	public BigDecimal getBigDecimal(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : number(v);
	}

	public String getString(int index) throws SQLException {
		Object v = column(index);
		return v == null ? null : v.toString();
	}

	public String getChar(int index) throws SQLException {
		return getString(index);
	}

	public Object getObject(int index) throws SQLException {
		return column(index);
	}

	// ------------------------------------------------------------- stored procedures

	public void registerOutParameter(int index, int sqlType) throws SQLException {
		((CallableStatement) statement).registerOutParameter(index, sqlType);
	}

	public Integer getCallableInt(int index) throws SQLException {
		Object v = ((CallableStatement) statement).getObject(index);
		return v == null ? null : Integer.valueOf(number(v).intValue());
	}

	public Long getCallableLong(int index) throws SQLException {
		Object v = ((CallableStatement) statement).getObject(index);
		return v == null ? null : Long.valueOf(number(v).longValue());
	}

	public BigDecimal getCallableBigDecimal(int index) throws SQLException {
		Object v = ((CallableStatement) statement).getObject(index);
		return v == null ? null : number(v);
	}

	public String getCallableString(int index) throws SQLException {
		Object v = ((CallableStatement) statement).getObject(index);
		return v == null ? null : v.toString();
	}
}
