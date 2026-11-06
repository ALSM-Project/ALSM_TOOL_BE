package jp.co.alsm.common;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class FileRecord {

	public static final int SEQUENTIAL_ACCESS = 0;
	public static final int RANDOM_ACCESS = 1;
	public static final int DYNAMIC_ACCESS = 2;

	public static final int OPENED_I = 0;
	public static final int OPENED_O = 1;
	public static final int OPENED_IO = 2;

	private InputStream in = null;;
	private OutputStream out = null;
	private RandomAccessFile io = null;
	private String assignName = null;
	private int mode = -1;
	/** Records are lines: each write ends with CRLF, a read stops at the end of the line. */
	private boolean isCRLF = true;
	/** DD name of the file (SELECT ... ASSIGN TO name), used to find and to report its path. */
	private String ddName = null;
	private boolean isEOF = false;
	private int minRecLen = 0;
	private int maxRecLen = 0;
	private int accessMode = SEQUENTIAL_ACCESS;
	private int fileStatus = 0;
	private boolean isLastOpRead = false;
	private byte[] spaces = null;

	public FileRecord withAssignName(String assignName) {
		this.assignName = assignName;
		return this;
	}

	public FileRecord withAccessMode(int accessMode) {
		this.accessMode = accessMode;
		return this;
	}

	public FileRecord withDdName(String ddName) {
		this.ddName = ddName;
		return this;
	}

	/**
	 * The path of the file: the job parameter of its DD name (java -jar app.jar JOB CARDFILE=cards.txt),
	 * else an environment variable or system property of that name.
	 */
	private String resolveAssignName() throws IOException {
		if (assignName != null && !assignName.trim().isEmpty())
			return assignName.trim();
		if (ddName != null) {
			String value = System.getenv(ddName);
			if (value == null || value.trim().isEmpty())
				value = System.getProperty(ddName);
			if (value != null && !value.trim().isEmpty())
				return value.trim();
		}
		String dd = ddName == null ? "<DD name>" : ddName;
		fileStatus = 35;
		throw new IOException("No file is assigned to " + dd + ": run the job with " + dd + "=<path>");
	}

	public FileRecord withCRLF(boolean isCRLF) {
		this.isCRLF = isCRLF;
		return this;
	}

	public FileRecord withRecLen(int recLen) {
		this.minRecLen = recLen;
		this.maxRecLen = recLen;
		return this;
	}

	private void clearStatus() {
		fileStatus = 0;
		isLastOpRead = false;
	}

	public void openIO() throws IOException {
		clearStatus();
		if (in != null || out != null || io != null) {
			setOpeningOpened();
		}
		io = new RandomAccessFile(resolveAssignName(), "rw");
		mode = OPENED_IO;
	}

	private void delete(long startPosition, long endPosition) throws IOException {
		if (io == null) {
			setRewriteUnOpened();
		}
		long recLen = endPosition - startPosition;
		ByteBuffer copy = ByteBuffer.allocate((int) recLen);
		FileChannel fc = io.getChannel();
		try {
			int readBytes = 0;
			if (endPosition <= startPosition || endPosition > io.length()) {
				throw new IOException();
			}
			io.setLength(io.length() - recLen);
			do {
				io.seek(endPosition);
				if ((readBytes = fc.read(copy)) <= 0) {
					break;
				}
				fc.position(startPosition);
				copy.limit(readBytes);
				copy.position(0);
				fc.write(copy);
				if (io.getFilePointer() >= io.length()) {
					break;
				}
				endPosition += recLen;
				startPosition += recLen;
				copy.clear();
			} while (true);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void openI() throws IOException {
		clearStatus();
		if (in != null || out != null || io != null) {
			setOpeningOpened();
		}
		File file;
		file = new File(resolveAssignName());
		if (!file.exists() || !file.canRead()) {
			setInputNonExistantFile();
		}
		if (accessMode == SEQUENTIAL_ACCESS) {
			// Buffered with mark/reset, so a record can stop at the end of its line.
			this.in = new java.io.BufferedInputStream(new FileInputStream(file));
		} else {
			this.io = new RandomAccessFile(file, "r");
		}
		mode = OPENED_I;
	}

	public void openO() throws IOException {
		clearStatus();
		if (in != null || out != null || io != null) {
			setOpeningOpened();
		}
		File file;
		file = new File(resolveAssignName());
		this.out = new FileOutputStream(file);
		mode = OPENED_O;
	}

	public void openExtend() throws IOException {
		if (in != null || out != null || io != null) {
			setOpeningOpened();
		}
		clearStatus();
		File file;
		file = new File(resolveAssignName());
		this.out = new FileOutputStream(file, true);
	}

	public void close() throws IOException {
		if (in == null && out == null && io == null) {
			setClosingUnOpened();
		}
		clearStatus();
		closeI();
		closeO();
		closeIO();
	}

	private void closeIO() throws IOException {
		if (io != null) {
			io.close();
			io = null;
		}
	}

	private void closeI() throws IOException {
		if (in != null) {
			in.close();
			in = null;
		}
	}

	private void closeO() throws IOException {
		if (out != null) {
			out.close();
			out = null;
		}
	}

	/** Writes a record given as text, padded with spaces to the record length. */
	public void write(String value) throws IOException {
		write(value, maxRecLen);
	}

	/** Key of a pending START: the next read skips the records before it. */
	private String startKey = null;
	private int startOffset = 0;
	private int startLength = 0;
	private boolean startAfter = false;

	/** START without KEY: reading carries on from where the file is. */
	public void start() {
	}

	/** START KEY = key: positions at the first record whose key (offset, length in the record) is not below it. */
	public void startEq(String key, int offset, int length) throws IOException {
		position(key, offset, length, false);
	}

	public void startGe(String key, int offset, int length) throws IOException {
		position(key, offset, length, false);
	}

	public void startGt(String key, int offset, int length) throws IOException {
		position(key, offset, length, true);
	}

	/*
	 * An INDEXED file is kept as its records in key order (an unloaded VSAM KSDS), so START reads
	 * it again from the beginning and the next READ NEXT returns the first record at or after
	 * the key.
	 */
	private void position(String key, int offset, int length, boolean after) throws IOException {
		if (in != null) {
			closeI();
			openI();
		} else if (io != null) {
			io.seek(0);
		}
		StringBuilder padded = new StringBuilder(key == null ? "" : key);
		while (padded.length() < length)
			padded.append(' ');
		startKey = padded.substring(0, length);
		startOffset = offset;
		startLength = length;
		startAfter = after;
		clearStatus();
	}

	/** Reads the next record as text; at end of file this ends with an EOFException (status 10). */
	public String readRecord() throws IOException {
		while (true) {
			String record = readNextRecord();
			if (startKey == null)
				return record;
			int end = Math.min(record.length(), startOffset + startLength);
			String key = startOffset < end ? record.substring(startOffset, end) : "";
			int c = key.compareTo(startKey);
			if (startAfter ? c > 0 : c >= 0) {
				startKey = null;
				return record;
			}
		}
	}

	private String readNextRecord() throws IOException {
		if (accessMode != SEQUENTIAL_ACCESS || !(in instanceof java.io.BufferedInputStream)) {
			byte[] b = new byte[maxRecLen];
			read(b);
			return Record.text(b, 0, b.length);
		}
		// Sequential text files (the usual export of a mainframe data set): a record ends at the
		// record length or at the end of the line, whichever comes first; short lines are padded.
		clearStatus();
		if (in.available() <= 0) {
			setEOF();
		}
		java.io.ByteArrayOutputStream record = new java.io.ByteArrayOutputStream();
		int c;
		while (record.size() < maxRecLen && (c = in.read()) >= 0) {
			if (c == '\n')
				break;
			if (c == '\r') {
				in.mark(1);
				if (in.read() != '\n')
					in.reset();
				break;
			}
			record.write(c);
		}
		if (record.size() == maxRecLen) {
			in.mark(2);
			int next = in.read();
			if (next == '\r') {
				in.mark(1);
				if (in.read() != '\n')
					in.reset();
			} else if (next != '\n' && next >= 0) {
				in.reset();
			}
		}
		byte[] bytes = record.toByteArray();
		byte[] padded = new byte[maxRecLen];
		java.util.Arrays.fill(padded, (byte) ' ');
		System.arraycopy(bytes, 0, padded, 0, Math.min(bytes.length, maxRecLen));
		isLastOpRead = true;
		return Record.text(padded, 0, padded.length);
	}

	public void write(String value, int length) throws IOException {
		byte[] datas = new byte[length];
		byte[] source = Record.bytes(value);
		System.arraycopy(source, 0, datas, 0, Math.min(source.length, length));
		for (int i = 0; i < datas.length; i++) {
			if (datas[i] == 0x00) {
				datas[i] = 0x20;
			}
		}
		write(datas);
	}
	
	public void write(byte[] b) throws IOException {
		write(b, 0, b.length);
	}

	private void write(byte[] b, int offset, int len) throws IOException {
		clearStatus();
		if (mode == OPENED_IO) {
			if (io != null) {
				setWriteIO();
			}
		} else {
			if (out == null) {
				setWriteUnopened();
			}
			out.write(b, offset, len);
			if (isCRLF) {
				out.write(13);
				out.write(10);
			}
		}
	}

	public void rewrite(String str) throws IOException {
		rewrite(Record.bytes(str));
	}

	private void rewrite(byte[] b) throws IOException {
		rewrite(b, 0, b.length);
	}

	private void rewrite(byte[] b, int offset, int len) throws IOException {
		clearStatus();
		if (!isLastOpRead) {
			setRewriteWithoutRead();
		}
		if (len > maxRecLen || len < minRecLen) {
			setRewriteRecordLengthMismatch();
		}
		if (mode == OPENED_IO) {
			if (io == null) {
				setWriteUnopened();
			}
			if (isCRLF) {
				io.seek(io.getFilePointer() - (len + 2));
			} else {
				io.seek(io.getFilePointer() - len);
			}
			io.write(b, offset, len);
			if (isCRLF) {
				io.seek(io.getFilePointer() + 2);
			}
			return;
		}
		throw new IOException("Rewrite without opening file " + assignName + " in I-O mode.");
	}

	public String read(int len) {
		try {
			byte[] b = new byte[len];
			return read(b) == -1 ? null : Record.text(b, 0, b.length);
		} catch (IOException ex) {
			return null;
		}
	}


	public byte[] readBytes(int len) {
		try {
			byte[] b = new byte[len];
			return read(b) == -1 ? null : b;
		} catch (IOException ex) {
			return null;
		}
	}
	
	private int read() throws IOException, EOFException {
		clearStatus();
		int ret = 0;
		if (accessMode != SEQUENTIAL_ACCESS) {
			if (io == null) {
				setReadUnopened();
			}
			if (io.length() - io.getFilePointer() <= 0) {
				setEOF();
			}
			isEOF = false;
			ret = io.read();
		} else {
			if (in == null) {
				setReadUnopened();
			}
			if (in.available() <= 0) {
				setEOF();
			}
			isEOF = false;
			ret = in.read();
		}
		return ret;
	}

	private int read(byte[] b) throws IOException, EOFException {
		return read(b, 0, b.length, -1);
	}

	private int read(byte[] b, int offset, int len, long relativeKey) throws IOException, EOFException {
		clearStatus();
		int ret = 0;
		if (accessMode != SEQUENTIAL_ACCESS) {
			if (io == null) {
				setReadUnopened();
			}
			if (relativeKey >= 0) {
				seekIO(relativeKey);
			} else if (io.length() - io.getFilePointer() <= 0) {
				setEOF();
			}
			isEOF = false;
			ret = io.read(b, offset, len);
		} else {
			if (in == null) {
				setReadUnopened();
			}
			if (in.available() <= 0) {
				setEOF();
			}
			isEOF = false;
			ret = in.read(b, offset, len);
		}
		if (isCRLF) {
			read();
			read();
		}
		return ret;
	}

	public void delete(long relativeKey) throws IOException {
		long saveFP = seekIO(relativeKey);
		if (isCRLF) {
			delete(io.getFilePointer(), io.getFilePointer() + maxRecLen + 2);
		} else {
			delete(io.getFilePointer(), io.getFilePointer() + maxRecLen);
		}
		io.seek(saveFP);
	}

	public long seekIO(long relativeKey) throws IOException {
		try {
			long saveFP = isCRLF ? (--relativeKey) * (maxRecLen + 2) : (--relativeKey) * (maxRecLen);
			io.seek(saveFP);
			return saveFP;
		} catch (IOException ie) {
			throw new IOException();
		}
	}

	public void setEOF() throws IOException {
		fileStatus = 10;
		if (isEOF) {
			throw new IOException("Attempting to read beyond EOF");
		} else {
			isEOF = true;
			throw new EOFException();
		}
	}

	public String getFileStatus() {
		if (fileStatus == 0) {
			return "00";
		}
		String ret = String.valueOf(fileStatus);
		if (ret.length() == 1) {
			return '0' + ret;
		} else if (ret.length() == 2) {
			return ret;
		} else if (ret.length() > 2) {
			return ret.substring(0, 1);
		} else {
			return "99";
		}
	}

	public void advanceLines(int lines) throws IOException {
		if (spaces == null) {
			StringBuffer spBuf = new StringBuffer(maxRecLen);
			for (int i = 0; i < maxRecLen; ++i) {
				spBuf.append(' ');
			}
			spaces = spBuf.toString().getBytes(Constant.ENCODING);
		}
		for (; lines > 0; --lines) {
			write(spaces);
		}
	}

	public void advancePage() throws IOException {
		if (spaces == null) {
			StringBuffer spBuf = new StringBuffer(maxRecLen);
			for (int i = 0; i < maxRecLen; ++i) {
				spBuf.append(' ');
			}
			spaces = spBuf.toString().getBytes(Constant.ENCODING);
		}
		spaces[0] = '\f';
		write(spaces);
		spaces[0] = ' ';
	}

	private void setInputNonExistantFile() throws IOException {
		fileStatus = 35;
		throw new IOException("Open Input on a non-existant/unreadable file. Path: " + assignName);
	}

	private void setOpeningOpened() throws IOException {
		fileStatus = 41;
		throw new IOException("Attempting to open a file which is already open. Path: " + assignName);
	}

	private void setRewriteUnOpened() throws IOException {
		fileStatus = 99;
		throw new IOException("Attempting to rewrite to a file which is not open. Path: " + assignName);
	}

	private void setClosingUnOpened() throws IOException {
		fileStatus = 42;
		throw new IOException("Attempting to close a file which is not open. Path: " + assignName);
	}

	private Exception setRewriteRecordLengthMismatch() throws IOException {
		fileStatus = 44;
		throw new IOException("Rewrite with record length not in range " + String.valueOf(minRecLen) + " and "
				+ String.valueOf(maxRecLen) + ". Path: " + assignName);
	}

	private void setRewriteWithoutRead() throws IOException {
		fileStatus = 43;
		throw new IOException(
				"Rewrite without prior read not allowed on files opened in I-O mode. Path: " + assignName);
	}

	private void setReadUnopened() throws IOException {
		fileStatus = 47;
		throw new IOException("Read operation on closed file. Path: " + assignName);
	}

	private void setWriteUnopened() throws IOException {
		fileStatus = 48;
		throw new IOException("Write operation on closed file. Path: " + assignName);
	}

	private void setWriteIO() throws IOException {
		fileStatus = 48;
		throw new IOException("Write operation on a file opened in I-O mode. Path: " + assignName);
	}

}
