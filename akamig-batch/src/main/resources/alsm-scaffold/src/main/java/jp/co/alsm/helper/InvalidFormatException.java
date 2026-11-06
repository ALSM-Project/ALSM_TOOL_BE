package jp.co.alsm.helper;

@SuppressWarnings("serial")
public class InvalidFormatException extends Exception {

	private String message = null;

	public InvalidFormatException(String msg) {
		this.message = msg;
	}

	public String getMessage() {
		return this.message;
	}

}