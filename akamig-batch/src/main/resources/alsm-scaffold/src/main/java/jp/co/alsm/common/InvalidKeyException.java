package jp.co.alsm.common;

import java.io.IOException;

public class InvalidKeyException extends IOException {

	private static final long serialVersionUID = 1L;

	private String message = "Invalid key in a COBOL indexed file.";

	public String getMessage() {
		return message;
	}
	
}
