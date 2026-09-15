package com.res.common;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import com.res.cobol.parser.CobolParser;
import com.res.cobol.parser.JavaCharStream;
import com.res.cobol.parser.RESCharStream;

public class RESContext {

	//Run related
	private String sourceFileName;
	private CobolParser parser=null;
// DaoNH MOD Start: Support unicode processing
//	private RESCharStream charStream=null;
	private JavaCharStream charStream=null;
	private Reader srcIn=null;
	private InputStream isIn=null;
// DaoNH MOD End: Support unicode processing
	private boolean sqlTranslated=false;
	private StringBuffer optionsVerbose=new StringBuffer();
	private boolean processVerbose=false;
	private boolean processPreprocessOnly=false;
	private boolean processParseOnly=false;
	private boolean traceOn=false;
	private int traceLevel=0;
	
	private String currencySign=null;
	private boolean decimalPointIsComma = false;
	
	public void setParser(CobolParser parser) {
		this.parser = parser;
	}
	public CobolParser getParser() {
		return parser;
	}
	// DaoNH MOD Start: Support unicode processing
//	public void setCharStream(RESCharStream charStream) {
//		this.charStream = charStream;
//	}
	public void setCharStream(JavaCharStream charStream) {
		this.charStream = charStream;
	}
//	public RESCharStream getCharStream() {
//		return charStream;
//	}
	public JavaCharStream getCharStream() {
		return charStream;
	}
	// DaoNH MOD End: Support unicode processing
	public void setSourceFileName(String sourceFileName) {
		this.sourceFileName = sourceFileName;
	}
	public String getSourceFileName() {
		return sourceFileName;
	}
	public Reader getSourceFile() {
		return srcIn;
	}
	public void setSourceFile(Reader srcIn) {
		this.srcIn=srcIn;
	}
	// DaoNH ADD Start: Support unicode processing
	public InputStream getSourceIn() {
		return isIn;
	}
	public void setSourceIn(InputStream isIn) {
		this.isIn=isIn;
	}	
	// DaoNH ADD Start: Support unicode processing
	public void setSqlTranslated(boolean sqlTranslated) {
		this.sqlTranslated = sqlTranslated;
	}
	public boolean isSqlTranslated() {
		return sqlTranslated;
	}
	public void setOptionsVerbose(StringBuffer optionsVerbose) {
		this.optionsVerbose = optionsVerbose;
	}
	public StringBuffer getOptionsVerbose() {
		return optionsVerbose;
	}
	public void setProcessVerbose(boolean processVerbose) {
		this.processVerbose = processVerbose;
	}
	public boolean isProcessVerbose() {
		return processVerbose;
	}
	public void setProcessPreprocessOnly(boolean processPreprocessOnly) {
		this.processPreprocessOnly = processPreprocessOnly;
	}
	public boolean isProcessPreprocessOnly() {
		return processPreprocessOnly;
	}
	public void setProcessParseOnly(boolean processParseOnly) {
		this.processParseOnly = processParseOnly;
	}
	public boolean isProcessParseOnly() {
		return processParseOnly;
	}
	public void setTraceOn(boolean traceOn) {
		this.traceOn = traceOn;
	}
	public boolean isTraceOn() {
		return traceOn;
	}
	public void setTraceLevel(int traceLevel) {
		this.traceLevel = traceLevel;
	}
	public int getTraceLevel() {
		return traceLevel;
	}
	public void setDecimalPointIsComma(boolean decimalPointIsComma) {
		this.decimalPointIsComma = decimalPointIsComma;
	}
	public boolean isDecimalPointIsComma() {
		return decimalPointIsComma;
	}
	public void setCurrencySign(String currencySign) {
		this.currencySign = currencySign;
	}
	public String getCurrencySign() {
		return currencySign;
	}
}
