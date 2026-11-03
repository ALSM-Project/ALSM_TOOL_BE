package com.res.java.lib;

public class RunTimeUtil {
	
	private static RunTimeUtil runTimeUtil=null;
	
	private RunTimeUtil() {}
	
	public static RunTimeUtil getInstance() {
		if(runTimeUtil==null){
			runTimeUtil=new RunTimeUtil();
		}
		return runTimeUtil;
	}
	
	public String stripQuotes(String in) {
		return stripQuotes(in,true);
	}
	
	public String stripQuotes(String in,boolean trim) {
		if((in=in.trim()).length()>0&&(in.charAt(0)=='\''||in.charAt(0)=='\"')) {
			if(in.lastIndexOf(in.charAt(0))>0)
				in=in.substring(1,in.lastIndexOf(in.charAt(0)));
			if(trim) return in.trim();
		}
		return in;
	}
	
	public boolean isWhiteSpaces(char c) {
		return (c==' '||c=='\t'||c=='\r'||c=='\n'||c=='\f');
	}
	
	public String stripTrailingBlanks(String in) {
		if(in==null) return "";
		String s;
		return (in.substring(0,(in.indexOf(s=in.trim())+s.length())));
	}
	
	public void reportError(String s,boolean exit) {
		System.out.println(s);
		if(exit) {
			throw new com.res.common.CobolTranslationException(s);
		}
	}
	
}
