package com.res.java.util;

import com.res.cobol.Main;
import com.res.common.RESConfig;
import com.res.java.translation.symbol.SymbolConstants;
import com.res.java.translation.symbol.SymbolProperties;

public class DataClassFileWriter extends ClassFileWriter {
	//20210703 Update S
	//public DataClassFileWriter(SymbolProperties props){
	public DataClassFileWriter(SymbolProperties props,SymbolProperties parent){
	//20210703 Update E		
		super(Files.openClassFile(props,true));
		setData(true);
		if(RESConfig.getInstance().isGenerateBeanInfo()) {
			setBeanInfoPrintStream(Files.openBeanInfoFile(props,true));
		}
		//20210703 Update S
		//printHeader(props);
		printHeader(props,parent);
		//20210703 Update E
	}
	//package jp.co.alsm.batch
	//20210703 update S
	//public void printHeader(SymbolProperties props) {
	public void printHeader(SymbolProperties props, SymbolProperties parent) {
	//20210703 update E
		String s;
		if ((s = NameUtil.getPackageName(props,false))!=null&&s.length()>0) {
			s=s.replace("src.main.java.", "");
			//20210703 update S
			//s+="."+ props.getJavaName1();
			s+="."+ parent.getJavaName1();
			//20210703 update E
			println("package "+s+";");
			println("package "+s+";",true);
		}
		if(props.getType()==SymbolConstants.PROGRAM) {
			//println("/************************************************************************");
			//println(" ** RES generated this class file for Data Encapsulation "+
			//		" of program "+props.getDataName().toUpperCase()+" in source file "
			//		+Main.getContext().getSourceFileName().toUpperCase());
		} else {
			//println("/************************************************************************");
			//println(" ** RES generated this class file from Data Level "+
			//		props.getDataName().toUpperCase()+" in source file "
			//			+Main.getContext().getSourceFileName().toUpperCase());
		}
		/*
		println(" ************************************************************************");
		println(" ** Generated at time "+RESUtil.getTime()+ 
				" on "+RESUtil.getDayOfWeek()+
				", "+RESUtil.getDate());
		if(Main.getContext().isProcessVerbose()) {
			println(" ** "+Main.getContext().getOptionsVerbose().toString());
		}
		println(" ************************************************************************");
		*/
	}
	
}
