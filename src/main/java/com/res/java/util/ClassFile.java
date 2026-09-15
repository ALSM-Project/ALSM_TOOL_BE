package com.res.java.util;

import java.io.PrintWriter;
import java.util.Stack;

import com.res.common.RESConfig;
import com.res.java.translation.symbol.SymbolProperties;
import com.res.java.translation.symbol.SymbolTable;

public class ClassFile {
	
	public static ClassFileWriter current=null;
	public static Stack<ClassFileWriter> files=new Stack<ClassFileWriter>();
	
	public PrintWriter out = null;
	public boolean importJavaUtil = false;
	public boolean importJavaIo = false;
	//20210703 add s
	private static SymbolProperties parent=null;
	//20210703 add E 
	public static void doProgramScope(SymbolProperties props, boolean dataLevel) {
		if (files==null)
			files=new Stack<ClassFileWriter>();
		if(dataLevel)
			//20210703 Update S
			//current=new DataClassFileWriter(props);
			current=new DataClassFileWriter(props,parent);
			//20210703 Update E
		else  {
			if(SymbolTable.getScope().isCurrentProgramTopLevel()) {	
				//20210703 add S
				parent =props;	
				//20210703 add E
				current=new ProgramClassFileWriter(props);
			} else
				((ProgramClassFileWriter)current).printHeader(props,true);
		}
		files.push(current);
	}

	//QuyTV S
	public static void doProgramScopeJobTemplate() {	
		SymbolProperties pro= SymbolTable.getScope().getCurrentProgram();
		pro.setFlgJobFile(true);
		current=new ProgramClassFileWriter(pro,true,1);
		files.push(current);
	}
	//QuyTV E
	// EMT ADD STEP 2025
	public static void doProgramScopeStepTemplate() {	
		SymbolProperties pro= SymbolTable.getScope().getCurrentProgram();
		pro.setFlgStepFile(true);
		current=new ProgramClassFileWriter(pro,true,2);
		files.push(current);
	}	
	//
	// EMT ADD CONST 2025
	public static void doProgramScopeConstTemplate() {	
		SymbolProperties pro= SymbolTable.getScope().getCurrentProgram();
		pro.setFlgConstFile(true);
		current=new ProgramClassFileWriter(pro,true,3);
		files.push(current);
	}	
	//
	// EMT ADD TASKLET 2025
	public static void doProgramScopeTaskLetTemplate() {	
		SymbolProperties pro= SymbolTable.getScope().getCurrentProgram();
		pro.setFlgTaskLet(true);
		current=new ProgramClassFileWriter(pro);
		files.push(current);
	}	
	//
	public static void doProgramScope(ClassFileWriter gen) {
		current=gen;
		files.push(current);
	}

	public static void endProgramScope() {
		if(current==null)
			return;
		current.println("}");
		current.println("}",true);
		if(files.peek().isData()||SymbolTable.getScope().isCurrentProgramTopLevel()) {			
		current.close();
		
		} else 
			current.backTab();
		files.pop();
		if(files.size()>0) {
			current=files.peek();
		} else {
			current=null;
		}
		// TODO: Setting back outputDir of Resconfig object to null 
	}

	public static void doMethodScope(String line,boolean bi) {
		if(current==null)
			return;
		current.println(line,bi);
	}

	public static void endMethodScope(boolean bi) {
		if(current==null)
			return;
		current.println("}",bi);
	}

	public static void println(String line,boolean bi) {
		if(current==null)
			return;
		current.println(line,bi);
	}
	
	public static void print(String line,boolean bi) {
		if(current==null)
			return;
		current.print(line,bi);
	}
	
	public static void printPlainln(String line,boolean bi) {
		if(current==null)
			return;
		current.println(line,bi);
	}
	
	public static void printPlain(String line,boolean bi) {
		if(current==null)
			return;
		current.print(line,bi);
	}
	
	public static void printPlain(char c,boolean bi) {
		if(current==null)
			return;
		current.printPlain(c,bi);
	}

	public static void tab(boolean bi) {
		if(current==null)
			return;
		current.tab(bi);
	}
	
	public static void backTab(boolean bi) {
		if(current==null)
			return;
		current.backTab(bi);
	}
	
	////
	
	public static void doMethodScope(String line) {
		if(current==null)
			return;
		current.println(line);
	}

	public static void endMethodScope() {
		if(current==null)
			return;
		current.println("}");
	}

	public static void println(String line) {
		if(current==null)
			return;
		current.println(line);
	}
	
	public static void print(String line) {
		if(current==null)
			return;
		current.print(line);
	}
	
	public static void printPlainln(String line) {
		if(current==null)
			return;
		current.println(line);
	}
	
	public static void printPlain(String line) {
		if(current==null)
			return;
		current.print(line);
	}
	
	public static void printPlain(char c) {
		if(current==null)
			return;
		current.printPlain(c);
	}

	public static void tab() {
		if(current==null)
			return;
		current.tab();
	}
	
	public static void backTab() {
		if(current==null)
			return;
		current.backTab();
	}


	
	
}
