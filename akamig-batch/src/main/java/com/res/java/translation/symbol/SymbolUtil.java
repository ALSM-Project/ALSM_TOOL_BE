package com.res.java.translation.symbol;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.regex.Pattern;

//import org.omg.PortableServer.POA;

import com.res.cobol.Main;
import com.res.common.RESConfig;
import com.res.java.lib.CobolSymbol;
import com.res.java.lib.Console;
import com.res.java.lib.Constants;
import com.res.java.lib.FieldFormat;
import com.res.java.lib.exceptions.InvalidCobolFormatException;
import com.res.java.translation.engine.Cobol2Java;
import com.res.java.translation.engine.TranUtil;
import com.res.java.util.ClassFile;
import com.res.java.util.NameUtil;


public class SymbolUtil {

	private static SymbolUtil symbolUtil=null;
	private SymbolUtil() {}
	
	public static SymbolUtil getInstance() {
		if(symbolUtil==null)
			symbolUtil=new SymbolUtil();
		return symbolUtil;
	}
	

	private ArrayList<String[]> copy_List ;
	private ArrayList<String> headerLst = new ArrayList<String>();
	private ArrayList<String> classLst = new ArrayList<String>();
	private ArrayList<String> varLst = new ArrayList<String>();
	private ArrayList<String> constructorsLst = new ArrayList<String>();
	private ArrayList<String> constructorsLst1 = new ArrayList<String>();
	private ArrayList<String> setgetLst = new ArrayList<String>();
	private ArrayList<String> modelNameLst = new ArrayList<String>();
	private String modelName =null;
	
	private boolean  bFlag = false;
	private boolean isFirstRun = true;
	
	private String strParent;
	private int idxFiller;
	
	/** Level-01 items already written to the Model, so the same item is never declared twice. */
	private java.util.Set<String> emittedModelAttrs = new java.util.HashSet<String>();
	public void setCopyList(ArrayList<String[]> value) {
		this.copy_List = value;
	}
	public ArrayList<String[]> getCopyList(){
		return copy_List;
	}
	

	private void doWriteModel(SymbolProperties props,boolean programScope)
	{
		if(programScope) {
			props.setIsFormat(false);
			ClassFile.doProgramScope(props,true);
			
			for(String item : headerLst){			
				ClassFile.println(item);
			}
			
			for(String item : classLst){			
				if (item.equals("tab()")) ClassFile.tab();
				else if (item.equals("backTab()")) ClassFile.backTab();
				else ClassFile.println(item);
			}

			// A data item reached through more than one path (a PERFORM VARYING index, a level-88
			// parent, a copybook pulled in twice) is queued once per path. Java rejects a second
			// declaration of the same field, and that one error also stops Lombok from generating
			// any getter or setter, so every field is written once.
			java.util.Set<String> declaredFields = new java.util.HashSet<String>();
			java.util.regex.Pattern fieldDeclaration =
					java.util.regex.Pattern.compile("^private\\s+.+?\\s+(\\w+)\\s*(?:=.*)?;\\s*$");
			for(String item : varLst){
				if (item.equals("tab()")) ClassFile.tab();
				else if (item.equals("backTab()")) ClassFile.backTab();
				else {
					java.util.regex.Matcher m = fieldDeclaration.matcher(item);
					if (m.matches() && !declaredFields.add(m.group(1))) continue;
					ClassFile.println(item);
				}
			}

			for(String item : constructorsLst){
				if (item.equals("tab()")) ClassFile.tab();
				else if (item.equals("backTab()")) ClassFile.backTab();
				else ClassFile.println(item);
			}
			ClassFile.backTab();
			ClassFile.println("}");
			
			for(String item : setgetLst){		
				if (item.equals("tab()")) ClassFile.tab();
				else if (item.equals("backTab()")) ClassFile.backTab();
				else ClassFile.println(item);
			}
		//	ClassFile.backTab();
		//	ClassFile.println("}");
		
		}
				
	}
	public void Reset() {
		headerLst = new ArrayList<String>();
		classLst = new ArrayList<String>();
		varLst = new ArrayList<String>();
		constructorsLst = new ArrayList<String>();
		// Left out, these two carried one program's field initialisers and 88-level getters
		// into the next program of the same run (Model01 received CBACT01C's applResult).
		constructorsLst1 = new ArrayList<String>();
		Cobol2Java.level88Getters.clear();
		setgetLst = new ArrayList<String>();
		modelNameLst = new ArrayList<String>();

		bFlag = false;
		isFirstRun = true;
		emittedModelAttrs = new java.util.HashSet<String>();	
	}

	public void doClassBegin(SymbolProperties props, String name, boolean programScope) {
		modelName = SymbolTable.getScope().getFirstProgram().getJavaName2() + "Model";
		if (programScope) {
			if (isFirstRun) {
				headerLst.add("");
				headerLst.add("import org.springframework.stereotype.Component;");
				headerLst.add("");
				headerLst.add("import java.math.BigDecimal;");
				headerLst.add("import jp.co.alsm.common.Constant;");
				headerLst.add("import jp.co.alsm.common.Record;");
				headerLst.add("import jp.co.alsm.helper.StringHelper;");
				classLst.add("import lombok.Getter;");
				classLst.add("import lombok.Setter;");
				classLst.add("");
				classLst.add("@Component");
				classLst.add("@Getter");
				classLst.add("@Setter");
				classLst.add("public class " + modelName + " {");
				classLst.add("");
				constructorsLst.add("tab()");
				constructorsLst.add("");
				constructorsLst.add("public " + modelName + "(){");
				isFirstRun=false;
			}
			SymbolProperties tmpSymProp = props;
			/*
			 * Khai bao su dung trong FILE
			 *        FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           				LABEL RECORD STANDARD.
       				  01  WKB-REC.
            		  02 FILLER                        PIC  X(250).
			 */
			
			if (props.getGroupDictionary().get(0).contains("filler")) {
				/*KHONG XOA
				varLst.add("tab()");
				constructorsLst.add("tab()");
				if (tmpSymProp != null) {
					varLst.add("private " + tmpSymProp.getJavaName2() + " " + tmpSymProp.getJavaName1() + ";");
					// constructorsLst.add("tab()");
					constructorsLst.add(tmpSymProp.getJavaName1() + " = new " + tmpSymProp.getJavaName2() + "();");

					setgetLst.add("");
					setgetLst.add("@Getter");
					setgetLst.add("@Setter");
					setgetLst.add("public class " + tmpSymProp.getJavaName2() + "() {");
					setgetLst.add("tab()");
					setgetLst.add("");

					setgetLst.add("public static final int ADJUST_LENGTH = " + tmpSymProp.getAdjustedLength() + ";");
					setgetLst.add("");
					// Loop and print inner class attributes
					ArrayList<SymbolProperties> childSP = null;
					childSP = tmpSymProp.getChildren();
					if (childSP != null && !childSP.isEmpty()) {
						for (SymbolProperties child : childSP) {
							String javaName1 =child.getJavaName1();
							javaName1 = javaName1.substring(0,javaName1.length()-2) + "1";
							String javaName2 =child.getJavaName2();
							javaName2 = javaName2.substring(0,javaName2.length()-2) + "1";
							setgetLst.add("@CharactersFormat(format = \"" + child.getPictureString() + "\")");
							setgetLst.add("private String " + javaName1 + ";");
							setgetLst.add("");

							setgetLst.add("public String get" + javaName2 + "() {");
							setgetLst.add("	return " + javaName1 + ";");
							setgetLst.add("}");
							setgetLst.add("");

							setgetLst.add("public void set" + javaName2 + "(String value) {");
							setgetLst.add("	" + javaName1 + "	= value;");
							setgetLst.add("}");
							setgetLst.add("");

							setgetLst.add("public void write(String value) {");
							setgetLst.add("	set" + javaName2 + "(value.substring(0,"
									+ tmpSymProp.getAdjustedLength() + "));");
							setgetLst.add("}");
							setgetLst.add("");

							setgetLst.add("@Override");
							setgetLst.add("public String toString() {");
							setgetLst.add("	return get" + javaName2 + "();");
							setgetLst.add("}");
						}
						setgetLst.add("backTab()");
						setgetLst.add("}");
					}
				}	KHONG XOA*/
			} else {
				bFlag=false;
				printModelAttrs(props, programScope);
				constructorsLst.add("backTab()");
			}
		} else {

			classLst.add("public class " + modelName + " {");
			constructorsLst.add("tab()");
			constructorsLst.add("public " + modelName + "{");
		}
		if (RESConfig.getInstance().isUsePointers())
			ClassFile.println("public BytesField " + props.getJavaName1() + "Fld_ = new BytesField(this,"
					+ String.valueOf(props.getUnAdjustedOffset()) + ',' + String.valueOf(props.getLength()) + ");");

	}

	public void doClassEnding(SymbolProperties props,boolean programScope)
	{
	//	String name1=props.getJavaName1();
	//	String name2=props.getJavaName2();
		
	 //   int adjLen=props.getAdjustedLength();

	    // DAONH MOD Start: Print valid Model attributes only
//		if(props.getType()!=SymbolConstants.PROGRAM) {
	    if(props.getType()!=SymbolConstants.PROGRAM && !props.getGroupDictionary().get(0).contains("filler")) {
		// DAONH MOD Start: Print valid Model attributes only
			
			// DAONH DEL Start: No need
//			printInitialize(props);
			// DAONH DEL Start: No need

//			printConstructors(props, adjLen);
			// DAONH DEL Start: Replace by lombok getter/setter
//			printStringAccessors(props);

//			printGroupAccessors(props, true, getGroupOccursIdx(props));
			// DAONH DEL End: Replace by lombok getter/setter
	    	/* 20210901 Delete S
			if(props.isIndexedFileRecord()){
				TranUtil.get().printSetSQLProperties(props);
				TranUtil.get().printgetSQLResults(props);
				TranUtil.get().printSetStartSQLProperties(props);
			}
	    	 20210901 Delete E	 */		
		}
				
		if(props.getType()!=SymbolConstants.PROGRAM) {
			
// DaoNH MOD Start: Closing the Model class
			ClassFile.backTab();
			if(programScope)
			{
				ClassFile.endProgramScope();	
			}
			else
			{
				// endProgramScope() writes the closing brace of the class itself; writing one
				// here as well left every generated Model file with an unmatched extra "}".
				ClassFile.endProgramScope();
			}
// DaoNH MOD End: Closing the Model class
// DAONH DEL Start: TODO check later
//			printClassAccessors(props, name1, name2);
//			
//			printClass88s(props);
// DAONH DEL Start: TODO check later
			
		}
	}

	private static String modifier0="";
	private static String modifier1="";
	private static String getSubscripts="";
	private static String modifier2="";
	private static String modifier3="";
	private static String modifier4="";
	private static String setSubscripts="";
	
	private void printGroupAccessors(SymbolProperties props,
			boolean isTopLevel, int idx) {

		if(!props.getRef()&&!props.getMod()) return;
		int relOff = -1;
		if(isTopLevel) {
			ClassFile.doMethodScope("public byte[] getBytes(" +getGroupOccursSignature(props)+
				") {");
			if(RESConfig.getInstance().getOptimizeAlgorithm()==2&&props.isUsedNativeJavaTypes()) {
				idx = 0;
				relOff=0;
			}
			else idx = -2;
		}
		else{
			ClassFile.doMethodScope("public byte[] get" +
					props.getJavaName2() +
					"(" +getGroupOccursSignature(props)+
			") {");
			if(RESConfig.getInstance().getOptimizeAlgorithm()==2&&props.isUsedNativeJavaTypes())
				relOff=props.getUnAdjustedOffset();
		}
		ClassFile.tab();
		
		if(isSingleTopLevel(props)&&!props.isUsedNativeJavaTypes()){
			printGroupGetClassString(props);
		}
		else
		if(props.hasChildren()) {
			ClassFile.println("CobolBytes " +
					SymbolTable.TOSTRING_REF_STRING_ +
					"=new CobolBytes("+String.valueOf(props.getLength())+",true);");
			for(Iterator<SymbolProperties> ite=props.getChildren().iterator();ite.hasNext();) {
				SymbolProperties child=ite.next();
				if(child.getRedefines()!=null||child.getLevelNumber()==88||child.getLevelNumber()==66) continue;
				child.setIndexesWorkSpace(props.getIndexesWorkSpace());
				printGroupGetBytesOccursLoop(child,idx,relOff) ;
			}
			ClassFile.println("return " +
					SymbolTable.TOSTRING_REF_STRING_ +
					".getBytes();");
		}
		else
			ClassFile.println("return new b[0];");
		ClassFile.backTab();
		ClassFile.endMethodScope();
		if(isTopLevel) {
			ClassFile.doMethodScope("public void valueOf(byte[] val" +
					(((getSubscripts=getGroupOccursSignature(props)).length()>0)?(","+getSubscripts):"")+
			
				") {");
			if(RESConfig.getInstance().getOptimizeAlgorithm()==2&&props.isUsedNativeJavaTypes())
				idx = -1;
			else idx = -2;
		}
		else {
			ClassFile.doMethodScope("public void set" +
					props.getJavaName2() +
					"(byte[] val" +(((getSubscripts=getGroupOccursSignature(props)).length()>0)?(","+getSubscripts):"")+
			") {");
			relOff=props.getUnAdjustedOffset();
		}
		ClassFile.tab();
		if(isSingleTopLevel(props)&&!props.isUsedNativeJavaTypes()){
			printGroupSetClassString(props);
		}
		else
		if(props.hasChildren()) {
			ClassFile.println("CobolBytes " +
					SymbolTable.TOSTRING_REF_STRING_ +
					"=new CobolBytes(val,"+String.valueOf(props.getLength())+");");
			for(Iterator<SymbolProperties> ite=props.getChildren().iterator();ite.hasNext();) {
				SymbolProperties child=ite.next();
				if(child.getRedefines()!=null||child.getLevelNumber()==88||child.getLevelNumber()==66) continue;
				child.setIndexesWorkSpace(props.getIndexesWorkSpace());
				printGroupSetBytesOccursLoop(child,"val",idx,relOff);
			}
		}
		else
			ClassFile.println("return;");
		ClassFile.backTab();
		ClassFile.endMethodScope();
	}

	private boolean isSingleTopLevel(SymbolProperties props) {
		if(!props.isUsedNativeJavaTypes()) return true;
		while( props.hasChildren()&&
				!(props=props.getChildren().get(0)).isUsedNativeJavaTypes()&&
				props.getIsFiller())
			;
		 return props.getChildren()!=null&&props.getChildren().size()==1&&!props.getChildren().get(0).isUsedNativeJavaTypes();
	}
	
	private void printGroupGetClassString(SymbolProperties props) {
		props.setIndexesWorkSpace(null);
		ClassFile.println("return "+
				getGetterString(props,-2,-1)+";");
	}

	private void printGroupSetClassString(SymbolProperties props) {
		props.setIndexesWorkSpace(null);
		ClassFile.println(getSetterString(props,-2,-1));
	}
	
	//Of the form...
	//super.setDisplayLong(0,13,bytes_.getDisplayLong(0,13,false,false,false),false,false,false);
	private void printGroupSetCobolBytesString(SymbolProperties props,int modifiedIdx,int relativeUnadjustedOffset) {
		int i=0;
		/*
		if(modifiedIdx>=0&&modifiedIdx<=props.getNoOccursSubscripts()) {
			String s=getGetterString(props,modifiedIdx,relativeUnadjustedOffset);
			ClassFile.println(getSetterString(props,-1,-1).replace(",val", ","+SymbolTable.TOSTRING_REF_STRING_+
					s.substring(((i=s.indexOf('.'))<0)?0:i).replace(",val", ",")
					)+";");
		}
		 else*/ {
			String s=getGetterString(props,modifiedIdx,relativeUnadjustedOffset);
			ClassFile.println(getSetterString(props,modifiedIdx,-1).replace(",val", ","+SymbolTable.TOSTRING_REF_STRING_+
					s.substring(((i=s.indexOf('.'))<0)?0:i).replace(",val", ",")
					)+";");
		}
	}
	
	//Of the form...
	//bytes_.setDisplayLong(0,13,super.getDisplayLong(0,13,false,false,false),false,false,false);
	private void printGroupGetCobolBytesString(SymbolProperties props,int modifiedIdx,int relativeUnadjustedOffset) {
		int i=0;

		/*if(modifiedIdx>=0&&modifiedIdx<=props.getNoOccursSubscripts()) {
			String s=getSetterString(props,modifiedIdx,relativeUnadjustedOffset);
			ClassFile.println(SymbolTable.TOSTRING_REF_STRING_+s.substring(((i=s.indexOf('.'))<0)?0:i).replace(",val", ","+
					getGetterString(props,-1,relativeUnadjustedOffset))+";");
		} 
			else*/ {
			String s=getSetterString(props,modifiedIdx,relativeUnadjustedOffset);
			ClassFile.println(SymbolTable.TOSTRING_REF_STRING_+s.substring(((i=s.indexOf('.'))<0)?0:i).replace(",val", ","+
					getGetterString(props,modifiedIdx,-1))+";");
		}
	}
	
	// Of the form...
	// setName(bytes_.getDisplayLong(0,13,false,false,false));
	String getGroupSetterString(SymbolProperties props, String value, int modifiedIdx,int relativeUnadjustedOffset)
	{int i=0;
		String s=getSetterString(props,modifiedIdx,relativeUnadjustedOffset);
		return SymbolTable.TOSTRING_REF_STRING_+s.substring(((i=s.indexOf('.'))<0)?0:i).replace(",val", ","+value);

	}

	//Of the form...
	//bytes_.setDisplayLong(0,13,getName(),false,false,false);
	String getGroupGetterString(SymbolProperties props,int modifiedIdx,int relativeUnadjustedOffset)
	{int i=0;
		String s=getGetterString(props,modifiedIdx,relativeUnadjustedOffset);
		return SymbolTable.TOSTRING_REF_STRING_+s.substring(((i=s.indexOf('.'))<0)?0:i);
	}

	//Of the form...
	//for(int idx_=0;idx_<10240;++idx_) {
	//   bytes_.setDisplayLong(0+13*idx_,13,getName(idx_+1),false,false,false);
	//   bytes_.setDisplayLong(13*idx_,13,getName2(idx_+1),false,false,false);
	//   bytes_.setDisplayLong(26*idx_,16,getName3(idx_+1),false,false,false);
	//}
	public void printGroupGetBytesOccursLoop(SymbolProperties props,int modifiedIdx,int relOff) {
		if(props.isOccurs()) {
			//Of form... for(int idx1=0;idx1<10240;++idx_) {
			ClassFile.println("for(int idx"+String.valueOf(props.getNoOccursSubscripts()) +
					"=" +String.valueOf(props.getMaxOccursInt())+
					";idx"+String.valueOf(props.getNoOccursSubscripts()) +
					">=1" +
				";--" +
				"idx"+String.valueOf(props.getNoOccursSubscripts()) +
				") {");
			ClassFile.tab();
			if(props.getIndexesWorkSpace()==null) props.setIndexesWorkSpace(new ArrayList<String>());
			props.getIndexesWorkSpace().add("idx"+String.valueOf(props.getNoOccursSubscripts()));
		}

		if((!props.getRef()&&!props.getMod())||(props.getIsFiller()&&props.isGroupData())) {
			if(props.hasChildren()) {
				for(SymbolProperties child:props.getChildren()){
					child.setIndexesWorkSpace(props.getIndexesWorkSpace());
					//Of the form... bytes_.setDisplayLong(0+13*--idx1,13,getName(++idx1),false,false,false);
					printGroupGetBytesOccursLoop(child,modifiedIdx,relOff);
				}
			}
			
		} 
		else
		if(!props.isUsedNativeJavaTypes())
			printGroupGetCobolBytesString(props,modifiedIdx,relOff);
		else 
			if(props.isOccurs()) {
				if(props.isGroupData()&&props.getIsFiller()) {
					for(SymbolProperties child:props.getChildren()){
						child.setIndexesWorkSpace(props.getIndexesWorkSpace());
						if(child.getIsFiller()&&child.isElementData()) 
							printGroupGetBytesString(child,modifiedIdx,relOff);
						else
						//Recursion
							printGroupGetBytesOccursLoop(child,modifiedIdx,relOff);
					}
					
				} else {
					//Of the form... bytes_.setDisplayLong(0+13*idx_,13,getName(idx_+1),false,false,false);
					printGroupGetBytesString(props,modifiedIdx,relOff);
				}
			}
			//Of the form... bytes_.setDisplayLong(0+13*idx_,13,getName(idx_+1),false,false,false);
			else
				printGroupGetBytesString(props,modifiedIdx,relOff);
		if(props.isOccurs()) {
			ClassFile.backTab();
			ClassFile.println("}");
			if(props.getIndexesWorkSpace()!=null)
				props.getIndexesWorkSpace().remove(props.getIndexesWorkSpace().size()-1);
		}
	}

	//Of the form...
	//bytes_.setDisplayLong(0+13*--idx1,13,getName(++idx1),false,false,false);
	void printGroupGetBytesString(SymbolProperties props, int modifiedIdx,int relOff)
	{
		//if(props.getIsFiller())
			//printGroupGetCobolBytesString(props,relOff);
		//else {
		ClassFile.println(getGroupSetterString(props,SymbolUtil.getLocalName(NameUtil.getJavaName(props,false)),modifiedIdx,relOff)+";");
		//}
	}
	//Of the form...
	//for(int idx1=0;idx1<10240;++idx1) {
	//	setName1(bytes_.getDisplayLong(0+13*--idx1,13,false,false,false),++idx1);
	//	setName2(bytes_.getDisplayLong(0+13*--idx1+13,13,false,false,false),++idx1);
	//	setName3(bytes_.toString(0+13*--idx1+26,19),++idx1);
	//}
	 void printGroupSetBytesOccursLoop(SymbolProperties props,String value,int modifiedIdx,int relOff) {
		if(props.isOccurs()) {
			ClassFile.println("for(int idx"+String.valueOf(props.getNoOccursSubscripts()) +
					"=" +String.valueOf(props.getMaxOccursInt())+
					";idx"+String.valueOf(props.getNoOccursSubscripts()) +
					">=1" +
				";--" +
				"idx"+String.valueOf(props.getNoOccursSubscripts()) +
				") {");
/*
			ClassFile.println("for(int " +
					"idx"+String.valueOf(props.getNoOccursSubscripts()) +
					"=1;" +
					"idx"+String.valueOf(props.getNoOccursSubscripts()) +
					"<=" +
				String.valueOf(props.getMaxOccurs())+
				";++" +
				"idx"+String.valueOf(props.getNoOccursSubscripts()) +
				") {");
				*/
			ClassFile.tab();
			if(props.getIndexesWorkSpace()==null)
				props.setIndexesWorkSpace(new ArrayList<String>());
			props.getIndexesWorkSpace().add("idx"+String.valueOf(props.getNoOccursSubscripts()));
		}
		if(!props.getRef()&&!props.getMod()||(props.getIsFiller()&&props.isGroupData())) {
			if(props.hasChildren()) {
				for(SymbolProperties child:props.getChildren()){
					child.setIndexesWorkSpace(props.getIndexesWorkSpace());
					printGroupSetBytesOccursLoop(child,value,modifiedIdx,relOff);
				}
			} 
		} 
		else
		if(!props.isUsedNativeJavaTypes())
			printGroupSetCobolBytesString(props,modifiedIdx,relOff);
		else {
			if(props.isOccurs()) {
				if(props.getPictureString()==null&&props.getIsFiller()) {
					for(SymbolProperties child:props.getChildren()){
						child.setIndexesWorkSpace(props.getIndexesWorkSpace());
						printGroupSetBytesOccursLoop(child,value,modifiedIdx,relOff);
					}
				} else {
					printGroupSetBytesString(props,value,modifiedIdx,relOff);
				}
			} else {
				printGroupSetBytesString(props,value,modifiedIdx,relOff);
			}
		}
		if(props.isOccurs()) {
			ClassFile.backTab();
			ClassFile.println("}");
			if(props.getIndexesWorkSpace()!=null)
				props.getIndexesWorkSpace().remove(props.getIndexesWorkSpace().size()-1);
		}
	}

	//Of the form...
	//setName1(bytes_.getDisplayLong(0,13,false,false,false));
	public void printGroupSetBytesString(SymbolProperties props,String value, int modifiedIdx,int relOff) {
		//if(props.getIsFiller())
			//printGroupSetCobolBytesString(props,relOff);
		//else {
			//String s=NameUtil.getJavaIndexes(props).trim();
			//if(s.length()>0) s=','+s;
			ClassFile.println(getLocalName(NameUtil.getJavaName(props,true)).replace("%0",getGroupGetterString(props,modifiedIdx,relOff))+";");
		//}
	}

	static String getLocalName(String fullName) {
		int i=((i=fullName.indexOf('('))<0)?fullName.length():++i;
		return fullName.substring(fullName.substring(0,i).lastIndexOf('.')+1);
	}

	public static boolean setCheckUseNativeJavaTypes(SymbolProperties props,boolean forceCobolBytes){
		if(props.isFloatingPoint()) {
			props.setUsedNativeJavaTypes(true);
			return true;
		}
		props.setUsedNativeJavaTypes(false);
	    if(props.getIsFiller()||props.getIsSuppressed()||props.isConsolidateParagraph()) return false;
		if(RESConfig.getInstance().getOptimizeAlgorithm()==0||forceCobolBytes) { 
			return false;
		}
		SymbolProperties props2=props;
		if(RESConfig.getInstance().getOptimizeAlgorithm()==1){
			boolean ret=true;
			while((props2=props2.getParent())!=null){
				ret&=(props2.isUsedNativeJavaTypes()||props2.isProgram());
			}
			if(!ret)
				return false;
		}
		
	  	//if(props.getParent().getType()!=PROGRAM&&
	  			//props.getParent().getLevelNumber()!=1&&
	  			//(props.getParent().getRef()||props.getParent().getMod()))
			//return false;
		props2=props;
		if(!(props2.getRedefines()!=null&&
				props2.getRedefines().getDataUsage()==Constants.INDEX&&
				props2.getRedefines().is01Group())) {
			while(props2!=null) {
					if(props2.getRedefines()!=null||(props2.getRedefinedBy()!=null&&props2.getRedefinedBy().size()>0)) 
						return false;
					if((props2.getLevelNumber()==1||props2.getLevelNumber()==77)&&props2.isAChildHasRedefines())
						return false;
		
					props2=props2.getParent();
			}
		}
		props.setUsedNativeJavaTypes(true);
		return true;
	}

	public void printJavaTypesAccessors(SymbolProperties props) {
		
		if(props.getIsSuppressed())	return;
		if(!props.isUsedNativeJavaTypes()&&!props.isFloatingPoint()) return;
		if(props.getIsFiller()) return;
		if(!(props.getRef()||props.getMod())) return;
		
		CobolSymbol sym=(CobolSymbol)props.getJavaType();
		String name1=(String) props.getJavaName1();
		String name2=(String) props.getJavaName2();
		
		String s="";
		modifier0="";
		modifier1="";
		getSubscripts="";
		modifier2="";
		modifier3="";
		modifier4="";
		setSubscripts="";
		
		if(props.isGroupData()||sym.type==CobolSymbol.GROUP) {
			props.setIsFormat(false);
			if(RESConfig.getInstance().getOptimizeAlgorithm()==2)
				printGroupAccessors(props, false, getGroupOccursIdx(props));
			else{
				props.setUsedNativeJavaTypes(false);
				printCobolByteAccessors(props);
			}
			return;
		}
		
		if(props.isOccurs()||props.isAParentInOccurs()) {
			ArrayList<SymbolProperties> a =props.getOccursParents();int i=0;
			//offsetCalc=String.valueOf(a.get(0).getOffset());
			if(a!=null&&a.size()>0) {
				for(Iterator<SymbolProperties> ite=a.iterator();ite.hasNext();)  {
					SymbolProperties curr=ite.next();++i;
					modifier0+="[]";
					modifier1+="["+String.valueOf(curr.getMaxOccursInt())+"]";
					getSubscripts+="int idx"+String.valueOf(i)+((ite.hasNext())?",":"");
					setSubscripts+=",int idx"+String.valueOf(i);
					modifier2+="[--idx"+String.valueOf(i)+"]";
					modifier3+="idx"+String.valueOf(i)+((ite.hasNext())?",":"");
					modifier4+=",idx"+String.valueOf(i);
				}
				
			}
		}
		
		if(FieldFormat.isPlainString(props.getPictureString()))
			props.setIsFormat(false);
		
		if(props.getIsFormat()) {
			printFieldFormat(props);
		}
	
    	switch(sym.type) {
    	case CobolSymbol.BIGDECIMAL:
    		s="private "+SymbolConstants.get(sym.type)+modifier0+" "+name1+" = ";
    		if(modifier1.length()<=0)
    			s+=SymbolConstants.get(sym.type)+".ZERO;";
    		else
	    		s+="new "+SymbolConstants.get(sym.type)+modifier1+";";    
    		break;
    	case CobolSymbol.BYTE:
    	case CobolSymbol.CHAR:
    	case CobolSymbol.SHORT:
    	case CobolSymbol.INTEGER:
    	case CobolSymbol.FLOAT:
    	case CobolSymbol.DOUBLE:
    	case CobolSymbol.LONG:
    		s="private "+SymbolConstants.get(sym.type)+modifier0+" "+name1;
    		if(modifier1.length()>0)
	    		s+="= new "+SymbolConstants.get(sym.type)+modifier1+";";
    		s+=";";
			break;
    	case CobolSymbol.STRING:
    		if(modifier1.length()<=0)
    			modifier1="()";
    		s="private "+SymbolConstants.get(sym.type)+modifier0+" "+name1+
    		"= new "+SymbolConstants.get(sym.type)+modifier1+";";
    		break;
    	case CobolSymbol.GROUP:
    	//TODO for group moves
    	default:
    		if(modifier1.length()<=0)
    			modifier1="()";
    		s="private "+SymbolConstants.get(sym.type)+modifier0+" "+name1+
    		"= new "+SymbolConstants.get(sym.type)+modifier1+";";
    	}
    	
		if(s.length()>0)
			ClassFile.println(s);
		
		ClassFile.doMethodScope("public " +SymbolConstants.get(sym.type)+
				" get"+name2+"("+getSubscripts+") {");
		ClassFile.tab();
		ClassFile.println("return " +name1+modifier2+";");
		ClassFile.backTab();
		ClassFile.endMethodScope();
		ClassFile.doMethodScope("public "+
				" void set"+name2+
				"(" +
				SymbolConstants.get(sym.type)+
				" val" +setSubscripts+") {");
		ClassFile.tab();
		if(sym.type==CobolSymbol.GROUP){
			ClassFile.println("return;");
		}
		else {
			s=getJavaTypeSetterString(props);
			ClassFile.println(name1+modifier2+"="+s.replace("%0","val")+";");
		}
		ClassFile.backTab();
		ClassFile.endMethodScope();
	
		if(props.isAsBytesAccessor()) {
			ClassFile.doMethodScope("public " +SymbolConstants.get(sym.type)+
					" get"+name2+"AsBytes("+getSubscripts+") {");
			ClassFile.tab();
			ClassFile.println("CobolBytes " + SymbolTable.TOSTRING_REF_STRING_ +
					"=new CobolBytes("+String.valueOf(props.getLength())+");");
			printGroupGetBytesOccursLoop(props,-1,props.getUnAdjustedOffset()) ;
			ClassFile.println("return " +SymbolTable.TOSTRING_REF_STRING_ +	".getBytes();");
			ClassFile.backTab();
			ClassFile.endMethodScope();
		}
		
		printJavaTypeClassConditionTail(props);
		return;
	}

	private void printFieldFormat(SymbolProperties props) {
		/*
		String otherFormat="";
		if(Main.getContext().getCurrencySign()!=null)
			otherFormat="," +Main.getContext().getCurrencySign();
		if(Main.getContext().isDecimalPointIsComma())
			otherFormat=",true";
		*/
		ClassFile.println("public " +
				((SymbolTable.getScope().isCurrentProgramTopLevel()||ClassFile.current.isData())?"static ":"") +
				"final FieldFormat "+props.getJavaName1()+"Fmt_ = new FieldFormat(\""+
				props.getJavaName1()+"\",\""+props.getJavaType().pic+"\"" +
						");");
	}

	private void printJavaTypeClassConditionTail(SymbolProperties props) {

		if(props.getJavaType().usage==CobolSymbol.DISPLAY||props.getJavaType().usage==CobolSymbol.PACKED_DECIMAL) {
			String isNumeric="";
			String isAlphabetic="";
			String isAlphabeticLower="";
			String isAlphabeticUpper="";
			
			switch(props.getJavaType().type) {
			case CobolSymbol.STRING:
				isNumeric="return " +props.getJavaName1()+".matches(\"[0-9+-]*\");";
				isAlphabetic="return "+props.getJavaName1()+".matches(\"[a-zA-Z ]*\");";
				isAlphabeticLower="return "+props.getJavaName1()+".matches(\"[a-z ]*\");";
				isAlphabeticUpper="return "+props.getJavaName1()+".matches(\"[A-Z ]*\");";
				break;
			case CobolSymbol.BIGDECIMAL:
			case CobolSymbol.SHORT:
			case CobolSymbol.INTEGER:
			case CobolSymbol.LONG:
			case CobolSymbol.FLOAT:
			case CobolSymbol.DOUBLE:
			case CobolSymbol.CHAR:
			case CobolSymbol.BYTE:
				isNumeric="return true;";
				isAlphabetic="return false";
				isAlphabeticLower="return false";
				isAlphabeticUpper="return false";
				break;
			case CobolSymbol.GROUP:
				isNumeric="return " +"new String(get"+props.getJavaName2()+"())"+".matches(\"[0-9+-]*\");";
				isAlphabetic="return "+"new String(get"+props.getJavaName2()+"())"+".matches(\"[a-zA-Z ]*\");";
				isAlphabeticLower="return "+"new String(get"+props.getJavaName2()+"())"+".matches(\"[a-z ]*\");";
				isAlphabeticUpper="return "+"new String(get"+props.getJavaName2()+"())"+".matches(\"[A-Z ]*\");";
				break;
			default:
				isNumeric="return false;";
				isAlphabetic="return false";
				isAlphabeticLower="return false";
				isAlphabeticUpper="return false";
				break;
			}
			
	    	if(props.isNumericTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"Numeric(" +getSubscripts+") {");
	    		ClassFile.tab();
				ClassFile.println(isNumeric);
				ClassFile.backTab();
	    		ClassFile.endMethodScope();
	    	}
	    	if(props.isAlphabeticTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"Alphabetic(" +getSubscripts+
						") {");
	    		ClassFile.tab();
				ClassFile.println(isAlphabetic);
				ClassFile.backTab();
	    		ClassFile.endMethodScope();
	    	}
	    	if(props.isAlphabeticLowerTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"AlphabeticLower(" +getSubscripts+
						") {");
	    		ClassFile.tab();
				ClassFile.println(isAlphabeticLower);
				ClassFile.backTab();
	    		ClassFile.endMethodScope();
	    	}
	       	if(props.isAlphabeticUpperTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"AlphabeticUpper(" +getSubscripts+
						") {");
	    		ClassFile.tab();
				ClassFile.println(isAlphabeticUpper);
				ClassFile.backTab();
	    		ClassFile.endMethodScope();
	    	}
		}
	}
	private String refString=null;
	private String offsetCalc=null;
	public void printCobolByteAccessors(SymbolProperties props) {
		if(props.getIsSuppressed())	return;
		if(props.getIsFiller()) return;
		if(props.isUsedNativeJavaTypes()||props.isFloatingPoint()) {printJavaTypesAccessors(props); return;}
		if(!(props.getRef()||props.getMod()||props.getIsFiller())) return ;
		if((props.getLength()==0||props.getJavaType()==null)) return ;
		
		String s;
		preparePrintCobolBytesAccessors(props);
		String name1=(String) props.getJavaName1();
		String name2=(String) props.getJavaName2();
		
		if(props.isGroupData()) {
			props.setIsFormat(false);
			if(!props.getIsFiller()) {				
			}
			return;
		} else {
			if (getOptionFormat(props).contains("COMP")) {
				varLst.add("tab()");
				//Write for Var
				varLst = printVar1(props,varLst);
				varLst.add("backTab()");
				//Write for constructor
				int idx2, idx1 =0;
				idx2=props.getLength();
				constructorsLst1 = printConstructor1(props, constructorsLst1, idx1, idx2);
				//Write for method get set
				setgetLst=printSetGetVar(props,setgetLst);
			} else {
				varLst.add("tab()");
				varLst.add("private " + getJavaType1(props) + " " + name1 + ";");			
				varLst.add("backTab()");
				if (!getValue(props).equals(""))
					constructorsLst1.add(getValue(props));

			}
			
		}
		
/* TuNT1 Delete
		if(props.getIsSuppressed())	return;
		if(props.getIsFiller()) return;
		if(props.isUsedNativeJavaTypes()||props.isFloatingPoint()) {printJavaTypesAccessors(props); return;}
		if(!(props.getRef()||props.getMod()||props.getIsFiller())) return ;
		if((props.getLength()==0||props.getJavaType()==null)) return ;
		
		
		String s;
		preparePrintCobolBytesAccessors(props);
		//String name1=(String) props.getJavaName1();
		String name2=(String) props.getJavaName2();
		
			if(props.isGroupData()) {
				props.setIsFormat(false);
				if(!props.getIsFiller()) {
					
		    		ClassFile.doMethodScope("public byte[]"+
		    				" get"+name2+"("+getSubscripts+") {");
		    		ClassFile.tab();	
		    		ClassFile.println("return "+getGetterString(props,-1,0)+";");
		    		ClassFile.backTab();
		    		ClassFile.endMethodScope();
		    		
		    		ClassFile.doMethodScope("public "+
		    				" void set"+name2+
		    				"(byte[] val" 
		    				 +setSubscripts+") {");
		    		ClassFile.tab();	
		    		ClassFile.println(getSetterString(props,-1,0)+";");
		    		ClassFile.backTab();
		    		ClassFile.endMethodScope();
		    		
		   			props.setIsFormat(Boolean.FALSE);
				}
				return;
			} else {

				if(FieldFormat.isPlainString(props.getPictureString())) {
					props.setIsFormat(false);
				}
				
				if(props.getIsFormat()) {
					printFieldFormat(props);
		TuNT1			*/
					/*
					String fmtLine=
						"public " +
						((SymbolTable.getScope().isCurrentProgramTopLevel()||ClassFile.current.isData())?"static ":"") +
						"final FieldFormat "+name1+"Fmt_ = new FieldFormat(\""+
						name1+"\",\""+props.getJavaType().pic+"\");";
					ClassFile.println(fmtLine);
					*/
		/* TuNT1
				}
				
				
				//Now getters
				ClassFile.doMethodScope("public "+SymbolConstants.get(props.getJavaType().type)+
					" get"+name2+"(" +getSubscripts+") {");
				ClassFile.tab();
				s=getGetterString(props,-1,0);
				//props.setFillerGetter(s);
				s="return "+s+";";
				ClassFile.println(s);
				ClassFile.backTab();
				ClassFile.endMethodScope();
				
				//Now setters
	    		ClassFile.doMethodScope("public "+
	    				" void set"+name2+
	    				"("+SymbolConstants.get(props.getJavaType().type)+" val" +
	    						setSubscripts+") {");
	    		ClassFile.tab();
	    		s=getSetterString(props,-1,0);
				//props.setFillerSetter(s);
				s+=";";
				ClassFile.println(s);
				ClassFile.backTab();
				ClassFile.endMethodScope();
				
			}
			if(props.isAsBytesAccessor()) {
				ClassFile.doMethodScope("public " +SymbolConstants.get(props.getJavaType().type)+
						" get"+name2+"AsBytes("+getSubscripts+") {");
				ClassFile.tab();
				ClassFile.println("CobolBytes " + SymbolTable.TOSTRING_REF_STRING_ +
						"=new CobolBytes("+String.valueOf(props.getLength())+");");
				printGroupGetBytesOccursLoop(props,-1,props.getUnAdjustedOffset()) ;
				ClassFile.println("return " +SymbolTable.TOSTRING_REF_STRING_ +	".getBytes();");
				ClassFile.backTab();
				ClassFile.endMethodScope();
			}
    	printCobolBytesClassConditionTail(props);
    TuNT1	*/
		return ;
	}

	private void preparePrintCobolBytesAccessors(SymbolProperties props) {
		if((props.getLevelNumber()==1||props.getLevelNumber()==77)&&
				props.getRedefines()!=null&&props.getPictureString()!=null&&
				props.getRedefines().getPictureString()==null){
			refString=props.getRedefines().getJavaName1();
		}
		else
			refString="super";
	
		getSubscripts="";
		setSubscripts="";
		
		Integer offset=props.getOffset();
		offsetCalc=offset.toString().trim();
		
		if(props.isOccurs()||props.isAParentInOccurs()) {
			ArrayList<SymbolProperties> a =props.getOccursParents();
			if(a!=null&&a.size()>0) {
				//			offsetCalc="";
				int i=0;
				for(Iterator<SymbolProperties> ite=a.iterator();ite.hasNext();)  {
					ite.next();++i;
					getSubscripts+="int idx"+String.valueOf(i)+((ite.hasNext())?",":"");
					setSubscripts+=",int idx"+String.valueOf(i);
	
					
				}
			}
			offsetCalc=getOccursString(props, -1, 0,offset);
		}
	}

	private void printCobolBytesClassConditionTail(SymbolProperties props) {
		
		if(props.getJavaType().usage==CobolSymbol.DISPLAY||props.getJavaType().usage==CobolSymbol.PACKED_DECIMAL) {
			if(props.isNumericTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"Numeric(" +getSubscripts+") {");
				ClassFile.tab();
				ClassFile.println("return " +
						refString +
						".isNumeric("+offsetCalc+","+String.valueOf(props.getLength())+");");
				ClassFile.backTab();
				ClassFile.endMethodScope();
			}
			if(props.isAlphabeticTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"Alphabetic(" +getSubscripts+
						") {");
				ClassFile.tab();
				ClassFile.println("return " +
						refString +
						".isAlphabetic("+offsetCalc+","+String.valueOf(props.getLength())+");");
				ClassFile.backTab();
				ClassFile.endMethodScope();
			}
			if(props.isAlphabeticLowerTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"AlphabeticLower(" +getSubscripts+
						") {");
				ClassFile.tab();
				ClassFile.println("return " +
						refString +
						".isAlphabeticLower("+offsetCalc+","+String.valueOf(props.getLength())+");");
				ClassFile.backTab();
				ClassFile.endMethodScope();
			}
			if(props.isAlphabeticUpperTested()){
				ClassFile.doMethodScope("public boolean is"+props.getJavaName2()+"AlphabeticUpper(" +getSubscripts+
						") {");
				ClassFile.tab();
				ClassFile.println("return " +
						refString +
						".isAlphabeticUpper("+offsetCalc+","+String.valueOf(props.getLength())+");");
				ClassFile.backTab();
				ClassFile.endMethodScope();
			}
		}
	}

	public String getGetterString(SymbolProperties props,int modifiedIdx,int relativeToUnAdjustedOffset)	{
		
		int offset=getOffset(props,relativeToUnAdjustedOffset);
		int len=props.getLength();
		if((len==0||props.getJavaType()==null)) return "";
		String refString;
		
		if((props.getLevelNumber()==1||props.getLevelNumber()==77)&&
				props.getRedefines()!=null&&!props.isGroupData()&&
				props.getRedefines().getPictureString()==null){
			refString=props.getRedefines().getJavaName1();
		}
		else {
			refString="super";
		}
		
		String offsetCalc;
		if(modifiedIdx==-2) {
			len=getMaxLen(props);
			offsetCalc=String.valueOf(offset);
		}
		else
		offsetCalc = getOccursString(props, modifiedIdx,
				relativeToUnAdjustedOffset, offset);
		
	
		if(props.isGroupData()) {
			return refString+".getBytes("+offsetCalc+","+String.valueOf(len)+")";
		}
		
		switch(props.getJavaType().type) {
		case CobolSymbol.BYTE:
		case CobolSymbol.CHAR: 
			return  getSingleByteGetterString(props, refString,	offsetCalc);
		case CobolSymbol.SHORT:
		case CobolSymbol.INTEGER: 
			return  getIntegerGetterString(props,  refString,offsetCalc);
		case CobolSymbol.LONG: 
			return  getLongGetterString(props,  refString, offsetCalc);
		case CobolSymbol.FLOAT:
		case CobolSymbol.DOUBLE:
		case CobolSymbol.BIGDECIMAL: 
				return  getBigDecimalGetterString(props,  refString, offsetCalc);
		case CobolSymbol.STRING:
				if(props.getDataUsage()==Constants.DISPLAY)
					return refString +".toString("+offsetCalc+","+len+")";
				else return "notAvailable";
		default:
		}
		return  "";
	}


	private int getOffset(SymbolProperties props,int relativeToUnAdjustedOffset) {
		if(relativeToUnAdjustedOffset>=0&&props.isAParentInOccurs()) {
			return props.getUnAdjustedOffset()-props.getOccursParents().get(props.getOccursParents().size()-1).getUnAdjustedOffset();
		}
		boolean test=(relativeToUnAdjustedOffset<=0);
		if(RESConfig.getInstance().getOptimizeAlgorithm()==2&&
				(props.isUsedNativeJavaTypes()||props.getIsFiller()))
			test=(relativeToUnAdjustedOffset<0);
		return (test)?props.getOffset():(props.getUnAdjustedOffset()-relativeToUnAdjustedOffset);
	}

	public String getSetterString(SymbolProperties props,int modifiedIdx, int relativeToUnAdjustedOffset)	{
		
		Integer offset=0;
		offset=getOffset( props,relativeToUnAdjustedOffset);
		Integer len=props.getLength();
		int usage=props.getDataUsage();
		CobolSymbol sym=(CobolSymbol) props.getJavaType();
		String s=null;
		if((len==0||sym==null)) return "";
		
		String refString;
		if((props.getLevelNumber()==1||props.getLevelNumber()==77)&&
				props.getRedefines()!=null&&props.getPictureString()!=null&&
				props.getRedefines().getPictureString()==null){
			refString=props.getRedefines().getJavaName1();
		}
		else {
			refString="super";
		}
		
		String offsetCalc;
		if(modifiedIdx==-2) {
			len=getMaxLen(props);
			offsetCalc=String.valueOf(offset);
		}
		else
			offsetCalc = getOccursString(props, modifiedIdx,
				relativeToUnAdjustedOffset, offset);
		
		if(props.isGroupData()) {
			return refString +
			".valueOf("+offsetCalc+","+
			len.toString()+",val"+
					",0);";
		}
		
		switch(sym.type) {
		case CobolSymbol.BYTE:
		case CobolSymbol.CHAR: {
			s = getSingleByteSetterString(props, refString, offsetCalc);
			break;
		}
		case CobolSymbol.SHORT:
		case CobolSymbol.INTEGER: {
			s = getIntegerSetterString(props, refString, offsetCalc);
			break;
		}
		case CobolSymbol.LONG: {
			s = getLongSetterString(props,  refString,offsetCalc);
			break;
		}
		case CobolSymbol.FLOAT:
		case CobolSymbol.DOUBLE:
		case CobolSymbol.BIGDECIMAL: {
			{
				s = getBigDecimalSetterString(props,refString, offsetCalc);
				break;
				}
		}
		case CobolSymbol.STRING:
			{
				switch(usage){
				case CobolSymbol.DISPLAY:
					s=refString +
							".valueOf("+offsetCalc+","+
							len.toString().trim()+"," +
							"val"+
							")"; break;
				case CobolSymbol.BINARY:
				case CobolSymbol.PACKED_DECIMAL:
				case CobolSymbol.COMPUTATIONAL1:
				default: s="notAvailable";
				}
				break;
				}
		}
		return s;
	}
	
	
	public String getPointerSetterString(SymbolProperties props)	{
		
		Integer len=props.getLength();
		int usage=props.getDataUsage();
		CobolSymbol sym=(CobolSymbol) props.getJavaType();
		String s=null;
		if((len==0||sym==null)) return "";
		
		String refString;
		if((props.getLevelNumber()==1||props.getLevelNumber()==77)&&
				props.getRedefines()!=null&&props.getPictureString()!=null&&
				props.getRedefines().getPictureString()==null){
			refString=props.getRedefines().getJavaName1();
		}
		else {
			refString="super";
		}
		
		if(props.isGroupData()) {
			return refString +
			".valueOf("+offsetCalc+","+
			len.toString()+",val"+
					",0);";
		}
		
		switch(sym.type) {
		case CobolSymbol.BYTE:
			s = refString+".setByte(val)";
			break;
		case CobolSymbol.CHAR: 
			s = refString+".setChar(val)";
			break;
		case CobolSymbol.SHORT:
		case CobolSymbol.INTEGER: 
			s = refString+".setInteger(val)";
			break;
		case CobolSymbol.LONG: 
			s = refString+".setLong(val)";
			break;
		case CobolSymbol.FLOAT:
		case CobolSymbol.DOUBLE:
		case CobolSymbol.BIGDECIMAL: 
			s = refString+".setBigDecimal(val)";
				break;
		case CobolSymbol.STRING:
				switch(usage){
				case CobolSymbol.DISPLAY:
					s = refString+".setString(val)";
					break;
				case CobolSymbol.BINARY:
				case CobolSymbol.PACKED_DECIMAL:
				case CobolSymbol.COMPUTATIONAL1:
				default: s="notAvailable";
				}
				break;
		}
		return s;
	}
	
	
	private int getMaxLen(SymbolProperties props)
	{
		int maxlen=0;
		if(props.isOccurs()||props.isAParentInOccurs()) {
			ArrayList<SymbolProperties> a =props.getOccursParents();
			if(a!=null&&a.size()>0) {
				for(ListIterator<SymbolProperties> ite=a.listIterator(a.size());ite.hasPrevious();)  {
					props=ite.previous();
					maxlen+=((props.getMaxOccursInt()>0)?props.getLength()*(props.getMaxOccursInt()):props.getLength());
				}
			}
		}
		else 
			maxlen=props.getLength();
		 return maxlen;	
	}
	private int getGroupOccursIdx(SymbolProperties props) {
		return props.getNoOccursSubscripts();
	}

	private String getGroupOccursSignature(SymbolProperties props) {
		String signature="";
		String subscripts="";
		int i=0;
		if(props.isOccurs()||props.isAParentInOccurs()) {
			ArrayList<SymbolProperties> a =props.getOccursParents();
			if(a!=null&&a.size()>0) {
				for(Iterator<SymbolProperties> ite=a.iterator();ite.hasNext();)  {
					ite.next();++i;
					signature+="int idx"+String.valueOf(i)+((ite.hasNext())?",":"");
					subscripts+="idx"+String.valueOf(i)+((ite.hasNext())?",":"");
				}
				
			}
			props.setIndexesWorkSpace(new ArrayList<String>());
			props.getIndexesWorkSpace().add(subscripts);
		}
		return signature;
	}
	
	public String getOccursString(SymbolProperties props,
			int modifiedIdx, int relativeToUnAdjustedOffset, Integer offset
			) {
		getSubscripts="";
		setSubscripts="";
		String offsetCalc=null;
		boolean hideIndexes = RESConfig.getInstance().getOptimizeAlgorithm()!=2;
		if(props.isOccurs()||props.isAParentInOccurs()) {
			ArrayList<SymbolProperties> a =props.getOccursParents();
			int i=1;
			if(a!=null&&a.size()>0) {
				for(Iterator<SymbolProperties> ite=a.iterator();ite.hasNext();i++)  {
					SymbolProperties curr=ite.next();
					if(i>modifiedIdx) {
						if(offsetCalc==null) {
							if(!hideIndexes)
								offsetCalc="super.__offset(";
							else
								offsetCalc="";
						}
						else
							if(!hideIndexes)
								offsetCalc+=',';
							else 
								offsetCalc+='+';
						getSubscripts+="int idx"+String.valueOf(i)+((ite.hasNext())?",":"");
						setSubscripts+=",int idx"+String.valueOf(i);
						if(hideIndexes) {
							if((offset=getOffset(curr,relativeToUnAdjustedOffset))==0) {
								if(curr.getLength()==1) {
									offsetCalc+="--"+"idx"+String.valueOf(i);
								} else {
									offsetCalc+=String.valueOf(curr.getLength())+"*--"+"idx"+String.valueOf(i);
								}
							} else {
								if(curr.getLength()==1) {
									offsetCalc+=String.valueOf(getOffset( curr,relativeToUnAdjustedOffset))+
									'+'+"--"+"idx"+String.valueOf(i);
								} else {
									offsetCalc+=String.valueOf(getOffset( curr,relativeToUnAdjustedOffset))+
									'+'+String.valueOf(curr.getLength())+"*--"+"idx"+String.valueOf(i);
								}
							}
						}
						else
						{
							offsetCalc+=String.valueOf(getOffset( curr,relativeToUnAdjustedOffset))+
							','+String.valueOf(curr.getLength())+','+"idx"+String.valueOf(i);
						}
	
					}
				}
				if(offsetCalc!=null) {
					if(!hideIndexes)
						offsetCalc+=')';
					if(props.isAParentInOccurs()&&(offset=getOffset(props,relativeToUnAdjustedOffset))>0)
						offsetCalc+="+"+String.valueOf(offset);
					return offsetCalc;
				}
			}
		}
		return offset.toString().trim();
	}
	

	private  String getBigDecimalSetterString(SymbolProperties props,String refString, String offsetCalc) {
		switch(props.getDataUsage()){
		case CobolSymbol.DISPLAY:
			return refString +".setDisplayBigDecimal("+offsetCalc+","+
			props.getLength()+",val,"+String.valueOf(props.getJavaType().maxFractionLength)+","+
			String.valueOf(props.isSigned())+","+String.valueOf(props.isSignLeading())+","+
			String.valueOf(props.isSignLeading())+")"; 
		case CobolSymbol.BINARY:
			return refString +".setBinaryBigDecimal("+offsetCalc+","+
					String.valueOf(props.getAdjustedLength())+",val,"+
					String.valueOf(props.getJavaType().maxFractionLength)+","+
					String.valueOf(props.isSigned())+")";
		case CobolSymbol.PACKED_DECIMAL:
			return refString +".setPackedDecimalBigDecimal("+offsetCalc+","+
					String.valueOf(props.getAdjustedLength())+",val,"+
					String.valueOf(props.getJavaType().maxFractionLength)+","+
			String.valueOf(props.isSigned())+")"; 
		case CobolSymbol.COMPUTATIONAL1:
		case CobolSymbol.COMPUTATIONAL2:
			return refString +".setBinaryBigDecimal("+offsetCalc+","+
			props.getAdjustedLength()+",new BigDecimal(val),"+String.valueOf(props.getJavaType().maxFractionLength);
		default:SymbolUtil.reportError();
		}
		return "";
	}

	private String getLongSetterString(SymbolProperties props,
			String refString, String offsetCalc) {
		switch(props.getDataUsage()){
		case CobolSymbol.DISPLAY:
			return refString +".setDisplayLong("+offsetCalc+","+
			props.getLength()+","+"val"+","+String.valueOf(props.isSigned())+","+
			String.valueOf(props.isSignLeading())+","+String.valueOf(props.isSignLeading())+")"; 
		case CobolSymbol.BINARY:
			return refString +".setBinaryLong("+offsetCalc+","+	String.valueOf(props.getAdjustedLength())+","
			+"val"+"," +String.valueOf(props.isSigned())+")"; 
		case CobolSymbol.PACKED_DECIMAL:
			return refString +".setPackedDecimalLong("+offsetCalc+","+
					String.valueOf(props.getAdjustedLength())+","+"val"+","+String.valueOf(props.isSigned())+")";
		case CobolSymbol.COMPUTATIONAL1:
			return refString +".setBinaryBigDecimal("+offsetCalc+","+props.getAdjustedLength()+","+
			"super.normalizeDecimal(val,"+props.getAdjustedLength()+","+props.getJavaType().maxFractionLength+")"+","+
			props.getJavaType().maxFractionLength+")"; 
		default:SymbolUtil.reportError();
		}
		return "";
	}

	private String getIntegerSetterString(SymbolProperties props,String refString, String offsetCalc) {
		switch(props.getDataUsage()){
		case CobolSymbol.DISPLAY:
			return refString +".setDisplayInt("+offsetCalc+","+	String.valueOf(props.getLength())+",val"+
					","+String.valueOf(props.isSigned())+","+String.valueOf(props.isSignLeading())+","+
					String.valueOf(props.isSignLeading())+")"; 
		case CobolSymbol.BINARY:
			return refString +".setBinaryInt("+offsetCalc+","+
					String.valueOf(props.getAdjustedLength())+",val"+"," +String.valueOf(props.isSigned())+")";
		case CobolSymbol.PACKED_DECIMAL:
			return refString +".setPackedDecimalInt("+offsetCalc+","+String.valueOf(props.getAdjustedLength())
					+",val"+"," +String.valueOf(props.isSigned())+")"; 
		case CobolSymbol.COMPUTATIONAL1:
			return refString +".setBinaryBigDecimal("+offsetCalc+","+String.valueOf(props.getAdjustedLength())+","+
			"super.normalizeDecimal(val,"+props.getAdjustedLength()+","+
			String.valueOf(props.getJavaType().maxFractionLength)+")"+","+
			props.getJavaType().maxFractionLength+")"; 
		default:SymbolUtil.reportError();
		}
		return "";
	}


	private String getBigDecimalGetterString(SymbolProperties props,String refString, String offsetCalc) {
		switch(props.getDataUsage()){
		case CobolSymbol.DISPLAY:
			return refString +".getDisplayBigDecimal("+offsetCalc+","+
			String.valueOf(props.getLength())+","+String.valueOf(props.getJavaType().maxFractionLength)+","+
			String.valueOf(props.isSigned())+","+String.valueOf(props.isSignLeading())+","+
			String.valueOf(props.isSignLeading())+")"; 
		case CobolSymbol.BINARY:
			return refString +".getBinaryBigDecimal("+offsetCalc+","+String.valueOf(props.getAdjustedLength())+
				","+new Integer(props.getJavaType().maxFractionLength).toString().trim()+")"; 
		case CobolSymbol.PACKED_DECIMAL:
			return 	refString +".getPackedDecimalBigDecimal("+offsetCalc+","+
				String.valueOf(props.getAdjustedLength())+","+
				String.valueOf(props.getJavaType().maxFractionLength)+")"; 
		case CobolSymbol.COMPUTATIONAL1:
		case CobolSymbol.COMPUTATIONAL2:
			return refString + ".getBinaryBigDecimal("+offsetCalc+","+String.valueOf(props.getLength())+","+
			String.valueOf(props.getJavaType().maxFractionLength)+").floatValue()"; 
		default:SymbolUtil.reportError();
		}
		return "";
	}

	private String getLongGetterString(SymbolProperties props,String refString, String offsetCalc) {
		switch(props.getDataUsage()){
		case CobolSymbol.DISPLAY:
			return	refString+".getDisplayLong("+offsetCalc+","+String.valueOf(props.getLength())+","+
			String.valueOf(props.isSigned())+","+String.valueOf(props.isSignLeading())+","+
			String.valueOf(props.isSignLeading())+")"; 
		case CobolSymbol.BINARY:
			return refString+".getBinaryLong("+offsetCalc+","+String.valueOf(props.getAdjustedLength())+")";
		case CobolSymbol.PACKED_DECIMAL:
			return refString+".getPackedDecimalLong("+offsetCalc+","+
			String.valueOf(props.getAdjustedLength())+")"; 
		case CobolSymbol.COMPUTATIONAL1:
			return refString +".getBinaryBigDecimal("+offsetCalc+","+String.valueOf(props.getLength())+","
			+new Integer(props.getJavaType().maxFractionLength).toString().trim()+")"; 
		default:SymbolUtil.reportError();
		}
		return "";
	}

	private String getSingleByteGetterString(SymbolProperties props,String refString, String offsetCalc) {
		if(props.getJavaType().type==CobolSymbol.CHAR) {
			return refString +".getChar("+offsetCalc+")";
		} else
			return refString +".getByte("+offsetCalc+")";
	}
	
	private String getSingleByteSetterString(SymbolProperties props,String refString, String offsetCalc) {
		if(props.getJavaType().type==CobolSymbol.CHAR) {
			return refString +".setChar("+offsetCalc+",val)";
		} else
			return refString +".setByte("+offsetCalc+",val)";
	}
	
	private String getIntegerGetterString(SymbolProperties props,String refString, String offsetCalc) {
		switch(props.getDataUsage()){
		case CobolSymbol.DISPLAY:
			return refString+".getDisplayInt("+offsetCalc+","+String.valueOf(props.getLength())+","+
			String.valueOf(props.isSigned())+","+String.valueOf(props.isSignLeading())+","+
			String.valueOf(props.isSignLeading())+")";
		case CobolSymbol.BINARY:
			return refString+".getBinaryInt("+offsetCalc+","+String.valueOf(props.getAdjustedLength())+")"; 
		case CobolSymbol.PACKED_DECIMAL:
			return refString+".getPackedDecimalInt("+offsetCalc+","+String.valueOf(props.getAdjustedLength())+")"; 
		case CobolSymbol.COMPUTATIONAL1:
			return refString +".getBinaryBigDecimal("+offsetCalc+","+String.valueOf(props.getLength())+","+
			new Integer(props.getJavaType().maxFractionLength).toString().trim()+")"; 
		default:SymbolUtil.reportError();
		return "";
		}
	}
	
	private String getJavaTypeSetterString(SymbolProperties props)
	{
		CobolSymbol sym=(CobolSymbol) props.getJavaType();
		if(sym==null) return "%0";
			
		switch(sym.type) {
		case CobolSymbol.SHORT:
		case CobolSymbol.INTEGER: 
			return "(int) super.normalizeBinary(%0,"+String.valueOf(props.getAdjustedLength())+
				","+String.valueOf(props.isSigned())+")"; 
		case CobolSymbol.LONG: 
				return "super.normalizeBinary(%0,"+String.valueOf(props.getAdjustedLength())+
						","+String.valueOf(props.isSigned())+")";
		case CobolSymbol.BIGDECIMAL:
				return "super.normalizeDecimal(%0,"+String.valueOf(props.getAdjustedLength())+","+
				String.valueOf(props.getJavaType().maxFractionLength)+","+String.valueOf(props.getJavaType().isSigned)+")";
		case CobolSymbol.STRING: 
			return "super.normalizeString(%0,"+String.valueOf(props.getLength())+"," +
			String.valueOf(props.isJustifiedRight())+
					")"; 
		}
		return "%0";
	}

	public String getLocalFillerSetter(SymbolProperties props, String fillerVal) {
		return getSetterString(props,-1,-1).replace(",val", ","+fillerVal);
	}

	public String getLocalFillerGetter(SymbolProperties props) {
		return getGetterString(props,-1,-1);
	}

	public String getFillerSetter(SymbolProperties props, String fillerVal) {
		String level01Name=getLevel01Name(props);
		return getLocalFillerSetter(props,fillerVal).replace("super.", ((level01Name==null)?"":(level01Name+'.')));
	}

	public String getFillerGetter(SymbolProperties props) {
		String level01Name=getLevel01Name(props);
		return getGetterString(props,-1,-1).
			replace("super.", ((level01Name==null)?"":(level01Name+'.')));
	}
	
	private String getLevel01Name(SymbolProperties props) {
		while((props=props.getParent())!=null&&!props.is01Group())
			;
		if(props==null) return null;
		else return props.getJavaName1();
	}
	
	private void printClass88s(SymbolProperties props) {
		ArrayList<SymbolProperties> a = (ArrayList<SymbolProperties>)props.getChildren();
		if(a!=null) {
			//boolean isNativeJavaType=RESConfig.getInstance().isUseNativeJavaTypes();
			for(SymbolProperties child : a){
				if(child.getLevelNumber()!=88) break;
				Cobol2Java.getInstance().doOne88(child);
			}
		}
	}

	// QuyTV delete declare model variable
	public void printClassAccessors(SymbolProperties props, String name1, String name2) {
		if (name1.equals("sqlca")) {
			// ClassFile.println("public "+name2+" "+name1+" = new "+name2+"();"); 
		} else if (props.getRedefines() == null)
			/* ClassFile.println("private "+name2+" "+name1+" = new "+name2+"();"); */
			Console.print("");
		else {
			if (props.getRedefines().is01Group()) {
				/*
				 * ClassFile.println("private "+name2+" "+name1+" = new "+name2+"(" +
				 * props.getRedefines().getJavaName1()+ ");");
				 */
			} else {
				String exprString = "this," + String.valueOf(props.getRedefines().getOffset()) + ","
						+ String.valueOf(props.getLength());
				/*
				 * ClassFile.println("private "+name2+" "+name1+" = new "+name2+"(" +exprString+
				 * ");");
				 */
			}
		}
		if (!RESConfig.getInstance().isUsePointers()) {
			/*
			 * ClassFile.println("public byte[] get"+name2+"() {");
			 * ClassFile.println("\treturn " +name1+".getBytes();"); ClassFile.println("}");
			 * ClassFile.println("public void set"+name2+"(byte[] val) {");
			 * ClassFile.println("\t" +name1+".valueOf(val);"); ClassFile.println("}");
			 */
		} else {
			/*
			 * ClassFile.println("public BytesField get"+name2+"() {");
			 * ClassFile.println("\treturn " +name1+"."+name1+"Fld_;");
			 * ClassFile.println("}");
			 * ClassFile.println("public void set"+name2+"(byte[] val) {");
			 * ClassFile.println("\t" +name1+".valueOf(val);"); ClassFile.println("}");
			 */
		}
		printClassConditionTail(props);
	}

	private void printClassConditionTail(SymbolProperties props) {
		if(props.isUsedNativeJavaTypes()) {
			printJavaTypeClassConditionTail(props);
		} else {
			preparePrintCobolBytesAccessors(props);
			printCobolBytesClassConditionTail(props);
		}
	}
	
	private void printStringAccessors(SymbolProperties props) {
		ClassFile.doMethodScope("public String toString() {");
		ClassFile.tab();
		if(props.hasChildren()) {
		                    ClassFile.println("return new String(getBytes());");
		}
		else
			ClassFile.println("return \"\";");
		ClassFile.backTab();
		ClassFile.endMethodScope();
		ClassFile.doMethodScope("public void valueOf(String val) {//Bytes Vs. Chars");
		ClassFile.tab();
		if(props.hasChildren()) {
		                    ClassFile.println("valueOf(val.getBytes());");
		}
		else
			ClassFile.println("return;");
		ClassFile.backTab();
		ClassFile.endMethodScope();
	}

	private void printConstructors(SymbolProperties props, Integer adjLen) {
// DaoNH MOD Start: Update constructor when group all data into one Model
//		ClassFile.doMethodScope("public "+props.getJavaName2()+"() {");
		ClassFile.println("");
		ClassFile.doMethodScope("public " + modelName + "() {");
// DaoNH MOD End: Update constructor when group all data into one Model
		ClassFile.tab();
// DaoNH ADD Start: Update constructor when group all data into one Model
		SymbolProperties tmpSymProp = props;
		while(!tmpSymProp.getDataName().equals("RETURN-CODE")) {
			ClassFile.println("");
			ClassFile.println(tmpSymProp.getJavaName1() + " = new " + tmpSymProp.getJavaName2() + "();");
			tmpSymProp = tmpSymProp.getSibling();
		};
// DaoNH ADD End: Update constructor when group all data into one Model

// TODO: DAONH DEL Start: check later with Java type
//		if(//(!props.isUsedNativeJavaTypes()||RESConfig.getInstance().getOptimizeAlgorithm()==2)
//				//&&
//				adjLen>0){
//			ClassFile.println("super(new CobolBytes("+String.valueOf(adjLen)+"));");
//		} else {
//			ClassFile.println("super();");
//		}
// TODO: DAONH DEL End: check later with Java type
		ClassFile.backTab();
		ClassFile.endMethodScope();
// TODO: DAONH DEL Start: check later with Java type
//		ClassFile.doMethodScope("public "+props.getJavaName2()+"(CobolBytes b) {//For redefines");
//		ClassFile.println("\tsuper(b); }");
//		ClassFile.doMethodScope("public "+props.getJavaName2()+"(CobolBytes b,int off,int len) {//For redefines");
//		ClassFile.println("\tsuper(b,off,len); }");
// TODO: DAONH DEL End: check later with Java type
	}

// DAONH ADD Start: Group all data into one Model with inner classes	
	/**
	 * Print out Model attributes
	 * 
	 * @param props SymbolProperties
	 * @param object Object
	 */
	private void printModelAttrs(SymbolProperties props,boolean programScope) {
		SymbolProperties tmpSymProp = props;
		varLst.add("tab()");
		constructorsLst.add("tab()");
		if (!constructorsLst1.isEmpty()) {
			constructorsLst.addAll(constructorsLst1);
			constructorsLst1.clear();
		}
		while (tmpSymProp != null && !tmpSymProp.getDataName().equals("RETURN-CODE") && !bFlag) {
			if (!emittedModelAttrs.add(tmpSymProp.getDataName())) {
				tmpSymProp = tmpSymProp.getSibling();
				continue;
			}
			boolean isCopy = false;
			if (copy_List != null) {
				for(String[] strCopy : copy_List)
				{
					if (isCommonModelCopybook(strCopy[1]) &&
							(tmpSymProp.getLevelNumber() ==1 && tmpSymProp.getDataName().contains(strCopy[1]) ||
							tmpSymProp.getDataName().equals(strCopy[0])) )
					{
						//System.out.println(tmpSymProp.getDataName());
						//String modelName =  ConvertCopyName(strCopy[1]).trim();
						String modelName;
						if (tmpSymProp.getDataName().contains(strCopy[1]))
							modelName =ConvertCopyName(tmpSymProp.getDataName()).trim();
						else
							modelName =  ConvertCopyName(strCopy[1]).trim();
							
						if (!modelNameLst.contains(modelName)) {						
						   headerLst.add("import jp.co.alsm.common.model." + modelName + ";");
						   modelNameLst.add(modelName);
						}
						varLst.add("private " + modelName + " " + tmpSymProp.getJavaName1() + ";");
						constructorsLst.add(tmpSymProp.getJavaName1() + " = new " + modelName    + "();");
						//tmpSymProp = tmpSymProp.getSibling();
						isCopy = true;
						//break;
					}
				}
				if (!isCopy && tmpSymProp.getRedefinedBy() == null) {
					// Truong hop bien Level 1 la bien duoc gan kieu
					if (tmpSymProp.getPictureString() != null) {
						if (tmpSymProp.getLevelNumber() == 1) {
							varLst.add("private " + getJavaType1(tmpSymProp) + " " + tmpSymProp.getJavaName1() + ";");
						}
					} else {
						// Truong hop la bien level1 va la bien groups
						if (tmpSymProp.getLevelNumber() == 1) {
							varLst.add("private " + tmpSymProp.getJavaName2() + " " + tmpSymProp.getJavaName1() + ";");
							constructorsLst.add(tmpSymProp.getJavaName1() + " = new " + tmpSymProp.getJavaName2() + "();");
						}
						printInnerClass(tmpSymProp);
						// tmpSymProp = tmpSymProp.getSibling();
					}
				}

			} else {
				// Truong hop bien Level 1 la bien duoc gan kieu
				if (tmpSymProp.getPictureString() != null) {
					varLst.add("private " + getJavaType1(tmpSymProp) + " " + tmpSymProp.getJavaName1() + ";");
				} else {
					// Truong hop la bien level1 va la bien groups
					varLst.add("private " + tmpSymProp.getJavaName2() + " " + tmpSymProp.getJavaName1() + ";");
					constructorsLst.add(tmpSymProp.getJavaName1() + " = new " + tmpSymProp.getJavaName2() + "();");
					printInnerClass(tmpSymProp);
					// tmpSymProp = tmpSymProp.getSibling();
				}
			}
			tmpSymProp = tmpSymProp.getSibling();
		}
		bFlag = true;
		//20210901 Update S
		varLst.add("backTab()");
		//constructorsLst.add("backTab()");
		//doWriteModel(props,programScope);
		if (tmpSymProp != null && tmpSymProp.getDataName().equals("RETURN-CODE"))
		{
		   doWriteModel(props,programScope);
		   doClassEnding(props,!programScope);
		}
		//20210901 Update E
	}

	/**
	 * Print inner class
	 * 
	 * @param props SymbolProperties
	 */
	private ArrayList<String> recordLst;
	private ArrayList<String> recordContr;
	private ArrayList<String> methodLst;
	int fillerIdx=0;
	int idx =0;
	int idx1;
	int idx2;
	private void printInnerClass(SymbolProperties props) {	
		/* Xu ly cho level1
		 * Tuong uong 1 bien level 1 se create 
		 * 1. Class model
		 * 2. Neu bien child with getPictureString() = null 
		 *    >>>Xu ly get, set, write, toString
		 * 3. Neu bien child with getPictureString() != null 
		 * >>>Xu ly write, toString
		 */   
		//setgetLst.add("");
		//setgetLst.add("@Getter");
		//setgetLst.add("@Setter");
		setgetLst.add("public class " + props.getJavaName2() + "{");
		setgetLst.add("tab()");
		setgetLst.add("");
		// Loop and print inner class attributes
		ArrayList<SymbolProperties> childSP = null;

		childSP = props.getChildren();
		if (childSP != null && !childSP.isEmpty()) {
			ResetVar();
			recordContr.add("public " +props.getJavaName2() +"() {");
			recordContr.add("tab()");
			for (SymbolProperties child : childSP) {
				if (child.isOccurs()) {
					idx = 0;
					occursShori(child);
					continue;
				}
				/* Update 20210831 S */
				idx1= child.getOffset();
				// if(child.getRedefines()!=null)
				if (child.getRedefines() == null) {
					// continue;
					// }
					if (!child.getJavaName1().toLowerCase().contains("filler")) {
						// Write for var
						recordLst = printVar(child, recordLst);
						// Write for constructor
						/* Update 20210831 S */
						if (child.getPictureString()!=null && child.getPictureString().contains("X(")) {
							idx2 = idx1 + child.getJavaType().maxStringLength;
						} else {
							idx2 = idx1 + child.getLength();
						}
						/* Update 20210831 E */
						recordContr = printConstructor(child, recordContr, idx1, idx2);
						// Write for method set get
						methodLst = printSetGetVar(child, methodLst, false);
					} else {
						/* Update 20210831 S */
						if (child.getPictureString().contains("X(")) {
							idx2 = idx1 + child.getJavaType().maxStringLength;
						} else {
							idx2 = idx1 + child.getLength();
						}
						/* Update 20210831 E */

					}
				/* 20210831 Add S */
				}
				else
				{
					// For Redefine 
					SymbolProperties reference = child.getRedefines();
					if (reference.getPictureString()!=null && reference.getPictureString().contains("X(")) {
						idx1 = idx2 - reference.getJavaType().maxStringLength;
					} else {
						idx1 = idx2 - reference.getLength();
					}
				}
				/* 20210831 Add E */
				if (child.hasChildren()) {
					printNestedChild(child);
				}
				idx1 = idx2;
			}
			// The buffer holds the whole group: its declared length, or the end of its last child
			// when OCCURS items push the layout past that. "Length" was never declared anywhere.
			int recordLength = Math.max(Math.max(props.getAdjustedLength(), props.getLength()), idx2);
			recordLst.add("private Record record = new Record(new byte[" + recordLength + "]);");
			/* 20280831 Add S */
			recordLst.add("public byte[] getDatas() {");
			recordLst.add("tab()");
			recordLst.add("return record.getDatas();");
			recordLst.add("backTab()");
			recordLst.add("}");
			recordLst.add("");
			/* 20280831 Add E */
			recordContr.add("backTab()");
			recordContr.add("}");
			if (recordLst != null) {
				for (String item : recordLst) {
					setgetLst.add(item);
				}
			}
			if (recordContr != null) {
				for (String item : recordContr) {
					setgetLst.add(item);
				}
			}
			if (methodLst != null) {
				for (String item : methodLst) {
					setgetLst.add(item);
				}
			}
			// Print out PICTURE total length of current Object
			setgetLst.add("public static final int Length = " + props.getAdjustedLength() + ";");
			setgetLst.add("");
			printWritebackTabeToString();
			setgetLst.add("");
			setgetLst.add("backTab()");
			setgetLst.add("}");
		}
				/*
				//Get all bien level 2
				ResetVar();
				idx2=idx1+child.getAdjustedLength();
				writeLst.add("	set" + child.getJavaName2() + "(value.substring(" + idx1 + "," + idx2 + "));");
				toStringLst.add("get" + child.getJavaName2() + "()");
				if (child != null && child.getChildren() != null) {
					// Todo here
					printNestedChild(child);
				} else {
					// Todo here
					printNode(props, child);
				}
				idx1=idx2;
			}
			//print write, 
			if (writeLst != null) {
				setgetLst.add("public void write(String value) {");
				for (String item : writeLst) {
					setgetLst.add(item);
				}
				setgetLst.add("}");
			}
			// Print toString
			if (toStringLst != null) {
				String tmp = "";
				setgetLst.add("@Override");
				setgetLst.add("public String toString() {");
				tmp += "	return ";
				for (String item : toStringLst) {
					tmp += item + " + ";
				}
				tmp= tmp.substring(0,tmp.length()-2) + ";";
				setgetLst.add(tmp);
				setgetLst.add("}");
			}
		}
		else
		{
			// System.out.println("Level 1 Hava Value");
			printNode(props, props);
		}
		setgetLst.add("backTab()");
		setgetLst.add("}");
		setgetLst.add("");
		*/
	}

	// Bien
	private void printNestedChild(SymbolProperties props) {
		int idx11 = idx1;
		int idx22 = 0;
		for (SymbolProperties child : props.getChildren()) {
			if (child.isOccurs()) {
				idx = idx11;
				occursShori(child);
				continue;
			}
			/* Update 20210831 S */
			idx11= child.getOffset();
			// if(child.getRedefines()!=null)
			if (child.getRedefines() == null) {
				// continue;
				// }
				if (!child.getJavaName1().toLowerCase().contains("filler")) {
					// Write for Var
					recordLst = printVar(child, recordLst);
					// Write for constructor
					/* Update 20210831 S */
					//idx22 = idx11 + child.getLength();
					if (child.getPictureString()!= null  && child.getPictureString().contains("X(")) {
						idx22 = idx11 + child.getJavaType().maxStringLength;
					} else {
						idx22 = idx11 + child.getLength();
					}
					/* Update 20210831 E */
					recordContr = printConstructor(child, recordContr, idx11, idx22);
					// Write for method get set
					methodLst = printSetGetVar(child, methodLst, false);
				} else {
					/* Update 20210831 S */
					//idx22 = idx11 + child.getLength();
					if (child.getPictureString().contains("X(")) {
						idx22 = idx11 + child.getJavaType().maxStringLength;
					} else {
						idx22 = idx11 + child.getLength();
					}
					/* Update 20210831 E */
				}
			/* 20210831 Add S */
			} else {
				// For Redefine 
				SymbolProperties reference = child.getRedefines();
				if (reference.getPictureString()!=null && reference.getPictureString().contains("X(")) {
					idx11 = idx22 - reference.getJavaType().maxStringLength;
				} else {
					idx11 = idx22 - reference.getLength();
				}
			}
			/* 20210831 Add E */
			if (child.hasChildren()) {
				printNestedChild(child);
			}
			idx11 =idx22;
			idx1=idx22;
		}
		//SymbolProperties parentNode= props.getParent();
		/*
		if (props.getParent() != null && props.getParent().getJavaName2() != strParent) {
			if (props.hasChildren() && props.getChildren().size()<2) {//O day dung voi LPH-HEADER but khong dung voi INPUT-AREA
				if (props.getChildren().get(0).getPictureString() != null) {
					printSetGet(props);
				}
			}else {
				if (props.getLevelNumber()<3)
				{
					printSetGetGroup(props);
				}
			}
			strParent=props.getJavaName2();
		}
		for (SymbolProperties child : props.getChildren()) {
			if (!child.hasChildren()) {
					printNode(props, child);
			} else {
				// Todo Code here
				if (child.getPictureString() == null) {
					if (props.getChildren()!=null && props.getChildren().size()<2)
					{
						printSetGet(props, child);
					}
				} 
				printSetGetGroup(child);
				printNestedChild(child);
			}
		}
		*/
	}
	private void printOccursNestedChild(SymbolProperties props) {
		for (SymbolProperties child : props.getChildren()) {
			if(child.isOccurs()) continue;
			methodLst=printSetGetVar(child,methodLst,true);
			if (child.hasChildren()) {
				printOccursNestedChild(child);
			}
		}

	}
	private String getJavaType1(SymbolProperties props)
	{
		CobolSymbol sym=(CobolSymbol) props.getJavaType();
		String s=SymbolConstants.get(sym.type);
		// A PIC X(1) item is kept as text: the generated code assigns it string literals
		// ("N", "Y") and compares it with Compare.eq(String, String), never with a char.
		if(s.equals("char")) s="String";
    	return s;
	}

	private static boolean isNumericJavaType(String type) {
		return type.equals("byte")||type.equals("short")||type.equals("int")||type.equals("long")
				||type.equals("float")||type.equals("double")||type.equals("BigDecimal");
	}

	/**
	 * Getter and setters for one item of a group, over its Record. A numeric item reads as its
	 * Java type and accepts any numeric value; a text item accepts numbers too (COBOL lets a
	 * numeric literal be moved to an alphanumeric item), written left-justified as digits.
	 */
	private void addTypedAccessors(ArrayList<String> out, String javaName2, String rec, String type) {
		String set=rec+".set(value, "+rec+".getOffset(), "+rec+".getEnd());";
		out.add("");
		if(isNumericJavaType(type)) {
			String result;
			if(type.equals("int")) result=rec+".toInt()";
			else if(type.equals("long")) result=rec+".toLong()";
			else if(type.equals("BigDecimal")) result=rec+".toBigDecimal()";
			else if(type.equals("double")) result=rec+".toBigDecimal().doubleValue()";
			else if(type.equals("float")) result=rec+".toBigDecimal().floatValue()";
			else result="("+type+") "+rec+".toInt()";
			out.add("public "+type+" get"+javaName2+"() {");
			out.add("tab()");
			out.add("return "+result+";");
			out.add("backTab()");
			out.add("}");
			for(String t : new String[]{"int","long","BigDecimal","String"}) {
				out.add("");
				out.add("public void set"+javaName2+"("+t+" value) {");
				out.add("tab()");
				out.add(set);
				out.add("backTab()");
				out.add("}");
			}
			out.add("");
			out.add("public void set"+javaName2+"(double value) {");
			out.add("tab()");
			out.add(rec+".set(BigDecimal.valueOf(value), "+rec+".getOffset(), "+rec+".getEnd());");
			out.add("backTab()");
			out.add("}");
		} else {
			out.add("public String get"+javaName2+"() {");
			out.add("tab()");
			out.add("return "+rec+".toString();");
			out.add("backTab()");
			out.add("}");
			out.add("");
			out.add("public void set"+javaName2+"(String value) {");
			out.add("tab()");
			out.add(set);
			out.add("backTab()");
			out.add("}");
			for(String t : new String[]{"int","long","double"}) {
				out.add("");
				out.add("public void set"+javaName2+"("+t+" value) {");
				out.add("tab()");
				out.add("set"+javaName2+"(String.valueOf(value));");
				out.add("backTab()");
				out.add("}");
			}
			out.add("");
			out.add("public void set"+javaName2+"(BigDecimal value) {");
			out.add("tab()");
			out.add("set"+javaName2+"(value.toPlainString());");
			out.add("backTab()");
			out.add("}");
		}
	}
	private void ResetVar() {
		recordLst =new ArrayList<String>();
		recordContr =new ArrayList<String>();
		methodLst=new ArrayList<String>();
		fillerIdx=0;
		idx =0;
		idx1 = 0;
		idx2 = 0;
	//	strParent="";
	//	idxFiller=1;
	}
	private void occursShori(SymbolProperties child) {
		// Write for var
		//idx =0;
		recordLst = printVar(child, recordLst);
		recordContr=printConstructor(child,recordContr,idx,child.getLength()*child.getMaxOccursInt()+idx);
		methodLst=printSetGetVar(child,methodLst,false);
		if (child.hasChildren()) {
			printOccursNestedChild(child);
		}
	}
	private String getVarFormat(SymbolProperties child) {
		String strRet;
		if (child.getPictureString()!=null)
		{
			strRet=".withFormat(\"" + child.getPictureString() + "\")";
		}else if (child.isOccurs() && child.getMajorIndex()!=null){
			strRet=".withOption(\"OCCURS " + child.getMaxOccurs() + " INDEXED " + 
						child.getMajorIndex().getDataName()+ "\")";
		}else {
			strRet="";
		}
		return strRet;
	}
	private String getValue(SymbolProperties child) {
		String strRet="";
		ArrayList<SymbolProperties.CoupleValue> arrayValues = child.getValues();
		if (arrayValues != null) {
			String strValues = (String) arrayValues.get(0).value1.toString();
			// A COBOL literal may be quoted with ' or "; the Java string literal adds its own quotes.
			if (strValues.length() >= 2 && (strValues.startsWith("\"") && strValues.endsWith("\"")
					|| strValues.startsWith("'") && strValues.endsWith("'")))
				strValues = strValues.substring(1, strValues.length() - 1);
			if (isNumeric(strValues))
				strRet = "set" + child.getJavaName2() + "(" + strValues + ");";
			else
				// A backslash or double quote inside the literal must be escaped in the Java string.
				strRet = "set" + child.getJavaName2() + "(\"" + strValues.replace("\\", "\\\\").replace("\"", "\\\"") + "\");";
		}
		return strRet.replace("'", "");
	}
	private ArrayList<String> printVar(SymbolProperties child, ArrayList<String> arrayLst){
		
		arrayLst.add("private Record " + child.getJavaName1() + ";");
		return arrayLst;
	}
	private ArrayList<String> printVar1(SymbolProperties child, ArrayList<String> arrayLst){
		
		arrayLst.add("private Record " + child.getJavaName1() + ";");
		arrayLst.add("private Record record" + child.getJavaName2() + ";");
		return arrayLst;
	}
	private ArrayList<String> printConstructor(SymbolProperties child, ArrayList<String> arrayLst, int idx1,int idx2){
		arrayLst.add(child.getJavaName1() + " = record.subSequence(" +
	              idx1 + "," + idx2 + ")"+getVarFormat(child)+getOptionFormat(child)+";");
	if (!getValue(child).equals(""))
		arrayLst.add(getValue(child));
		return arrayLst;
	}
	private ArrayList<String> printConstructor1(SymbolProperties child, ArrayList<String> arrayLst, int idx1,int idx2){
		arrayLst.add("record" + child.getJavaName2() + " =  new Record(new byte[" + (idx2)  + "]);");
		idx2--;
		arrayLst.add(child.getJavaName1() + " = record" + child.getJavaName2() + ".subSequence(" +
	              idx1 + "," + idx2 + ")"+getVarFormat(child)+getOptionFormat(child)+";");
	if (!getValue(child).equals(""))
		arrayLst.add(getValue(child));
		return arrayLst;
	}
	private ArrayList<String> printSetGetVar(SymbolProperties child, ArrayList<String> arrayLst,boolean isOccurChild) {
		String javaName1=getName(child.getJavaName1());
		String javaName2=getName(child.getJavaName2());		
		arrayLst.add("");
		if (!isOccurChild) {
			addTypedAccessors(arrayLst, javaName2, javaName1, getJavaType1(child));
			return arrayLst;
		} else {
			if (getJavaType1(child).equals("int")) {
				arrayLst.add("public int get" + javaName2 + "(int index) {");
				arrayLst.add("tab()");
				int idx1= child.getLength();
				arrayLst.add("return Integer.parseInt(" + child.getParent().getJavaName1() + ".toString(index*"+
						child.getParent().getLength() + "+" + idx + "," + idx1+ "));");
				arrayLst.add("backTab()");
				arrayLst.add("}");
				arrayLst.add("");
				arrayLst.add("public void set" + javaName2 + "(int index, int value) {");
				arrayLst.add("tab()");
				arrayLst.add("int realIndex = "+child.getParent().getJavaName1() +".getOffset() + index * " +
						child.getParent().getLength()+ ";");		
				idx1+=idx;
				arrayLst.add(child.getParent().getJavaName1() + ".set(value, realIndex + " +
						idx +", realIndex +" + idx1 + ");");
				idx = idx1;
			} else {
				arrayLst.add("public String get" + javaName2 + "(int index) {");
				arrayLst.add("tab()");
				int idx1 = child.getLength();
				arrayLst.add("return " + child.getParent().getJavaName1() + ".toString(index*"
						+ child.getParent().getLength() + "+" + idx + "," + idx1 + ");");
				arrayLst.add("backTab()");
				arrayLst.add("}");
				arrayLst.add("");
				arrayLst.add("public void set" + javaName2 + "(int index, String value) {");
				arrayLst.add("tab()");
				arrayLst.add("int realIndex = " + child.getParent().getJavaName1() + ".getOffset() + index * "
						+ child.getParent().getLength() + ";");
				idx1 += idx;
				arrayLst.add(child.getParent().getJavaName1() + ".set(value, realIndex + " + idx + ", realIndex +"
						+ idx1 + ");");
				idx = idx1;
			}
		}
		arrayLst.add("backTab()");
		arrayLst.add("}");
		return arrayLst;
	}
	private ArrayList<String> printSetGetVar(SymbolProperties child, ArrayList<String> arrayLst) {
		String javaName1=getName(child.getJavaName1());
		String javaName2=getName(child.getJavaName2());		
		addTypedAccessors(arrayLst, javaName2, "record" + javaName2, getJavaType1(child));
		return arrayLst;
	}

	private void printWritebackTabeToString()
	{
		setgetLst.add("");
		setgetLst.add("public void write(String value) {");
		setgetLst.add("tab()");
		setgetLst.add("record.set(value, 0, Length);");
		setgetLst.add("backTab()");
		setgetLst.add("}");
		setgetLst.add("");
		setgetLst.add("public String toString() {");
		setgetLst.add("tab()");
		setgetLst.add("return record.toString();");
		setgetLst.add("backTab()");
		setgetLst.add("}");
	}
	private String getName(String strName) {
		if (strName.length() > 5) {
			if (strName.substring(0, 6).equals("Filler")) {
				if (fillerIdx == 0) {
					strName = "Filler";
				} else {
					strName = "Filler" + fillerIdx;
				}
				fillerIdx++;
			}
		}
		return strName;
	}
	private String getOptionFormat(SymbolProperties child) {
		String strRet="";
		switch (child.getDataUsage()) {
			case 20:
				strRet=".withOption(\"COMP\")";
				break;
			case 30:
				strRet=".withOption(\"COMP-3\")";
				break;				
			default:
				strRet="";
		}
		if (child.isOccurs())
		{
			strRet += ".withOption(\"OCCURS " + child.getMaxOccurs() + "\")"; 
		} 
		return strRet;
	}
	private void printNode(SymbolProperties props, SymbolProperties child) {
		if (child.getJavaName2().contains("Filler")) {
			setgetLst.add("@CharactersFormat(format = \"" + child.getPictureString() + "\")");
			ArrayList<SymbolProperties.CoupleValue> arrayValues = child.getValues();
			int fillercount=0;
			for (SymbolProperties child1 : props.getChildren()) {
				if (child1.getJavaName2().contains("Filler"))fillercount++;
			}
			if (fillercount>1)
			{
				if (arrayValues != null) {
					String strValues = (String) arrayValues.get(0).value1.toString();
					setgetLst.add("private String " + props.getJavaName2() + "Filler" + idxFiller + " = " + strValues + ";");
				} else {
					setgetLst.add("private String " + props.getJavaName2() + "Filler" + idxFiller + ";");
				}
				idxFiller++;
			}else {
				if (arrayValues != null) {
					String strValues = (String) arrayValues.get(0).value1.toString();
					setgetLst.add("private String " + props.getJavaName2() + "Filler = " + strValues + ";");
				} else {
					setgetLst.add("private String " + props.getJavaName2() + "Filler;");
				}
				idxFiller++;
			}
		}
		else {			
			setgetLst.add("@CharactersFormat(format = \"" + child.getPictureString() + "\")");
			ArrayList<SymbolProperties.CoupleValue> arrayValues = child.getValues();
			if (arrayValues != null) {
				String strValues = (String) arrayValues.get(0).value1.toString();
				setgetLst.add("private String " + child.getJavaName2() + " = " + strValues + ";");
			} else {
				setgetLst.add("private String " + child.getJavaName2() + ";");
			}
		}
		setgetLst.add("");
	}
private void printSetGet( SymbolProperties props) {
	if (props.getChildren().get(0).getJavaName2().contains("Filler")) {
		setgetLst.add("public String get" + props.getJavaName2() + "() {");
		setgetLst.add("	return get" + props.getJavaName2() + "Filler();");
		setgetLst.add("}");

		setgetLst.add("public void set" + props.getJavaName2() + "(String value) {");
		setgetLst.add("	set" + props.getJavaName2() + "Filler(value.substring(0," + props.getLength() + "));");
		setgetLst.add("}");
	} else {
		setgetLst.add("public String get" + props.getJavaName2() + "() {");
		setgetLst.add("	return get" + props.getJavaName2() + "();");
		setgetLst.add("}");
		
		setgetLst.add("public void set" + props.getJavaName2() + "(String value) {");
		setgetLst.add("	set" + props.getJavaName2() + "(value.substring(0," + props.getLength() + "));");
		setgetLst.add("}");
	}
	setgetLst.add("");
}
private void printSetGet( SymbolProperties props, SymbolProperties child) {
		setgetLst.add("public String get" + props.getJavaName2() + "() {");
		setgetLst.add("	return get" + child.getJavaName2() + "();");
		setgetLst.add("}");
		
		setgetLst.add("public void set" + props.getJavaName2() + "(String value) {");
		setgetLst.add("	set" + child.getJavaName2() + "(value.substring(0," + child.getLength() + "));");
		setgetLst.add("}");

		setgetLst.add("");
	}
//Add 20210707 S
private  boolean isNumeric(String strNum) {
	Pattern pattern = Pattern.compile("-?\\d+(\\.\\d+)?");
   if (strNum == null) {
       return false; 
   }
   return pattern.matcher(strNum).matches();
}
//Add 20210702 E
/*
private void printSetGetGroup( SymbolProperties props) {
	String strCons = "";
	int idx =1;
	int idy=0;
	int idy1;
	int fillercount=0;
	
	setgetLst.add("public void set" + props.getJavaName2() + "(String value) {");
	for (SymbolProperties child : props.getChildren()) {
		if (child.getJavaName2().contains("Filler"))fillercount++;
	}
	for (SymbolProperties child : props.getChildren()) {
		String strVarname ="";
		idy1 = idy +child.getLength();
		if (child.getJavaName2().contains("Filler")) {
			if (fillercount>1) {
				setgetLst.add("	set" + props.getJavaName2() + "Filler" + idx + "(value.substring(" +
					      idy + "," + idy1 + "));" );
				strVarname=props.getJavaName2() + "Filler" +idx ;
				idx++;						
			}else {
				setgetLst.add("	set" + props.getJavaName2() + "Filler" +  "(value.substring(" +
					      idy + "," + idy1 + "));" );
				strVarname=props.getJavaName2() + "Filler" ;						
			}		
		}else {					
			setgetLst.add("	set" + child.getJavaName2() + "(value.substring(" +
				      idy + "," + idy1 + "));" );	
			strVarname=child.getJavaName2();
			
		}
		idy = idy1;
		//Xu ly cho method get
		if (strCons.equals("")) {
			strCons += "get" + strVarname + "()";
		} else {
			strCons += " + get" + strVarname + "()";
		}
	}
	setgetLst.add("}");
	
	if (props.getLevelNumber() != 1 && props.getChildren().size()>1) {
		setgetLst.add("public String get" + props.getJavaName2() + "() {");
		setgetLst.add("	return " + strCons + ";");
		setgetLst.add("}");
	}			
}*/
// DAONH ADD End: Group all data into one Model with inner classes

	private void printInitialize(SymbolProperties props) {
		ClassFile.doMethodScope("public void initialize(Program p) {");
		ClassFile.tab();
		ClassFile.println("__setProgram(p);");
		printInit(props);
		ClassFile.backTab();
		ClassFile.endMethodScope();
	}

	private void printInit(SymbolProperties props) {
		if(props.hasChildren()) {
			for(SymbolProperties child:props.getChildren()) {
				Cobol2Java.getInstance().initializeOneSymbol(child);
				if(child!=null&&child.getChildren()!=null)
					printInit(child);
			}
		}
	}
	
	public static void reportError() {
		try{
			throw new InvalidCobolFormatException("");
		} catch(Exception e) {
			e.printStackTrace();
			System.exit(-1);
		}
	}

	public void reportError(SymbolProperties props,String msg) {
		System.out.println("@CobolSourceFile(\""+props.getDataDescriptionEntry().sourceFile+"\","+
				props.getDataDescriptionEntry().line+
				"):"+msg);
		System.exit(0);
	}
	
	public void reportError(String msg) {
		System.out.println("@CobolSourceFile(\""+Main.getContext().getSourceFileName()+"):"+msg);
		System.exit(0);
	}
	/**
	 * A copybook is imported as a ready-made model class from the runtime's common.model package
	 * only when it is listed in the alsm.commonModelCopybooks system property (comma separated).
	 * Every other copybook is generated into the program's own model, so the result compiles
	 * without extra classes.
	 */
	private static boolean isCommonModelCopybook(String copybook) {
		String listed = System.getProperty("alsm.commonModelCopybooks", "");
		for (String name : listed.split(",")) {
			if (!name.trim().isEmpty() && name.trim().equalsIgnoreCase(copybook))
				return true;
		}
		return false;
	}

	private String ConvertCopyName(String value)
	{
		String tmp = "";
		if (value != null && value.length()>1)
		{
			tmp=value.substring(0,1).toUpperCase();
			tmp += value.substring(1).toLowerCase();
			tmp+="Model";
		}
		return tmp;
	}

}
