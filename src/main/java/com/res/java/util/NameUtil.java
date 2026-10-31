package com.res.java.util;


import java.util.Iterator;
import java.util.Stack;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.res.common.RESConfig;
import com.res.java.lib.CobolSymbol;
import com.res.java.lib.Constants;
import com.res.java.lib.RunTimeUtil;
import com.res.java.translation.engine.TranslationTable;
import com.res.java.translation.symbol.SymbolConstants;
import com.res.java.translation.symbol.SymbolProperties;
import com.res.java.translation.symbol.SymbolTable;
import com.res.java.translation.symbol.SymbolUtil;

public class NameUtil {

	public static String convertCobolNameToJava(String cobolName,boolean firstUpper) {
		
		if(cobolName==null) return "";
		
		cobolName = cobolName.toUpperCase();
	
		StringBuilder javaName=new StringBuilder(cobolName.length()+5);
		
		int i=0;
		int j=0;
		
		while(i<cobolName.length()) {
			if(i==0) {
				if(cobolName.charAt(i)>='A'&&cobolName.charAt(i)<='Z') {
					if(firstUpper)
						javaName.append(cobolName.charAt(i));
					else
						javaName.append((char)NameUtil.lowerCase[cobolName.charAt(i)-'A']);
				}
				else {
					javaName.append("_"+cobolName.charAt(i));
				}
			} else
				if(cobolName.charAt(i)=='-'||cobolName.charAt(i)=='.') {
					i++;
					if(cobolName.charAt(i)>='A'&&cobolName.charAt(i)<='Z') {
						javaName.append(cobolName.charAt(i));
					} else { 
						javaName.append('_');
						if(cobolName.charAt(i)>='0'&&cobolName.charAt(i)<='9') 
							javaName.append(cobolName.charAt(i));
					}
				} else {
					if(cobolName.charAt(i)>='A'&&cobolName.charAt(i)<='Z') {
						javaName.append((char)NameUtil.lowerCase[cobolName.charAt(i)-'A']);
					}
					else if(cobolName.charAt(i)>='0'&&cobolName.charAt(i)<='9'||cobolName.charAt(i)=='_')
						javaName.append(cobolName.charAt(i));
					else;
				}
			i++;
			j++;
		}
		String name=javaName.toString().trim();
		if(JAVA_RESERVED_WORDS.contains(name.toLowerCase()))
			name+=String.valueOf(nameMark++)+"_";
		return name;
	}

	private static byte[] lowerCase =
	{'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r',
		's','t','u','v','w','x','y','z'};

	@SuppressWarnings("unchecked")
	public static String getJavaName(SymbolProperties props2,boolean lastSet) {
		
		if(props2.getIsFiller())
			if(!lastSet) return SymbolUtil.getInstance().getFillerGetter(props2);
			else return SymbolUtil.getInstance().getFillerSetter(props2, "%0");
		
		SymbolProperties props;
		String s = "";	
		
		if(props2.isVaryingArray()&&props2.getParent().isVarying()) {
			props2=props2.getParent();
		}
		else 
			if(props2.isVaryingLen()&&props2.getParent().isVarying()){
				if(!lastSet) return getJavaName(props2.getParent(),false)+".length()";
				else { 
					RunTimeUtil.getInstance().reportError("Warning: Setting Varying Length Ignored.",false);
					return "setNull(%0)";
				}
			}
		
		if(props2.getLevelNumber()==1&&props2.getPictureString()==null||
				(props2.getLevelNumber()==88&&
						(props2.getParent().getLevelNumber()==1||props2.getParent().getLevelNumber()==77)&&
				props2.getPictureString()==null)) {
			if(RESConfig.getInstance().isUsePointers()) {
					s="get"+getJavaName2(props2)+"()";
				if(lastSet) {
					s+="set"+"(%0)";
				}
			} else {
				if(!lastSet) {
					s="get"+getJavaName2(props2)+"()";
				} else {
					s="set"+getJavaName2(props2)+"(%0)";
				}
			}
			if(NameUtil.appendInstance)
				return "instance_." +s;
			else
				return s;
		}
		
		props=props2;
		
		Stack st = new Stack();
		boolean isFirst=true;
		while(props!=null) {
			if(!props.getIsSuppressed()&&!(props.getIsFiller()&&!props.is01Group()))
				if(props.getType()==SymbolConstants.DATA)
					if(props.getLevelNumber()==1||
							props.getLevelNumber()==77||isFirst)
						st.push(props);
			props=(SymbolProperties)props.getParent();
			isFirst=false;
		}
		
		boolean doRefModSet=false;
		String refModSet = "";
		if(lastSet&&!(props2.getSubstringWorkSpace()==null||props2.getSubstringWorkSpace().size()<=0)) {
			doRefModSet=true;
		}
		boolean first=true;
		boolean frstrun=true;
		while(st!=null&&st.size()>0) {
			//20210707 Add var level1 with PictureString<>null S 
			if (st.size()==1 && frstrun)
			{
				props = (SymbolProperties) st.pop();
				if (props.getLevelNumber()==1)
				{
					s="model.";
				}
				frstrun=false;
			}
			else
			//20210707 Add E
			props = (SymbolProperties) st.pop();
			if(!first) {
				s+=".";
				if(doRefModSet)	
					refModSet+=".";
			}
			
			if(st.size()>0) {
				// QuyTV add frefix "model.get" for get item from model
				s+="model.get" + getJavaName2(props)+"()";
				//s+=getJavaName2(props)+"()";
				if(doRefModSet)	refModSet+=getJavaName1(props);
			}
			else
			if(!lastSet){
				s+="get"+getJavaName2(props);
			}
			else {
				if(doRefModSet)	refModSet+="get"+getJavaName2(props);
				s+="set"+getJavaName2(props);
			}
			first=false;
		}
		if(!lastSet){
				s+="("+NameUtil.getJavaIndexes(props2)+")";
		}
			else {
				if(RESConfig.getInstance().isUsePointers()) {
					s="get"+getJavaName2(props2)+"()";
					if(lastSet) {
						s+="set"+"(%0)";
					}
				} else {
					String ind;
					if((ind=NameUtil.getJavaIndexes(props2)).length()<=0) {
						s+="(%0)";
						if(doRefModSet) {
							refModSet+="()";
						}
					}
					else {
						//Update 20210624 S
						//s+="(%0," +ind+	")";
						s+="(" + ind +	",%0)";
						//Update 20210624 E
						if(doRefModSet) {
							refModSet+="("+ind+")";
						}
					}
				}
			}
		
		if(!lastSet)
			s=NameUtil.doRefMod(props2,s);
		else
			if(doRefModSet)
				s=NameUtil.doRefMod2(props2,s,refModSet);
		
		if(NameUtil.appendInstance)
			return "instance_."+s;
		else
			return s;
	}
	
	 //Add 20210623 S
	public static boolean isNumeric(String strNum) {
		Pattern pattern = Pattern.compile("-?\\d+(\\.\\d+)?");
	    if (strNum == null) {
	        return false; 
	    }
	    return pattern.matcher(strNum).matches();
	}
	//Add 20210623 E
	
	public static String getJavaIndexes(SymbolProperties props) {
		if (props.getIndexesWorkSpace()==null||props.getIndexesWorkSpace().size()<=0) return "";
		String s="";
		if(props.getIndexesWorkSpace()!=null&&props.getIndexesWorkSpace().size()>0) {
			for(Iterator<String> ite=props.getIndexesWorkSpace().iterator();ite.hasNext();) {
				//Update 20210623 S
				//s+=ite.next();
				String ind =ite.next();
				if (isNumeric(ind)){
					int tmp=Integer.parseInt(ind);
					tmp=(tmp==0)?tmp:tmp-1;
					s+=tmp;
				}
				else
					s+=ind;
				//Update 20210623 E
				if(ite.hasNext())
					s+=",";
			}
		}
		return s;
	}

	static String doRefMod(SymbolProperties props,String s){

		if(props.getSubstringWorkSpace()==null||props.getSubstringWorkSpace().size()<=0)
			return s;
		if(props.getJavaType().type!=CobolSymbol.STRING) {
			if(props.getJavaType().type==Constants.GROUP) {
				s = "new String("+s+','+NameUtil.simplify(props.getSubstringWorkSpace().get(0));
				if(props.getSubstringWorkSpace().size()>1)
						s+=","+NameUtil.simplify(props.getSubstringWorkSpace().get(0)+"+"+props.getSubstringWorkSpace().get(1));
				s+=")";
			} else {
			if(props.getIsFormat())
				s=NameUtil.getFormatName2(props,true).replace("%0",s);
			else
				s="new String("+s+")";
			s=s+".substring("+NameUtil.simplify(props.getSubstringWorkSpace().get(0));
			if(props.getSubstringWorkSpace().size()>1)
				s+=","+NameUtil.simplify(props.getSubstringWorkSpace().get(0)+"+"+props.getSubstringWorkSpace().get(1));
			s+=")";
			}
		}
		
		s=TranslationTable.getInstance().convertType(s, props.getJavaType().type, CobolSymbol.STRING);
		
		return s;
		
	}
	static String doRefMod2(SymbolProperties props,String s,String g){//Set RefMod
		if(props.getSubstringWorkSpace()==null||props.getSubstringWorkSpace().size()<=0)
			return s;

		String refMod="new CobolString("+g+")";
		
		refMod=refMod+".refMod(%0,"+NameUtil.simplify(props.getSubstringWorkSpace().get(0));
		if(props.getSubstringWorkSpace().size()>1)
			refMod+=","+NameUtil.simplify(props.getSubstringWorkSpace().get(0)+"+"+
					props.getSubstringWorkSpace().get(1));
		refMod+=").toString()";
		s=s.replace("%0",TranslationTable.getInstance().convertType(refMod, props.getJavaType().type, CobolSymbol.STRING));
		return s;
	}

	static String simplify(String expr) {
		return expr;//simply.simplify(expr);
	}

	@SuppressWarnings("unchecked")
	@Deprecated
	public static String getFormatName(final SymbolProperties props2,boolean force) {
		if(!props2.getIsFormat())
			return "";
		SymbolProperties props=props2;
		String s = "";
		Stack st = new Stack();
		while(props!=null) {
			if(!props.getIsSuppressed())
				if(props.getType()==SymbolConstants.DATA)
					if(props.getLevelNumber()==1||
							props.getLevelNumber()==77||props.getPictureString()!=null)
						st.push(props);
			
			props=(SymbolProperties)props.getParent();
		}
		boolean first=true;
		while(st!=null&&st.size()>0) {
			props = (SymbolProperties) st.pop();
			String name1=(String)getJavaName1(props);
			String name2=(String)getJavaName2(props);
			if(!first)
				s+=".";
			if(st.size()==0){
				s+=name1+"Fmt_";
			}
			else 
				s+=name2;
			first=false;
		}
		return s+".format";
	}

	public static String getSetName(SymbolProperties props2) {
		return "set"+getJavaName2(props2);
	}

	public static String getFileName(SymbolProperties props2, boolean isData) {
		// DAONH MOD Start: Checking first time to see Cobol's data variable to create Model class
//		String s=getJavaName2(props2)+".java";
     	String s="";
		if(props2.isFirstDataVar() || isData) {
			s = SymbolTable.getScope().getFirstProgram().getJavaName2() + "Model.java";
		} else {
		if(props2.getFlgJobFile()) {
			 s=getJavaName2(props2)+"Job.java";
		}else {
			 s=getJavaName2(props2)+"Tasklet.java";
		}
				
		}
		return s;
		// DAONH MOD Start: Checking first time to see Cobol's data variable to create Model class
	}

	public static String getBeanInfoFileName(SymbolProperties props2, boolean isData) {
		String s=getJavaName2(props2)+"BeanInfo.java";
		return s;
	}

	public static String getPackageName(SymbolProperties props2, boolean programPackage) {
	
		RESConfig config = RESConfig.getInstance();
		
		if(programPackage)
				return config.getProgramPackage().replace('\\', '.');
		else {
			
			SymbolProperties props=props2;

			props=SymbolTable.getScope().getFirstProgram();
			
			if(props!=null&&!props.isProgram())	props=null;
			
			return config.getDataPackage().replace('\\', '.')+
				((RESConfig.getInstance().isLongDataPackageName()&&props!=null)?
						('.'+props.getJavaName1().toLowerCase()):"");

		}
	}

	public static String getPathName(String fileName,String suffix,boolean isData) {
		String p=null;
		if(isData) 
			p=RESConfig.getInstance().getDataPackage();
		else
			p=RESConfig.getInstance().getProgramPackage();
		p=p.toLowerCase().replace('.', '\\')+'\\';
		if(suffix!=null&&suffix.trim().length()>0) 
			p+=suffix.toLowerCase().replace('.','\\');
		p+='\\'+NameUtil.getClassName(fileName,suffix,isData);
		p+=".java"	;
		
		return p;
	}
	
	public static String getPackageName(String suffix,boolean isData) {
		String p=null;
		if(isData) 
			p=RESConfig.getInstance().getDataPackage();
		else
			p=RESConfig.getInstance().getProgramPackage();
		if(suffix!=null&&suffix.trim().length()>0) 
			p+='.'+suffix.toLowerCase();
		return p;
	}

	public static String getClassName(String fileName,String suffix,boolean isData) {
		String c=convertCobolNameToJava(fileName, true);
		if(suffix!=null&&suffix.trim().length()>0) {
			c+=convertCobolNameToJava(suffix, true);
		} 
		return c;
	}

	public static String getLongClassName(String fileName,String suffix,boolean isData) {
		String lc=getPackageName(suffix,isData)+"."+convertCobolNameToJava(fileName, true);
		if(suffix!=null&&suffix.trim().length()>0) {
			lc+=convertCobolNameToJava(suffix, true);
		} 
		return lc;
	}

	public static String getProgramName(SymbolProperties props) {
		while(props!=null&&props.getType()!=SymbolConstants.PROGRAM)
			props=props.getParent();
		if(props!=null)
			return props.getDataName();
		return "";
	}

	private static boolean appendInstance=false;

	private static final TreeSet<String> JAVA_RESERVED_WORDS= new TreeSet<String>();

	static {
		for(String s:new String[]{
				"abstract","continue","for","new","switch","assert",
				"default","goto","package","synchronized","boolean",
				"do","if","private","this","break","double","implements",
				"protected","throw","byte","else","import","public",
				"throws","case","enum","instanceof","return","transient",
				"catch","extends","int","short","try","char","final",
				"interface","static","void","class","finally","long",
				"strictfp","volatile","const","float","native","super","while"
				}){
			JAVA_RESERVED_WORDS.add(s);
		}
	}
	
	public static long nameMark=1;
	
	public static String getJavaName1(SymbolProperties props) {
		if(props.getJavaName1()!=null)
			return props.getJavaName1();
		RunTimeUtil.getInstance().reportError("Error: Java name for " +props.getDataName()+
				" is null. Contact RES support.", true);
		return "nullName";

	}
	
	public static String getJavaName2(SymbolProperties props) {
		if(props.getJavaName2()!=null)
			return props.getJavaName2();
		RunTimeUtil.getInstance().reportError("Error: Java name for " +props.getDataName()+
				" is null. Contact RES support.", true);
		return "NullName";
	}
	
	public static String convertCobolNameToSQL(String name) {
		return convertCobolNameToJava(RunTimeUtil.getInstance().stripQuotes(name.toUpperCase()).replace('.', '_'),true);
	}

	public static String getFormatName2(SymbolProperties lhs, boolean b) {
		
		if(!lhs.getIsFormat())
			return "%0";
		
		SymbolProperties props=lhs;
		String s = "";
		Stack<SymbolProperties> st = new Stack<SymbolProperties>();
		while(props!=null) {
			if(!props.getIsSuppressed())
				if(props.getType()==SymbolConstants.DATA)
					if(props.getLevelNumber()==1||
							props.getLevelNumber()==77||props.getPictureString()!=null)
						st.push(props);
			
			props=(SymbolProperties)props.getParent();
		}
		
		boolean first=true;
		
		while(st!=null&&st.size()>0) {
			props = (SymbolProperties) st.pop();
			String name1=(String)getJavaName1(props);
			String name2=(String)getJavaName2(props);
			if(!first)
				s+=".";
			if(st.size()==0){
				s+=name1+"Fmt_";
			}
			else 
				s+=name2;
			first=false;
		}
		/*
		if(Main.getContext().isDecimalPointIsComma()) 
			if(Main.getContext().getCurrencySign()!=null)
				return s+".format(%0,"+Main.getContext().getCurrencySign()+",true"+")";
			else 
				return s+".format(%0"+",true"+")";
		else
			if(Main.getContext().getCurrencySign()!=null)
				return s+".format(%0,"+Main.getContext().getCurrencySign()+",false)";
			else */
				return s+".format(%0)";
	
	}
}
