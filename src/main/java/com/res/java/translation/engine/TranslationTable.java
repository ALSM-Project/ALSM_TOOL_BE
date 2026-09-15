package com.res.java.translation.engine;

import java.util.regex.Pattern;

import com.res.java.lib.CobolSymbol;
import com.res.java.lib.Constants;
import com.res.java.translation.symbol.ExpressionString;
import com.res.java.translation.symbol.SymbolConstants;
import com.res.java.translation.symbol.SymbolProperties;
import com.res.java.translation.symbol.SymbolTable;
import com.res.java.util.NameUtil;


public class TranslationTable {

    private static TranslationTable thiz=null;

    public static TranslationTable getInstance() {
        if(thiz==null)
            thiz=new TranslationTable();
        return thiz;
    }

    private TranslationTable() {}

	private static final String[][] DOIDOP_CONVERSION=
	new String[][]{
	//To->    	BYTE						CHAR							
	//To->    	SHORT						INTEGER						LONG			
	//To->    	FLOAT						DOUBLE			
	//From		BIGDECIMAL					STRING						CLASS
	/*BYTE*/ 	{"%1",						"(char) (%1)",			
				"(short) (%1)",				"(int) (%1)",					"(long) (%1)",			
				"(float) (%1)",				"(double) (%1)",				
	    	"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
			"%1"},
   	/*CHAR*/ 	{"(byte) (%1)",				"%1",			
				"(short) (%1)",				"(int) (%1)",					"(long) (%1)",			
				"(float) %1",				"(double) %1",				
				"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
				"%1"},
	/*SHORT*/ 	{"(byte) (%1)",				"(char) (%1)",			
	    		"%1",						"%1",						"%1",
				"(float) (%1)",				"(double) (%1)",						
	    	"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
			"%1"},
   	/*INTEGER*/	{"(byte) (%1)",				"(char) (%1)",			
	    		"(short) (%1)",				"%1",						"%1",		
//20210712 Update S	    			    		
//				"(float) (%1)",				"(double) (%1)",
				"(float) (%1)",				"%1",				
//20210712 Update E
				"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
			"%1"},
   	/*LONG*/	{"(byte) (%1)",				"(char) (%1)",			
	    		"(short) (%1)",				"(int) (%1)",						"%1",			
				"(float) (%1)",				"(double) (%1)",				
	    	"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
			"%1"},
   	/*FLOAT*/	{"(byte) (%1)",				"(char) (%1)",			
	    		"(short) (%1)",				"(int) (%1)",						"(long) (%1)",			
				"%1",						"%1",				
	    	"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
				"%1"},
   	/*DOUBLE*/	{"(byte) (%1)",				"(char) (%1)",			
	    		"(short) (%1)",				"(int) (%1)",						"(long) (%1)",			
				"(float) (%1)",				"%1",				
	    	"new BigDecimal(%1)",		"String.valueOf(%1)",			"String.valueOf(%1).getBytes()",
			"%1"},
   	/*BIGDECIMAL*/	{"%1.byteValue()",		"(char) %1.byteValue()",			
	    		"%1.shortValue()",			"%1.intValue()",			"%1.longValue()",			
				"%1.floatValue()",				"%1.doubleValue()",				
				"%1",						"%1.toPlainString()",		"%1.toPlainString().getBytes()",
				"%1"},
   	//TuNT1 M S
	/*STRING*/	//{"Byte.parseByte(%1)",		"__getChar(%1)",		
   				{"Byte.parseByte(%1)",		"%1",		
	//TuNT M E				
					"FieldFormat.parseInt(%1)",	"FieldFormat.parseInt(%1)",		"FieldFormat.parseLong(%1)",			
					"Float.parseFloat(%1)",	"Double.parseDouble(%1)",	
					// TuNT1 M S
					//"new BigDecimal(%1)",		"%1",						"%1.getBytes()",
					"new BigDecimal(%1)",		"%1",						"%1",
					// TuNT1 M E
					"%1"},
   	//TuNT1 M S   				
	/*CLASS*///	{"%1[0]",		"__getChar(%1)",	
   	//TuNT1 M E
   				{"%1[0]",		"%1",
				"FieldFormat.parseInt(new String(%1))","FieldFormat.parseInt(new String(%1))","FieldFormat.parseLong(new String(%1))",			
				"Float.parseFloat(new String(%1))",	"Double.parseDouble(new String(%1))",				
				"new BigDecimal(new String(%1))","new String(%1)",		"%1",
						"%1"},
	/*OBJECT*/	{"(Byte)%1",		"(Char) %1",			
						"(Integer)%1","(Integer)%1","(Long)%1",			
								"(Float)%1",	"(Double)%1",				
								"new BigDecimal(%1.toString())","%1.toString()",		"%1.toString().getBytes()",
				"%1"}
	};
	

	private static final String[] DOIDOP_OPERATION=
		new String[]
			{"_Math.add(%1,%2)","_Math.subtract(%1,%2)","_Math.multiply(%1,%2)","_Math.divide(%1,%2)",
			 "_Math.remainder(%1,%2)","_Math.pow(%1,%2)"};
	private static final String[] DO_NATIVE_OP_OPERATION=
		new String[]
			{"%1 + %2","%1 - %2","%1 * %2","%1 / %2",
			 "%1 % %2","Math.pow(%1,%2)"};
	
	public static final int ADD_OP = 0;
	public static final int SUBTRACT_OP = 1;
	public static final int MULTIPLY_OP = 2;
	public static final int DIVIDE_OP = 3;
	public static final int REMAINDER_OP = 4;
	public static final int POW_OP = 5;
	
    public String doOp(int op,ExpressionString arg1,ExpressionString arg2) {
    	Cobol2Java.getInstance().expressionType=Math.max(arg1.type,arg2.type);
    	if(op==DIVIDE_OP) {
    		Cobol2Java.getInstance().expressionType=Math.max(Cobol2Java.getInstance().expressionType, Constants.DOUBLE);
    		arg1=convertType(arg1,Cobol2Java.getInstance().expressionType,arg1.type);
    	}
		if(arg1.type<Constants.INTEGER) {
			arg1=convertType(arg1,Constants.INTEGER,arg1.type);
			arg1.type=Constants.INTEGER;
		}
		if(arg2.type<Constants.INTEGER) {
			arg2=convertType(arg2,Constants.INTEGER,arg2.type);
			arg2.type=Constants.INTEGER;
		}
		if(op<0||op>=DOIDOP_OPERATION.length) op = 0;//reset to add()
		if(Math.max(arg1.type,arg2.type)>=CobolSymbol.BIGDECIMAL)
				return DOIDOP_OPERATION[op].replace("%1",arg1.toString()).replace("%2", arg2.toString());
		else return DO_NATIVE_OP_OPERATION[op].replace("%1",arg1.toString()).replace("%2", arg2.toString());
    }

    public String doOp(String opArg,ExpressionString arg1,ExpressionString arg2) {
    	int op =0;
    	switch(opArg.trim().charAt(0)) {
    	case '+':
    		op = ADD_OP;break;
    	case '-':
    		op = SUBTRACT_OP;break;
    	case '*':
    		op = MULTIPLY_OP;break;
    	case '/':
    		op = DIVIDE_OP;break;
    	case '%':
    		op = REMAINDER_OP;break;
    	default:
    		op = POW_OP;break;
    	}
    	return doOp(op,arg1,arg2);
    }
    
    public void doIdOp(SymbolProperties id,String op) {
		if(Cobol2Java.getInstance().expression.size()<=0) {
			Cobol2Java.getInstance().expression.push(new ExpressionString(id).toString());
			Cobol2Java.getInstance().expressionType=id.getJavaType().type;
		}
		else {
	    	Cobol2Java.getInstance().expression.push(
	    			doOp(op,ExpressionString.getExpression(),new ExpressionString(id)));
		}
   /* 	
		if(Cobol2Java.getInstance().expression.size()<=0)
			Cobol2Java.getInstance().expressionType=0;
		
		CobolSymbol sym=(CobolSymbol)id.getJavaType();
		
		if(sym==null||
				Cobol2Java.getInstance().expressionType>=CobolSymbol.MAX_TYPES||
				sym.type>=CobolSymbol.MAX_TYPES) return;
		
		String exprString= NameUtil.getJavaName(id,false);
		if(RESConfig.getInstance().isUsePointers()) exprString+=".get()";
		//Conversion
    	if(sym.type>Cobol2Java.getInstance().expressionType&&Cobol2Java.getInstance().expression.size()>0){
    		Cobol2Java.getInstance().expression.push(
    				convertType( Cobol2Java.getInstance().expression.pop3(),
    						sym.type, Cobol2Java.getInstance().expressionType)
    			);
    		Cobol2Java.getInstance().expressionType=sym.type;
    	}
    	else {
    		if(Cobol2Java.getInstance().expression.size()>0)
    			exprString=
    				convertType( exprString,Cobol2Java.getInstance().expressionType,
						sym.type);
    	}
    	
       	//Operation
		if(Cobol2Java.getInstance().expression.size()>0) {
	
			exprString = DOIDOP_OPERATION[mapOp2Idx(op)].
				replace("%2", Cobol2Java.getInstance().expression.pop3()).replace("%1",exprString);

		}

		Cobol2Java.getInstance().expression.push(exprString);
		Cobol2Java.getInstance().expression.setType(
		    	Cobol2Java.getInstance().expressionType=Math.max(Cobol2Java.getInstance().expressionType,sym.type)
		);
		*/
	}

    //This is Holier than Holy... do not touch without knowing your prayers.
    /*
	private int mapOp2Idx(String op) {
		switch(op.charAt(0)) {
		case '-':return 1;
		case '*':
			if(op.length()==2&&op.charAt(1)=='*'){
				return 5;
			}
			return 2;
		case '/':return 3;
		case '%':return 4;
		case '+':
		default:return 0;
		}
	}
	*/
	public static Pattern NUMBER_PATTERN = Pattern.compile("(([\\-\\+\\*\\/%]?[0-9]*(\\.[0-9]+)?)|\\.[0-9]+)+");
	
	public void doLiteralOp(String lit,String op) {
		if(Cobol2Java.getInstance().expression.size()<=0)
			Cobol2Java.getInstance().expression.push(Cobol2Java.getInstance().formatLiteral(lit));
		else {
			ExpressionString arg2=ExpressionString.getExpression();
			ExpressionString arg1=Cobol2Java.getInstance().formatLiteral(lit);
			Cobol2Java.getInstance().expression.push(
					doOp(op,arg2,arg1));
		}
		/*
		int prevExpressionType=0;

		if(Cobol2Java.getInstance().expression.size()>0)
			prevExpressionType=Cobol2Java.getInstance().expressionType;
		
		//Sets Cobol2Java.getInstance().expressionType
		String exprString=Cobol2Java.getInstance().formatLiteral((String)lit).toString();
				
		if(Cobol2Java.getInstance().expressionType>=CobolSymbol.MAX_TYPES||
				prevExpressionType>=CobolSymbol.MAX_TYPES) return;
		
		//Conversion 
    	if(Cobol2Java.getInstance().expressionType>prevExpressionType&&Cobol2Java.getInstance().expression.size()>0){
    		Cobol2Java.getInstance().expression.push(
    				convertType( Cobol2Java.getInstance().expression.pop3(),
    						Cobol2Java.getInstance().expressionType,prevExpressionType)
    		);
    	}
    	else {
    		if(Cobol2Java.getInstance().expression.size()>0)
			exprString=
				convertType( exprString
						,prevExpressionType,
						Cobol2Java.getInstance().expressionType);
    	}
		
    	
    	//Operation
		if(Cobol2Java.getInstance().expression.size()>0) {//Stack greater than 0
				exprString = DOIDOP_OPERATION[mapOp2Idx(op)].
    				replace("%2", Cobol2Java.getInstance().expression.pop3()).replace("%1",exprString);
		}
		
		Cobol2Java.getInstance().expression.push(exprString);
		Cobol2Java.getInstance().expression.setType(
		    	Cobol2Java.getInstance().expressionType=Math.max(Cobol2Java.getInstance().expressionType,prevExpressionType)
		);
		*/
	}

	public void doOp(SymbolProperties props,String op) {
		doIdOp(props,op);
	}
	
	public void doOp(String lit,String op) {
		doLiteralOp(lit,op);
	}
	public void doOp(String exprString,int expressionType,String op) {
		if(Cobol2Java.getInstance().expression.size()<=0) {
			Cobol2Java.getInstance().expression.push(exprString);
			Cobol2Java.getInstance().expressionType=expressionType;
		}
		else {
			ExpressionString arg2=ExpressionString.getExpression();
			ExpressionString arg1=new ExpressionString(exprString,expressionType);
			Cobol2Java.getInstance().expression.push(
					doOp(op,arg2,arg1));
		}
		/*
		int prevExpressionType=0;
		if(Cobol2Java.getInstance().expression.size()>0)
			prevExpressionType=Cobol2Java.getInstance().expressionType;
		
		if(expressionType>=CobolSymbol.MAX_TYPES||
				prevExpressionType>=CobolSymbol.MAX_TYPES) return;
		
		//Conversion 
    	if(expressionType>prevExpressionType&&Cobol2Java.getInstance().expression.size()>0){
    		Cobol2Java.getInstance().expression.push(
    				convertType(Cobol2Java.getInstance().expression.pop3()
							,expressionType,
							prevExpressionType)
				);
    		prevExpressionType=expressionType;
    	}
    	else {
    		if(Cobol2Java.getInstance().expression.size()>0)
				exprString = convertType(exprString, prevExpressionType,
						expressionType);
    	}
		
    	//Operation
		if(Cobol2Java.getInstance().expression.size()>0){//Stack greater than 0
				exprString = DOIDOP_OPERATION[mapOp2Idx(op)].
					replace("%2", Cobol2Java.getInstance().expression.pop3()).replace("%1",exprString);
		}
			
		Cobol2Java.getInstance().expression.push(exprString);
		
		Cobol2Java.getInstance().expression.setType(
		    	Cobol2Java.getInstance().expressionType=Math.max(Cobol2Java.getInstance().expressionType,prevExpressionType)
		);

	*/
	}
	public String getAssignString(SymbolProperties lhs,SymbolProperties rhs,boolean isAll) {
		 return getAssignString(lhs,rhs,isAll,true,false);
	}	
	public String getAssignString(SymbolProperties lhs,SymbolProperties rhs,boolean isAll,boolean addSemi) {
		 return getAssignString(lhs,rhs,isAll,addSemi,false);
	}
	public String getAssignString(SymbolProperties lhs,SymbolProperties rhs,boolean isAll,boolean addSemi,boolean isSpecialMove) {
		//Lay doi dung phep gan
		int allLength=0;
		if(isAll&&lhs!=null) allLength=lhs.getLength();
		
		String exprString,lhsVarString;
		int lhsType,rhsType=0;
		
		if(rhs==null)
			rhsType=Cobol2Java.getInstance().expressionType;
		else
			rhsType =rhs.getJavaType().type;
		
		if(lhs==null&&rhs==null) {//Create temporary variable assignment from the expression
			String tempVar="temp"+new Integer(SymbolTable.tempVariablesMark++).toString().trim();
			exprString=SymbolConstants.get(Cobol2Java.getInstance().expressionType)+" "+
				tempVar+" = "+Cobol2Java.getInstance().expression.pop()+";";
			return exprString;
		}
			
		if(lhs==null) {//Used in file reads and writes
			lhsVarString=Cobol2Java.getInstance().expression.pop()+"(%0)";
			lhsType=Cobol2Java.getInstance().expressionType;
		}
		else {//General assigns
			lhsType=lhs.getJavaType().type;
			isSpecialMove=isSpecialMove&&!((lhsType<Constants.STRING&&rhsType<Constants.STRING)||(lhsType>=Constants.STRING&&rhsType>=Constants.STRING));
			if(isSpecialMove) {
				if(lhs.getIsFormat()&&lhs.getJavaType().type>Constants.BIGDECIMAL) {
					lhsVarString=NameUtil.getJavaName(lhs,true).replace("%0",NameUtil.getFormatName2(lhs,false));
					rhsType=Constants.STRING;
				} else {
					lhsVarString=NameUtil.getJavaName(lhs,true);
				}
			}
			else
				lhsVarString=NameUtil.getJavaName(lhs,true);
		}
		
		if (rhs==null){ 
			exprString=Cobol2Java.getInstance().expression.pop3();
		}
		else {
			if(isSpecialMove){
				if(rhs.getIsFormat()) {
					exprString=NameUtil.getFormatName2(rhs,false).replace("%0",NameUtil.getJavaName(rhs,false));
					rhsType=Constants.STRING;
				}
				else {
					exprString=NameUtil.getJavaName(rhs,false);
					rhsType =Math.max(rhs.getJavaType().type,rhsType);
				}
			} else {
				exprString=NameUtil.getJavaName(rhs,false);
			}
		}
		
		if(lhsType==CobolSymbol.UNKNOWN||rhsType==CobolSymbol.UNKNOWN)
			return "";
		
		if(lhs.isJustifiedRight()) {
			exprString="__justifyRight("+exprString+","+String.valueOf(lhs.getLength())+")";
			rhsType=Constants.STRING;
		} 
		/*
		if(lhsType==CobolSymbol.STRING&&rhs!=null)
			exprString=convertToStringType(exprString,rhs);
		else
		if(lhsType==CobolSymbol.GROUP&&rhs!=null)
			exprString=convertToGroupType(exprString,rhs);
		else
		*/
		// Delete 20210623 S
			//exprString = convertType(exprString, lhsType, rhsType);
		// Delete 20210623 E
			
    	Cobol2Java.getInstance().expressionType=Math.max(lhsType,Cobol2Java.getInstance().expressionType);
    	
    	if(isAll&&allLength>0) 
    		exprString="__all("+exprString+","+String.valueOf(allLength)+")";
    	exprString =  lhsVarString.replace("%0",exprString);
		/* 
    	if(lhs==null)
    		return exprString;
    	else 
		*/
    		return exprString+(addSemi?";":"");
	}

	//Conversion 
	public String convertType(String exprString, int lhsType, int rhsType) {

		/*
		if((lhsType==CobolSymbol.BIGDECIMAL||rhsType==CobolSymbol.BIGDECIMAL)&&
				NUMBER_PATTERN.matcher(exprString).matches()){
			if(lhsType==Constants.GROUP)
				exprString=DOIDOP_CONVERSION[CobolSymbol.GROUP][lhsType].replace(
						"%1", exprString);
			else
				exprString=DOIDOP_CONVERSION[CobolSymbol.LONG][lhsType].replace(
						"%1", exprString);
				
			Cobol2Java.getInstance().expressionType=lhsType;
		}
		else
		*/
		Cobol2Java.getInstance().expressionType=lhsType;
			exprString=DOIDOP_CONVERSION[rhsType][lhsType].replace(
					"%1", exprString);
		return exprString;
	}

	//Conversion 
	public ExpressionString convertType(ExpressionString exprString, int lhsType, int rhsType) {
		exprString.literal=new StringBuffer(convertType(exprString.toString(),lhsType,rhsType));
		return exprString;
	}

	public String converSymbolToStringType(SymbolProperties symbol) {
		Cobol2Java.getInstance().expressionType=CobolSymbol.STRING;
		if(symbol.getJavaType().type<CobolSymbol.STRING&&symbol.getIsFormat())
			return NameUtil.getFormatName2(symbol,true).replace("%0",
				NameUtil.getJavaName(symbol, false));
		else
			return DOIDOP_CONVERSION[symbol.getJavaType().type][CobolSymbol.STRING].replace(
				"%1", NameUtil.getJavaName(symbol, false));
		
	}
	public ExpressionString converSymbolToStringType2(SymbolProperties symbol) {
		return new ExpressionString(converSymbolToStringType(symbol)).setLength(symbol.getLength());
	}


	public String convertToStringType(String rhs,SymbolProperties symbol) {
		Cobol2Java.getInstance().expressionType=CobolSymbol.STRING;
		if(symbol.getJavaType().type<CobolSymbol.STRING&&symbol.getIsFormat()) 
			return NameUtil.getFormatName2(symbol,true).replace("%0",
				rhs);
		else
			return DOIDOP_CONVERSION[symbol.getJavaType().type][CobolSymbol.STRING].replace(
				"%1", rhs);
	}
	
	public String convertToGroupType(String rhs,SymbolProperties symbol) {
		Cobol2Java.getInstance().expressionType=CobolSymbol.GROUP;
		if(symbol.getJavaType().type==CobolSymbol.GROUP&&symbol.getIsFormat())
			return NameUtil.getFormatName2(symbol,true).replace("%0",
					rhs)+".getBytes()";
		else
			return DOIDOP_CONVERSION[symbol.getJavaType().type][CobolSymbol.GROUP].replace(
					"%1", rhs);

	}
	
	public ExpressionString convertToStringType2(ExpressionString rhs,SymbolProperties symbol) {
		rhs.literal=new StringBuffer(convertToStringType(rhs.toString(),symbol));
		rhs.setLength(symbol.getLength());
		return rhs;
	}

	public static final int AND_CONDITION = 0;
	public static final int OR_CONDITION = 1;
	public static final int EQ_CONDITION = 2;
	public static final int NE_CONDITION = 3;
	public static final int LT_CONDITION = 4;
	public static final int GE_CONDITION = 5;
	public static final int GT_CONDITION = 6;
	public static final int LE_CONDITION = 7;
	public static final int POSITIVE_CONDITION = 0;
	public static final int NEGATIVE_CONDITION = 1;
	public static final int ZERO_CONDITION = 2;
	public static final int NOT_ZERO_CONDITION = 3;
	
	private static final int[] REVERSE_OP =	new int[]{1,0,3,2,5,4,7,6};
	private static final String[] DOID_NATIVE_CONDITION_OP =
		new String[]{"%1 && %2","%1 || %2",
					 "%1 == %2","%1 != %2",
					 "%1 < %2","%1 >= %2",
					 "%1 > %2","%1 <= %2"};
	private static final String[] DOID_METHODS_CONDITION_OP =
		new String[]{"%1 && %2","%1 || %2",
		"Compare.eq(%1,%2)","Compare.ne(%1,%2)",
		"Compare.lt(%1,%2)","Compare.ge(%1,%2)",
		"Compare.gt(%1,%2)","Compare.le(%1,%2)"};
	private static final String[] DOID_SPACES_CONDITION_OP =
		new String[]{"Compare.spaces(%0)==0","Compare.spaces(%0)!=0",
					 "Compare.spaces(%0)<0","Compare.spaces(%0)>=0",
					 "Compare.spaces(%0)>0","Compare.spaces(%0)<=0"};
	private static final int[] SINGLE_ARG_REVERSE_OP = new int[]   {1,0,3,2};
	
	private static final String[] DO_SINGLE_ARG_CONDITION_OP =
		new String[] {"Compare.isPositive(%0)","Compare.isNegative(%0)",
					  "Compare.isZero(%0)","!Compare.isZero(%0)","!(%0)"};

	public int getReverseOp(int op,boolean reverse) {
		if(op<0||op>=REVERSE_OP.length)op=0;
		return(reverse?REVERSE_OP[op]:op);
	}
	
	public String doCondition(int op,ExpressionString expr1,ExpressionString expr2,boolean doReverseCondition) {
		int l1,l2;
		if(expr1.props!=null)	l1=expr1.props.getLength();
		else l1=expr1.length;
		if(expr2.props!=null)	l2=expr2.props.getLength();
		else l2=expr2.length;
		int l = Math.max(l1,l2); 
		if(expr1.isAll) {
			expr1.literal.insert(0, "__all(").append(',').append(l).append(')');
		}
		if(expr2.isAll) {
			expr2.literal.insert(0, "__all(").append(',').append(l).append(')');
		}
		if(op<0||op>=DOID_NATIVE_CONDITION_OP.length) op = 2;//reset to ==
		if((expr1.toString().equals("\" \"")||expr2.toString().equals("\" \""))&&op>=2) {
			
			op -= 2;
			if(expr1.toString().equals("\" \""))return DOID_SPACES_CONDITION_OP[op].replace("%0",expr2.toString());
			else return DOID_SPACES_CONDITION_OP[op].replace("%0",expr1.toString());
		}
		if(op<0||op>=DOID_NATIVE_CONDITION_OP.length) op = 2;//reset to ==
/* Delete 20210624 S
		if(Math.max(expr1.type,expr2.type)<CobolSymbol.BIGDECIMAL) {
			if((expr1.type==Constants.CHAR||expr2.type==Constants.CHAR)&&!(expr1.type==Constants.CHAR&&expr2.type==Constants.CHAR)) {
				if(expr1.type==Constants.CHAR)
					return DOID_NATIVE_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
								                      .replace("%1",expr1.toString()+" - \'0\'").replace("%2",expr2.toString());
				else
					return DOID_NATIVE_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
								                      .replace("%1",expr1.toString()).replace("%2",expr2.toString()+" - \'0\'");
			}
			else
			return DOID_NATIVE_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
			                      .replace("%1",expr1.toString()).replace("%2",expr2.toString());
		} 
		else 
		{
			return DOID_METHODS_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
						                      .replace("%1",expr1.toString()).replace("%2",expr2.toString());
		}
		Delete 20210624 E */
		//Add 20210624 S
		String tmp = expr2.toString();
		if (NameUtil.isNumeric(tmp))
		{
			int idx= Integer.parseInt(tmp)-1;
			tmp=String.valueOf(idx);
		}
		if(Math.max(expr1.type,expr2.type)<CobolSymbol.BIGDECIMAL) {
			if((expr1.type==Constants.CHAR||expr2.type==Constants.CHAR)&&!(expr1.type==Constants.CHAR&&expr2.type==Constants.CHAR)) {
				if(expr1.type==Constants.CHAR)
					return DOID_NATIVE_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
								                      .replace("%1",expr1.toString()+" - \'0\'").replace("%2",tmp);
				else
					return DOID_NATIVE_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
								                      .replace("%1",expr1.toString()).replace("%2",tmp+" - \'0\'");
			}
			else
			return DOID_NATIVE_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
			                      .replace("%1",expr1.toString()).replace("%2",tmp);
		} 
		else 
		{
			return DOID_METHODS_CONDITION_OP[(doReverseCondition?REVERSE_OP[op]:op)]
						                      .replace("%1",expr1.toString()).replace("%2",tmp);
		}
		//Add 20210624 E
	}
	public ExpressionString doCondition2(int op,ExpressionString expr1,ExpressionString expr2,boolean doReverseCondition) {
		expr1.literal=new StringBuffer(doCondition(op,expr1,expr2,doReverseCondition));
		expr1.setLength(Math.max(expr1.length,expr2.length));
		return expr1;
	}
	
	public String doCondition(int op,ExpressionString expr, boolean doReverseCondition) {
		if(op<0||op>=DO_SINGLE_ARG_CONDITION_OP.length) op = 0;//reset to isPositive()
		return DO_SINGLE_ARG_CONDITION_OP[(doReverseCondition?SINGLE_ARG_REVERSE_OP[op]:op)]
			                      .replace("%0",expr.toString());
	}
	
	public ExpressionString doCondition2(int op,ExpressionString expr1,boolean doReverseCondition) {
		expr1.literal=new StringBuffer(doCondition(op,expr1,doReverseCondition));
		return expr1;
	}
	
}
