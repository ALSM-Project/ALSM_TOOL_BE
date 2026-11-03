package com.res.java.translation.engine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Stack;
import java.util.Vector;
import java.util.regex.Pattern;

import com.res.cobol.Main;
import com.res.cobol.RESNode;
import com.res.cobol.TreeToCommentFormatter;
import com.res.cobol.parser.CobolParserConstants;
import com.res.cobol.syntaxtree.*;
import com.res.cobol.visitor.DepthFirstVisitor;
import com.res.common.RESConfig;
import com.res.java.lib.CobolSymbol;
import com.res.java.lib.Constants;
import com.res.java.lib.RunTimeUtil;
import com.res.java.translation.symbol.ExpressionString;
import com.res.java.translation.symbol.SymbolConstants;
import com.res.java.translation.symbol.SymbolProperties;
import com.res.java.translation.symbol.SymbolTable;
import com.res.java.translation.symbol.SymbolUtil;
import com.res.java.util.ClassFile;
import com.res.java.util.NameUtil;

@SuppressWarnings("unused")
public class Cobol2Java extends DepthFirstVisitor {

	private static Cobol2Java thiz=null;
	private Cobol2Java() {}
	
	private  boolean doQualified = false;
	private  boolean doLiteral = false;
	private  boolean doingAcceptOption = false;
	private  boolean doingSingleParagraph=false;
	private  boolean doingNestedProgram=false;
	private  Stack<String> condition = new Stack<String>();
	public  int expressionType=0;
	public  String expressionString;
	public boolean isAll=false;
	public  ExpressionString literal = new ExpressionString();
	public  String lastTokenString=null;
	@SuppressWarnings("unchecked")
	private  Stack qualified=new Stack();
	@SuppressWarnings({ "unchecked" })
	private  Stack idProps = null;
	@SuppressWarnings("unchecked")
	private  Stack optionStack = null;
	private  SymbolProperties props = null;
	private  SymbolProperties paragraphProps = null;
	private  SymbolProperties sectionProps = null;
	private  SymbolProperties procedureProps = null;
	private  SymbolProperties procedureThruProps = null;
	private  Vector<IdOrLit> idOrLiteralList;
	private Stack<StringBuffer> aLine=new Stack<StringBuffer>();
	private Stack<StringBuffer> bLine=new Stack<StringBuffer>();
	private Stack<StringBuffer> cLine=new Stack<StringBuffer>();
	private int callParmIdx=0;
	private  String paragraphName = null;
	private  String sectionName = null;
	// QuyTV
	// taskletList using for print tasklet declare
	public static Hashtable<String,String> taskletList = new Hashtable<String, String>();
	private static ArrayList<String> sourceLst = new ArrayList<>();
	private static ArrayList<String> sourceLstFirst = new ArrayList<>();
	private static String strMainFunction=null;
	
	class IdOrLit {
		SymbolProperties id=null;
		ExpressionString literal=null;
		boolean rounded=false;
		IdOrLit(SymbolProperties sym) {
			id=sym;
		}
		IdOrLit(SymbolProperties sym,boolean rnd) {
			id=sym;rounded=rnd;
		}
		IdOrLit(ExpressionString lit) {
			literal=lit;
		}
		IdOrLit(String lit) {
			literal=new ExpressionString(lit);
		}
		SymbolProperties Id() {return id;}
		String Lit() { return literal.toString();}
		String get() {
			if(id!=null)return new ExpressionString(id).toString();
			else if(literal!=null) return formatLiteral(literal).toString();
			return null;
		}
		boolean isRounded() {return rounded;}
	}

	@SuppressWarnings("serial")
	public class ExpressionStack extends Stack<ExpressionString> {

		@Override
		public boolean empty() {
			return super.empty();
		}

		@Override
		public synchronized ExpressionString peek() {
			return super.peek();
		}

		public synchronized String peek3() {
			return super.peek().toString();
		}
		@Override
		public synchronized ExpressionString pop() {
			return super.pop();
		}
		public synchronized String pop3() {
			return super.pop().toString();
		}
		public ExpressionString push(String item) {
			return super.push(new ExpressionString(item,expressionType));
		}
		@Override
		public ExpressionString push(ExpressionString item) {
			return super.push(item);
		}
		public ExpressionString push(SymbolProperties item) {
			return super.push(new ExpressionString(item));
		}
		
		public boolean isAll() {
			return super.peek().isAll;
		}
		public void setAll(boolean b) {
			super.peek().isAll=b;
		}
		public int getType() {
			return super.peek().type;
		}
		public void setType(int b) {
			super.peek().type=b;
		}
	}

	public ExpressionStack expression = new ExpressionStack();
	@Override
	public void visit(CompilationUnit n) {
		super.visit(n);
	}

	@Override
	public void visit(NestedProgramUnit n) {
		doingNestedProgram=true;
		n.nestedIdentificationDivision.accept(this);
		n.nodeOptional1.accept(this);
		n.nodeOptional.accept(this);
		n.nodeOptional2.accept(this);
		n.nodeListOptional.accept(this);
		ClassFile.endProgramScope();
		dumper.printCobolComments(n.endProgramStatement.line);
		SymbolTable.getScope().endProgram();
		doingNestedProgram=false;
	}

	@Override
	public void visit(ProgramUnit n) {
		SymbolTable.getScope().setCloneOnLookup(true);
		n.identificationDivision.accept(this);
		n.nodeOptional1.accept(this);
		n.nodeOptional.accept(this);
		n.nodeOptional2.accept(this);
		dumper.printCobolComments(9999999);
		if(!SymbolTable.getScope().isCurrentProgramTopLevel()) {
			ClassFile.endProgramScope();
			SymbolTable.getScope().endProgram();
		}
		SymbolTable.getScope().setCloneOnLookup(false);
	}
	private boolean isDeclarative =false;
	@Override
	public void visit(Declaratives n) {
		isDeclarative=true;
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			NodeSequence nodeseq = (NodeSequence) e.nextElement();
			((SectionHeader)nodeseq.elementAt(0)).sectionName.accept(this);
			nodeseq.elementAt(2).accept(this);
			/* Update 20210623
			sourceLst.add("Section "+sectionProps.getJavaName1()+"=new Section(this,true) {");
			sourceLst.add("\tpublic CobolMethod run() {");
			sourceLst.add("tab()");
			sourceLst.add("return super.run();");
			sourceLst.add("backTab()");
			sourceLst.add("}};");
			*/
			nodeseq.elementAt(4).accept(this);
		}
		isDeclarative=false;
	}

	class UseCondition {
		boolean isError=false;
		String openCondition=null;
		public UseCondition(boolean e,String o,SymbolProperties sec) {
			isError=e;openCondition=o;section=sec;
		}
		SymbolProperties section = null;
	}
	private UseCondition useCondition = null;
	private ArrayList<UseCondition> useConditions = null;
	
	@Override
	public void visit(UseStatement n) {
		if(useConditions==null)
			useConditions=new ArrayList<UseCondition>();
		if(sectionProps==null) return;
		if(n.nodeChoice.which==1) {
			NodeSequence nodeseq=(NodeSequence)n.nodeChoice.choice;
			NodeChoice nodech=(NodeChoice)nodeseq.elementAt(6);
			switch(nodech.which) {
			case 0:
				NodeList filelist =(NodeList)nodech.choice;
				for(Enumeration<Node> e=filelist.elements();e.hasMoreElements();) {
					e.nextElement().accept(this);
					if(props==null) continue;
					useConditions.add(new UseCondition(true,props.getDataName(),sectionProps));
				}
				break;
			case 1:
			case 2:
			case 3:
			case 4:
				useConditions.add(new UseCondition(true,String.valueOf(nodech.which),sectionProps));
			}
		}
	}

	@Override
	public void visit(CurrencySignClause n) {
		n.literal.accept(this);
		Main.getContext().setCurrencySign(formatLiteral(literal).toString());
	}

	@Override
	public void visit(DecimalPointClause n) {
		Main.getContext().setDecimalPointIsComma(true);
	}
	
	private ArrayList<Node> delayedStatements=null;
	@Override
	public void visit(DataDescriptionEntry n) {
	
		if(n.nodeChoice.which==5) {
			NodeChoice nodech = (NodeChoice) ((NodeSequence)n.nodeChoice.choice).elementAt(2);
			if(nodech.which==3) {
				//DECLARE CURSOR
				if(delayedStatements==null) 
					delayedStatements = new ArrayList<Node>();
				delayedStatements.add(n);
			}
		}
		//super.visit(n);
	}


	@Override
	public void visit(ProcedureDivision n) {

		if(SymbolTable.getScope().getCurrentProgram()==null)
			return;
		aLine.push(new StringBuffer());
		ClassFile.doProgramScope(SymbolTable.getScope().getCurrentProgram(),false);
		//sourceLst.add("tab()");
		//Create Symbols and Accessors.
		SymbolTable.convert2Java();
		String className = (String) SymbolTable.getScope().getCurrentProgram().getJavaName2();
		/*
		if (SymbolTable.getScope().isCurrentProgramTopLevel()) {
			sourceLst.add("@Override");
			sourceLst.add("public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {");
			sourceLst.add("   try {");
			sourceLst.add("      // TODO");
			sourceLst.add("   ");
			sourceLst.add("   } catch (Exception e) {");
			sourceLst.add("      return RepeatStatus.FINISHED;");
			sourceLst.add("   }");
			sourceLst.add("   return RepeatStatus.FINISHED;");
			sourceLst.add("tab()");
			*/
			/*
			sourceLst.add("__processCmdLineArgs(args);");
			sourceLst.add(className+" instance_ = new "+className+"();");
	        sourceLst.add("instance_.execute(null); "); 
	        sourceLst.add("System.exit(instance_.__getReturnCode());");
			*/
		/*
			sourceLst.add("backTab()");
			//ClassFile.endMethodScope();
			
		}*/
		/*
		sourceLst.add("");
		sourceLst.add("init();");
		sourceLst.add("while (true) {");
		sourceLst.add("tab()");
		*/

		//sourceLst.add("public void execute(ProgramEnv __env) {");
		//sourceLst.add("tab()");
        //sourceLst.add("initialize(this); " );
		/* Delete 20210827 S
		if(n.nodeOptional.present()) {
			NodeList nodelist = ((UsingArgs)((NodeSequence)n.nodeOptional.node).elementAt(1)).nodeList;
			processUsingArgs1(nodelist);
		} 
		 Delete 20210827 E */
        if(delayedStatements!=null)
        	for(Node nd:delayedStatements) {
        		nd.accept(execSql2Java);
        	}
        if(n.procedureBody.paragraphs.nodeListOptional.present()) 
			n.procedureBody.paragraphs.nodeListOptional.accept(this);
      //  sourceLst.add("doCobolGotoStart();");
        /* Delete 20210827 S
		if(n.nodeOptional.present()) {
			NodeList nodelist = ((UsingArgs)((NodeSequence)n.nodeOptional.node).elementAt(1)).nodeList;
			processUsingArgs2(nodelist);
		}
		Delete 20210827 E */
		//sourceLst.add("backTab()");
		//ClassFile.endMethodScope();

		aLine.pop();
		if(n.nodeOptional1.present()) {
			dumper.startAtNextToken();
			dumper.visit(n.nodeOptional1);
			n.nodeOptional1.accept(this);
		}
		if(n.procedureBody.paragraphs.nodeListOptional1.present()) 
			n.procedureBody.paragraphs.nodeListOptional1.accept(this);
		if(n.procedureBody.nodeListOptional.present()) {
			n.procedureBody.nodeListOptional.accept(this);
		} 
		doTail();
		
	}

	private void processUsingArgs2(NodeList nodelist) {
		aLine.peek().setLength(0);bLine.push(new StringBuffer());
		for(Enumeration<Node> e=nodelist.elements();e.hasMoreElements();) {
			e.nextElement().accept(this);
			if(aLine.peek().length()>0) aLine.peek().append(',');
			if(props!=null){
				if(props.getIsFormat())
					line=NameUtil.getFormatName2(props, false).replace("%0",NameUtil.getJavaName(props,false));
				else
					line=new ExpressionString(props).toString();
				aLine.peek().append(line);
			}
		}
		aLine.peek().append(bLine.peek());
		sourceLst.add(aLine.peek().insert(0,"doCobolReturn(").append(");").toString());
		bLine.pop();
	}

	private void processUsingArgs1(NodeList nodelist) {
		sourceLst.add("__initializeCall(__env," +String.valueOf(nodelist.size())+
				");");
		int i=0;
		for(Enumeration<Node> e=nodelist.elements();e.hasMoreElements();) {
			e.nextElement().accept(this);
			if(props!=null){
				if(props.isAChildHasDependingOn()||(props.isOccurs()&&props.getDependingOnOccurs()!=null))
					aLine.peek().append(ClassFile.current.getTabs()).append(NameUtil.getJavaName(props, true).replace("%0",
							("__get"+SymbolConstants.getSQL(props.getJavaType().type)+"Arg("+
									String.valueOf(i++)+((props.getJavaType().type>=CobolSymbol.STRING)?
									(","+String.valueOf(props.getLength())):"")+")"))+';');
				else
				sourceLst.add(NameUtil.getJavaName(props, true).replace("%0",
						("__get"+SymbolConstants.getSQL(props.getJavaType().type)+"Arg("+
								String.valueOf(i++)+((props.getJavaType().type>=CobolSymbol.STRING)?
								(","+String.valueOf(props.getLength())):"")+")"))+';');
			}
		}
		sourceLst.add(aLine.peek().toString());
	}

	@Override
	public void visit(ExitProgramStatement n) {
		isLastExitStatement=true;
		//20210707 Update S
		//sourceLst.add("return null;");
		sourceLst.add("return ;");
		//20210707 Update E		
		super.visit(n);
	}
	@Override
	public void visit(StopStatement n) {
		//super.visit(n);
		/* Detete 20210828 S
		sourceLst.add("System.exit(super.__getReturnCode());");
		 Detete 20210828 E */
	}

	@Override
	public void visit(MoveStatement n) {
		
		switch(n.nodeChoice.which) {
		case 0:
			NodeSequence nodeseq = (NodeSequence) n.nodeChoice.choice;
			if(nodeseq.size()!=3)
				return;
			NodeChoice choice2 =(NodeChoice)nodeseq.elementAt(0);;
			choice2.accept(this);
			Object from=null;
			if(choice2.which==0) {
				from=props;
			} else {
				from=literal;
			}
			int saveType=expressionType;
			if(nodeseq.elementAt(2) instanceof NodeList) {
				for(Enumeration<Node> e=((NodeList)nodeseq.elementAt(2)).elements();
					e.hasMoreElements();) {
					Node node = e.nextElement();
					node.accept(this);
					doMove(from,props);
					expressionType=saveType;
				}
			}
			break;
		case 1:
			nodeseq = (NodeSequence) n.nodeChoice.choice;
			nodeseq.elementAt(1).accept(this);
			from=props;
			if(nodeseq.elementAt(3) instanceof NodeList) {
				for(Enumeration<Node> e=((NodeList)nodeseq.elementAt(3)).elements();
					e.hasMoreElements();) {
					Node node = e.nextElement();
					node.accept(this);
					doCorrespondingMove((SymbolProperties)from,props,true);
				}
			}
		}
	}
	
	//private  SimplifyExpression simply=new SimplifyExpression();
	
	private String simplify(String expr) {
		return expr;//simply.simplify(expr);
	}
	
	private void doMove(Object from,SymbolProperties to)
	{
		boolean isSourceANumber=false,isAllLiteral=false;SymbolProperties propsFrom=null;
		
		if (from instanceof SymbolProperties) {
			propsFrom=(SymbolProperties)from;
			expression.push(propsFrom);
			expressionType=propsFrom.getJavaType().type;
			isSourceANumber=(propsFrom.getJavaType().type<=CobolSymbol.BIGDECIMAL);
			sourceLst.add(TranslationTable.getInstance().getAssignString(to, propsFrom, isAllLiteral,true,true));
		} else 
			if (from instanceof ExpressionString){
				literal=formatLiteral(((ExpressionString)from));
				isAllLiteral=literal.isAll;
				expression.push(literal);
				isSourceANumber=(expressionType<=CobolSymbol.BIGDECIMAL);
				sourceLst.add(TranslationTable.getInstance().getAssignString(to, null, isAllLiteral,true,true));
			} 
		expression.clear();initExpressionType();
		return;
	}
	
	private boolean doCorrespondingMove(SymbolProperties from, SymbolProperties to,boolean first) {
		if(first) {
			if(!isCorrespondingValid2(from,to)) return false;
		}
		else if(!isCorrespondingValid(from,to)) return false;
		if(from.getPictureString()!=null||to.getPictureString()!=null) {
			doMove(from,to);
			return true;
		} else {
			if(from.getChildren()!=null&&to.getChildren()!=null) {
				for(int i=0;i<from.getChildren().size();++i) {
					for(int j=0;j<to.getChildren().size();++j) {
						from.getChildren().get(i).setIndexesWorkSpace(from.getIndexesWorkSpace());
						to.getChildren().get(j).setIndexesWorkSpace(to.getIndexesWorkSpace());
						if(doCorrespondingMove(from.getChildren().get(i),to.getChildren().get(j),false)) break;
					}
				}
			}
			return false;
		}
	}
	
	public boolean isCorrespondingValid(SymbolProperties corrF,SymbolProperties corrT) {
		if(!corrF.getDataName().equalsIgnoreCase(corrT.getDataName())) return false;
		if(corrF==null||corrF.isOccurs()||corrF.getIsFiller()||corrF.getJavaType()==null||
				corrF.getJavaType().type>Constants.GROUP||
				corrF.getRedefines()!=null||corrF.hasRenames())
			return false;
		if(corrT==null||corrT.isOccurs()||corrT.getIsFiller()||corrT.getJavaType()==null||
				corrT.getJavaType().type>Constants.GROUP||
				corrT.getRedefines()!=null||corrT.hasRenames())
			return false;
		return true;
	}
	
	public boolean isCorrespondingValid2(SymbolProperties corrF,SymbolProperties corrT) {
		if(corrF==null||corrF.getLevelNumber()==66||
				corrF.getLevelNumber()==77||corrF.getLevelNumber()==88||
				corrF.getJavaType()==null||corrF.getJavaType().type>Constants.GROUP)
			return false;
		if(corrT==null||corrT.getLevelNumber()==66||
				corrT.getLevelNumber()==77||corrT.getLevelNumber()==88||
				corrT.getJavaType()==null||corrT.getJavaType().type>Constants.GROUP)
			return false;
		return true;
	}
	
	private   final Pattern HEX_COBOL_PATTERN=Pattern.compile("[xX]\\\'[0-9a-fA-F]+\\\'|[xX]\\\"[0-9a-fA-F]+\\\"");
	private  final Pattern HEX_PATTERN=Pattern.compile("0[xX][0-9a-fA-F]+|0[xX][0-9a-fA-F]+");
	private  final Pattern INT_PATTERN=Pattern.compile("(\\+|\\-)?(\\d)+(\\,(\\d)+)*");
	private  final Pattern DEC_PATTERN=Pattern.compile("(\\+|\\-)?(\\d)*(\\.[\\d]+)");
	
	private  String line="";
	public  ExpressionString formatLiteral(String lit) {
		literal=new ExpressionString(lit);
		return formatLiteral(literal);
	}
	public  ExpressionString formatLiteral(ExpressionString litC) {

		int j=0;boolean isFunction=false;
		String lit=litC.toString();
		
		if(lit.equals("\"\\\"\""))//tell me why this?
			expressionType=CobolSymbol.STRING;
		else
		if((lit.indexOf('\"')==0&&(j=lit.lastIndexOf('\"'))>=0)||
				lit.indexOf('\'')==0&&
				(j=lit.lastIndexOf('\''))>=0) {
			lit='\"'+lit.substring(1,j).replace("\"\"", "\"").
				replace("\\", "\\\\").
				replace("\"", "\\\"").replace("\'", "\\\"")+'\"';
			expressionType=CobolSymbol.STRING;
		} else 
			if(HEX_COBOL_PATTERN.matcher(lit).matches()) {
				if(!doingArithmeticExpression) {
					lit="Integer.toHexString(0"+lit.replace("\'", "").replace("\"","").replace('X','x')+')';
					expressionType=CobolSymbol.STRING;
				} else {
					lit="0"+lit.replace("\'", "").replace("\"","").replace('X','x');
					expressionType=CobolSymbol.INTEGER;
				}
			}else
				if(HEX_PATTERN.matcher(lit).matches()) {
					if(!doingArithmeticExpression) {
						lit="Integer.toHexString("+lit+')';
						expressionType=CobolSymbol.STRING;
					} else {
						expressionType=CobolSymbol.INTEGER;
					}
				} else 
					if(INT_PATTERN.matcher(lit).matches()) {
						lit=stripLeadingZeros(lit);
						expressionType=CobolSymbol.INTEGER;
						if(Long.parseLong(lit)>Integer.MAX_VALUE||Long.parseLong(lit)<Integer.MIN_VALUE) {
							lit+="L";//tell me why this?
						}
					} else 
						if(DEC_PATTERN.matcher(lit).matches()) {
							lit=stripLeadingZeros(lit);
							expressionType=CobolSymbol.DOUBLE;
						} else
							if(lit.equalsIgnoreCase("ZERO")||lit.equalsIgnoreCase("ZEROS")||lit.equalsIgnoreCase("ZEROES")) {
								lit="0";
								expressionType=CobolSymbol.INTEGER;
							} else 
								if(lit.equalsIgnoreCase("SPACE")||lit.equalsIgnoreCase("SPACES")) {
									lit="\" \"";
									expressionType=CobolSymbol.STRING;
								}
								else
									if(lit.equalsIgnoreCase("HIGH-VALUE")||lit.equalsIgnoreCase("HIGH-VALUES")) {
										lit="-1";
										expressionType=CobolSymbol.INTEGER;
									}
									else
										if(lit.equalsIgnoreCase("LOW-VALUE")||lit.equalsIgnoreCase("LOW-VALUES")) {
											lit="0";
											expressionType=CobolSymbol.INTEGER;
										}
										else
											if(lit.equalsIgnoreCase("QUOTE")||lit.equalsIgnoreCase("QUOTES")) {
												lit="\"\\\"\"";
												expressionType=CobolSymbol.STRING;
											}
											else
												if(litC.literal.indexOf("__function__")==0) {
													isFunction=true;
													int idx=Integer.parseInt(litC.literal.substring(12, litC.literal.indexOf("__",12)));
													//Update 20210625 S
													//litC.literal=litC.literal.replace(0,litC.literal.indexOf("__",12)+2,SymbolConstants.getFunction(idx)).insert(0,"__function().");
													litC.literal =litC.literal.replace(0, litC.literal.length(), SymbolConstants.getFunction(idx));
													//Update 20210625 E
													expressionType=Math.max(litC.type,expressionType);
													litC.type=expressionType;
													return litC;
												}
												else
												{ 
													expressionType=litC.type;
													return litC;
												}
		litC.type=expressionType;
		litC.literal=new StringBuffer(lit);
		if(RESConfig.getInstance().isEbcdicMachine()&&expressionType==CobolSymbol.STRING&&!isFunction){
			litC.literal.insert(0, "__normalizeLiteral(").append(')');
		}
		return litC;
	}

	//Input is a valid number literal
	private  String stripLeadingZeros(String lit) {

		lit=lit.replace(",", "");
		
		if(ZERO_PATTERN.matcher(lit).matches())	return "0";
		
		int i=0;int sign=0;
		if(lit.charAt(0)=='+')
			i=1;
		else
			if(lit.charAt(0)=='-') {
				i=1;sign=-1;
			}
		
		do {
			if(lit.charAt(i)!='0') break;
			++i;
		}while(i<lit.length());
		
		lit=lit.substring(i);

		if(lit.charAt(0)=='.')lit='0'+lit;
		
		if(sign<0)lit='-'+lit;
		
		return lit;
		
	}
	
	@Override
	public void visit(DisplayStatement n) {
		line="";
		for(Enumeration<Node> e = n.nodeList.elements(); e.hasMoreElements();) {
			Node node = e.nextElement();
			NodeSequence nodeseq=(NodeSequence)node;
			NodeChoice nodechoice = (NodeChoice) nodeseq.elementAt(0);
			nodechoice.accept(this);
			switch(nodechoice.which) {
			case 0:
				if(props.getJavaType().type==CobolSymbol.STRING) {
					//20210712 Update S
					//line+="getString("+new ExpressionString(props)+','+String.valueOf(props.getLength())+")";
					line+=new ExpressionString(props) ;
					//20210712 Update E
				} else 
					if(props.getJavaType().type==CobolSymbol.STRING) {
						line+="getBytes("+new ExpressionString(props)+','+String.valueOf(props.getLength())+")";
					} else
				if(props.getIsFormat()) {
					//20210707 Update for error Display S
					//line+=NameUtil.getFormatName2(props,false).replace("%0",
					//new ExpressionString(props).toString());
					line+= new ExpressionString(props).toString();					
					//20210707 Update for error Display E					
				} else 
				if(props.getPictureString()==null&&props.getLevelNumber()==1){
					String temp=new ExpressionString(props).toString();
					line+=temp.substring(0,temp.lastIndexOf('.')+1)+props.getJavaName1()+".toString()";
				} else {
					line+=new ExpressionString(props).toString();
				}
				break;
			case 1:
				line+=formatLiteral(literal);
				break;
			default:
			}
			if(e.hasMoreElements())
				line+="+";
		}
		if(n.nodeOptional.present()) {
			n.nodeOptional.node.accept(this);
			if(!lastTokenString.equalsIgnoreCase("CONSOLE")) {
				sourceLst.add("__putEnvironment("+formatLiteral(
						new ExpressionString('\"'+RunTimeUtil.getInstance().stripQuotes(lastTokenString)+'\"'
						))+","+line+");");
				return;
			}
		}
		if(!n.nodeOptional1.present()){
			sourceLst.add("Console.println("+line+");");
		} else {
			sourceLst.add("Console.print("+line+");");
		}
	}

	// QuyTV
	// handling EXEC CICS Statement

	@Override
	public void visit(ExecCicsStatement n) {
		sourceLst.add("//TODO CICS");
		switch (n.nodeChoice1.which) {
		case 0:
			sourceLst.add("//TODO Send Cics");
			break;
		case 1:
			sourceLst.add("//TODO Send Text Cics");
			break;
		case 2:
			sourceLst.add("//TODO Send Control Cics");
			break;
		case 3:
			sourceLst.add("//TODO ReadQ Cics");
			break;
		case 4:
			sourceLst.add("//TODO WriteQ Cics");
			break;
		case 5:
			sourceLst.add("//TODO DeleteQ Cics");
			break;
		case 6:
			sourceLst.add("//TODO Return Cics");
			break;
		case 7:
			sourceLst.add("//TODO Link Cics");
			break;
		case 8:
			sourceLst.add("//TODO Load Cics");
			break;
		case 9:
			sourceLst.add("//TODO Read File Cics");
			break;
		case 10:
			sourceLst.add("//TODO Read Prev File Cics");
			break;
		case 11:
			sourceLst.add("//TODO Delete File Cics");
			break;
		case 12:
			sourceLst.add("//TODO Receive Map Cics");
			break;
		case 13:
			sourceLst.add("//TODO Inquire Cics");
			break;
		case 14:
			sourceLst.add("//TODO EndBR Cics");
			break;
		case 15:
			sourceLst.add("//TODO Handle Cics");
			break;
		case 16:
			sourceLst.add("//TODO Xctl Cics");
			break;
		case 17:
			sourceLst.add("//TODO Ignore Cics");
			break;			
		default:
			break;
		}
	}

	private  void initExpressionType() {
		//TODO to isolate the inits may be removed later
		expressionType=CobolSymbol.BYTE;
	}
	
	private  void initFunctionExpressionType() {
		//TODO to isolate the inits may be removed later
		expressionType=CobolSymbol.DOUBLE;
	}
	
	
	@Override
	public void visit(AddStatement n) {
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doTry();
		n.addBody.accept(this);
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doCatchArith(n.nodeOptional,n.nodeOptional1);
	}

	@Override
	public void visit(AddBody n) {
		initExpressionType();
		expression.clear();
		switch(n.nodeChoice.which) {
		case 1://Format 2
			NodeSequence nodeseq = (NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(0).accept(this);
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o = e.nextElement();
				if(o.Id()!=null) {
				//Identifier
					doIdOp(o.Id(),"+");
				} else { 
					doLiteralOp(o.Lit(),"+");
				}
			}
			if(idOrLiteralList.size()<1)
				break;
			if(idOrLiteralList.size()>1) {
				//Create a temporary variable
				String assign=TranslationTable.getInstance().getAssignString( null, null,false);
				sourceLst.add(assign);
				expression.push("temp"+String.valueOf(SymbolTable.tempVariablesMark-1));
			} else {
				//Here expression is on the stack
			}
			nodeseq.elementAt(2).accept(this);
			String top=expression.pop3();
			int topType=expressionType;
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o =e.nextElement();
				if(o.isRounded()) {
					expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
				}
				else expression.push(top);
				expressionType=topType;
				doIdOp(o.Id(),"+");
				String assign=TranslationTable.getInstance().getAssignString(o.Id(), null,false);
				sourceLst.add(assign);
			}
			break;
		case 0://Format 1
			nodeseq = (NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(0).accept(this);
			nodeseq.elementAt(1).accept(this);
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o = e.nextElement();
				if(o.Id()!=null) {
					doIdOp(o.Id(),"+");
				} else {
					doLiteralOp(o.Lit(),"+");
				}
			}
			if(idOrLiteralList.size()>1) {
				//Create a temporary variable
				String assign=TranslationTable.getInstance().getAssignString(null, null,false);
				sourceLst.add(assign);
				expression.push("temp"+new Integer(SymbolTable.tempVariablesMark-1).toString().trim());
			} else {
				//Here expression is on the stack
			}
			nodeseq.elementAt(3).accept(this);
			top=expression.pop3();
			topType=expressionType;
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o =e.nextElement();
				if(o.isRounded()) {
					expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
				}
				else expression.push(top);
				expressionType=topType;
				String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
				sourceLst.add(assign);
			}	
			break;
		case 2://Format 3
			nodeseq = (NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(1).accept(this);
			SymbolProperties from = props;
			nodeseq.elementAt(3).accept(this);
			doCorrespondingOp(from,props,"+",false,true);
		}
		//super.visit(n);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(MultiplyBody n) {
		initExpressionType();
		expression.clear();
		idOrLiteralList=new Vector<IdOrLit>();
		n.idOrLiteral.accept(this);
		switch(n.nodeChoice.which) {
		case 0://Format 1
			NodeSequence nodeseq = (NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(0).accept(this);
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o = e.nextElement();
				if(o.Id()!=null) {
					doIdOp(o.Id(),"*");
				} else {
					doLiteralOp(o.Lit(),"*");
				}
			}
			idOrLiteralList=new Vector();
			nodeseq.elementAt(2).accept(this);
			if(idOrLiteralList.size()>1) {
				//Create a temporary variable
				String assign=TranslationTable.getInstance().getAssignString( null, null,false);
				sourceLst.add(assign);
				expression.push("temp"+new Integer(SymbolTable.tempVariablesMark-1).toString().trim());
			} else {
				//Here expression is on the stack
			}
			String top=expression.pop3();
			int topType=expressionType;
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o =e.nextElement();
				if(o.isRounded()) {
					expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
				}
				else expression.push(top);
				expressionType=topType;
				String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
				sourceLst.add(assign);
			}	
			break;
		case 1:
			if(idOrLiteralList.size()!=1)
				break;
			IdOrLit o = idOrLiteralList.elementAt(0);
			if(o.Id()!=null) {
				doIdOp(o.Id(),"*");
			} else{
				doLiteralOp(o.Lit(),"*");
			}
			idOrLiteralList=new Vector();
			n.nodeChoice.choice.accept(this);
			top=expression.pop3();
			topType=expressionType;
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				o =e.nextElement();
				if(o.isRounded()) {
					expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
				}
				else expression.push(top);
				expressionType=topType;
				doIdOp(o.Id(),"*");
				String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
				sourceLst.add(assign);
			}
			break;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(SubtractStatement n) {

		initExpressionType();
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doTry();

		expression.clear();
		idOrLiteralList=new Vector();
		switch(n.nodeChoice.which) {
		case 0:
			NodeSequence nodeseq = (NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(0).accept(this);
			for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
				IdOrLit o = e.nextElement();
				if(o.Id()!=null) {
					doIdOp(o.Id(),"+");
				} else {
						doLiteralOp(o.Lit(),"+");
					}
			}
			if(idOrLiteralList.size()>1) {
				//Create a temporary variable
				String assign=TranslationTable.getInstance().getAssignString( null, null,false);
				sourceLst.add(assign);
				expression.push("temp"+new Integer(SymbolTable.tempVariablesMark-1).toString().trim());
			} else {
				//Here expression is on the stack
			}
			NodeChoice nodech = (NodeChoice)nodeseq.elementAt(2);
			switch(nodech.which) {
			case 0:
				idOrLiteralList=new Vector<IdOrLit>();
				NodeSequence nodeseq2 = (NodeSequence)nodech.choice;
				nodeseq2.elementAt(0).accept(this);
				String top=expression.pop3();
				int topType=expressionType;
				if(idOrLiteralList.elementAt(0).Id()!=null) {
					TranslationTable.getInstance().doOp(idOrLiteralList.elementAt(0).Id(),"-");
					TranslationTable.getInstance().doOp(top,topType,"-");	
				} else 
					if(idOrLiteralList.elementAt(0).Lit()!=null) {
						TranslationTable.getInstance().doOp
							(idOrLiteralList.elementAt(0).Lit(),"-");
						TranslationTable.getInstance().doOp(top,topType,"-");
					}
				idOrLiteralList=new Vector<IdOrLit>();
				nodeseq2.elementAt(2).accept(this);
				top=expression.pop3();
				topType=expressionType;
				for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
					IdOrLit o=e.nextElement();
					if(o.isRounded()) {
						expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
					}
					else expression.push(top);
					expressionType=topType;
					String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
					sourceLst.add(assign);
				}
				break;
			case 1:
				idOrLiteralList=new Vector();
				nodech.choice.accept(this);
				 top=expression.pop3();
				 topType=expressionType;
				for(Enumeration<IdOrLit> e=idOrLiteralList.elements();e.hasMoreElements();) {
					IdOrLit o=e.nextElement();
					/*
					if(o.isRounded()) {
						expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
					}
					else expression.push(top);
					*/
					expression.push(NameUtil.getJavaName(o.Id(), false));
					expressionType=o.Id().getJavaType().type;
					//doIdOp(o.Id(),"-");
					TranslationTable.getInstance().doOp(top,topType,"-");
					String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
					sourceLst.add(assign);
				}
				break;
			}
			break;
		case 1:
			nodeseq = (NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(1).accept(this);
			SymbolProperties from = props;
			nodeseq.elementAt(3).accept(this);
			doCorrespondingOp(from,props,"-",true,true);
		}
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doCatchArith(n.nodeOptional,n.nodeOptional1);
	}

	private void doTry() {
		sourceLst.add("try {");
		sourceLst.add("tab()");
		// QuyTV
		/* sourceLst.add("__reset();"); */
	}
	
	private void doCatchArith(Node n,NodeOptional n2) {
		sourceLst.add("__assertException();");
		n2.accept(this);
		sourceLst.add("backTab()");
		sourceLst.add("} catch(ArithmeticException ae) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ae");
		n.accept(this);
		sourceLst.add("backTab()");
		sourceLst.add("}");
	}

	private void exceptionPrintStackTrace(String e) {
		//if (RESConfig.getInstance().isExceptionPrintStackTraceOn())
			// QuyTV
			// sourceLst.add("__printStackTrace(" + e + ");");
	}
	
	
	@Override
	public void visit(MultiplyStatement n) {
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doTry();
		n.multiplyBody.accept(this);
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doCatchArith(n.nodeOptional,n.nodeOptional1);
	}
	

	@SuppressWarnings("unchecked")
	@Override
	public void visit(DivideBody n) {

		initExpressionType();
		expression.clear();
		idOrLiteralList=new Vector();
		switch(n.nodeChoice.which) {
		case 0:
			NodeSequence nodeseq=(NodeSequence)n.nodeChoice.choice;
			if(!((NodeOptional)nodeseq.elementAt(3)).present()) {
				nodeseq.elementAt(0).accept(this);
				if(idOrLiteralList.elementAt(0).Id()!=null) {
					doIdOp(idOrLiteralList.elementAt(0).Id(),"/");
				} else {
					doLiteralOp(idOrLiteralList.elementAt(0).Lit(),"/");
				}
				idOrLiteralList=new Vector();
				nodeseq.elementAt(2).accept(this);
				String top=expression.pop3();//from elementAt(0)
				int topType=expressionType;
				expression.clear();
				for(int i=0;i<idOrLiteralList.size();++i) {
					IdOrLit o=idOrLiteralList.get(i);
					if (o.Id()!=null) {
						if(o.isRounded()) {
							expression.push("__round("+top+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
						}
						else
							expression.push(top);
						expressionType=topType;
						doIdOp(o.Id(),"/");
						TranslationTable.getInstance().doOp(top,topType,"/");
						String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
						sourceLst.add(assign);
					}
				}
			} else {//Giving present
				NodeSequence nodeseq2=(NodeSequence)((NodeOptional)nodeseq.elementAt(3)).node;
				nodeseq.elementAt(0).accept(this);
				if(idOrLiteralList.size()==1) {
					if(idOrLiteralList.elementAt(0).Id()!=null) {
						doIdOp(idOrLiteralList.elementAt(0).Id(),"/");
					} else {
						doLiteralOp(idOrLiteralList.elementAt(0).Lit(),"/");
					}
				}
				String top2=expression.peek3();//These two for remainder calculation
				int type2=expressionType;
				idOrLiteralList.clear();
				nodeseq.elementAt(2).accept(this);
				expression.clear();
				if(idOrLiteralList.elementAt(0).Id()!=null) {
					doIdOp(idOrLiteralList.elementAt(0).Id(),"/");
				} else {
					doLiteralOp(idOrLiteralList.elementAt(0).Lit(),"/");
				}
				String top=expression.peek3();//from elementAt(2)
				int topType=expressionType;
				TranslationTable.getInstance().doOp(top2,type2,"/");
				idOrLiteralList.clear();
				nodeseq2.elementAt(1).accept(this);
				String top3=expression.pop3();
				int type3=expressionType;
				if (((NodeOptional)nodeseq2.elementAt(2)).present()) {//Also Remainder
					IdOrLit o=idOrLiteralList.get(0);
					if (o.Id()!=null) {
						if(o.isRounded()){
							expression.push("__round("+top3+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
						}
						else expression.push(top3);
						expressionType=type3;
						String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
						sourceLst.add(assign);

					}
					if(idOrLiteralList.size()>1){
						//TODO Syntax Error to be caught
						return;
					}
					expression.clear();
					expression.push(top);
					expressionType=topType;
					TranslationTable.getInstance().doOp(top2,type2,"%");
					idOrLiteralList.clear();
					nodeseq2.elementAt(2).accept(this);
					if(idOrLiteralList.size()==1) {
						String assign=TranslationTable.getInstance().getAssignString( idOrLiteralList.elementAt(0).Id(),
								null,false);
						sourceLst.add(assign);
					}
				} else {//Only Giving
					for(int i=0;i<idOrLiteralList.size();++i) {
						IdOrLit o=idOrLiteralList.get(i);
						if (o.Id()!=null) {
							if(o.isRounded()) {
								expression.push("__round("+top3+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
							}
							else expression.push(top3);
							expressionType=type3;
							String assign=TranslationTable.getInstance().getAssignString(o.Id(),
									null,false);
							sourceLst.add(assign);
	
						}
					}
				}
			}
			break;
		case 1:
			nodeseq=(NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(2).accept(this);
			if(idOrLiteralList.size()==1) {
				if(idOrLiteralList.elementAt(0).Id()!=null) {
					doIdOp(idOrLiteralList.elementAt(0).Id(),"/");
				} else {
					doLiteralOp(idOrLiteralList.elementAt(0).Lit(),"/");
				}
			}
			String top2=expression.peek3();//These two for remainder calculation
			int type2=expressionType;
			idOrLiteralList.clear();
			nodeseq.elementAt(0).accept(this);
			expression.clear();
			if(idOrLiteralList.size()==1) {
				if(idOrLiteralList.elementAt(0).Id()!=null) {
					doIdOp(idOrLiteralList.elementAt(0).Id(),"/");
				} else {
					doLiteralOp(idOrLiteralList.elementAt(0).Lit(),"/");
				}
			}
			String top=expression.peek3();//from elementAt(0)
			//int type1=expressionType;
			TranslationTable.getInstance().doOp(top2,type2,"/");
			idOrLiteralList.clear();
			nodeseq.elementAt(4).accept(this);
			String top3=expression.pop3();
			int type3=expressionType;
			if (((NodeOptional)nodeseq.elementAt(5)).present()) {//Also Remainder
				IdOrLit o=idOrLiteralList.get(0);
				if (o.Id()!=null) {
					if(o.isRounded()){
						expression.push("__round("+top3+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
					}
					else expression.push(top3);
					expressionType=type3;
					String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
					sourceLst.add(assign);

				}
				if(idOrLiteralList.size()>1){
					return;
				}
				expression.clear();
				expression.push(top);
				TranslationTable.getInstance().doOp(top2,type2,"%");
				idOrLiteralList.clear();
				nodeseq.elementAt(5).accept(this);
				if(idOrLiteralList.size()==1) {
					String assign=TranslationTable.getInstance().getAssignString( idOrLiteralList.elementAt(0).Id(), null,false);
					sourceLst.add(assign);
				}
			} else {//Only Giving
				for(int i=0;i<idOrLiteralList.size();++i) {
					IdOrLit o=idOrLiteralList.get(i);
					if (o.Id()!=null) {
						if(o.isRounded()) { 
							expression.push("__round("+top3+"," +String.valueOf(o.Id().getJavaType().maxFractionLength)+")");
						}
						else expression.push(top3);
						expressionType=type3;
						String assign=TranslationTable.getInstance().getAssignString( o.Id(), null,false);
						sourceLst.add(assign);

					}
				}
			}
			break;			
		}
	}

	@Override
	public void visit(DivideStatement n) {
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doTry();
		n.divideBody.accept(this);
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doCatchArith(n.nodeOptional,n.nodeOptional1);
	}

	private boolean doCorrespondingOp(SymbolProperties from, SymbolProperties to, String op,boolean doReverseOp,boolean first) {
		if(first) {
			if(!isCorrespondingValid2(from,to)) return false;
		}
		else if(!isCorrespondingValid(from,to)) return false;
		if(from.getPictureString()!=null&&to.getPictureString()!=null) {
			expression.clear();
			if(!doReverseOp) {
				doIdOp(from,op);
				doIdOp(to,op);
			} else {
				doIdOp(from,op);
				int topType=expressionType;
				String top = expression.pop3();
				doIdOp(to,op);
				TranslationTable.getInstance().doOp(top,topType,op);
			}
			String assign=TranslationTable.getInstance().getAssignString( to, null,false);
			sourceLst.add(assign);
			return true;
		}
		if(from.getChildren()!=null&&to.getChildren()!=null) {
			for(int i=0;i<from.getChildren().size();++i) {
				for(int j=0;j<to.getChildren().size();++j) {
					if(doCorrespondingOp(from.getChildren().get(i),to.getChildren().get(j),op,doReverseOp,false)) break;
				}
			}
		}
		return false;
	}
	
	private void doLiteralOp(ExpressionString lit,String op) {
		TranslationTable.getInstance().doLiteralOp(lit.toString(), op);
	}
	private void doLiteralOp(String lit,String op) {
		TranslationTable.getInstance().doLiteralOp(lit, op);
	}

	private void doIdOp(SymbolProperties id,String op) {
		TranslationTable.getInstance().doIdOp(id, op);
	}
	
	
	@Override
	public void visit(ArithIdentifier n) {
		super.visit(n);
		if(idOrLiteralList!=null)
			idOrLiteralList.add(new IdOrLit(props,n.nodeOptional.present()));

	}

	@Override
	public void visit(ArithIdentifierList n) {
		idOrLiteralList=new Vector<IdOrLit>();
		super.visit(n);
	}

	@Override
	public void visit(IdOrLiteral n) {
		super.visit(n);
		if(idOrLiteralList!=null) {
			switch(n.nodeChoice.which) {
			case 0:
				idOrLiteralList.add(new IdOrLit(props));
				break;
			case 1:
				idOrLiteralList.add(new IdOrLit(literal));
				break;
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(IdOrLiteralList n) {
		idOrLiteralList=new Vector();
		super.visit(n);
	}
	
	@Override
	public void visit(ComputeStatement n) {
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doTry();
		initExpressionType();
		Stack<IdOrLit> exprLHS= new Stack<IdOrLit>();
		for(Enumeration<Node> e = n.nodeList.elements();e.hasMoreElements();) {
			NodeSequence nodeseq = (NodeSequence) e.nextElement();
			nodeseq.elementAt(0).accept(this);
			IdOrLit idr=new IdOrLit(props,((NodeOptional)nodeseq.elementAt(1)).present());
			exprLHS.push(idr);
		}
		n.arithmeticExpression.accept(this);
		//String exprString=expression.pop3();
		SymbolProperties props2=null;
		for(Enumeration<IdOrLit> e=exprLHS.elements();e.hasMoreElements();) {
			IdOrLit idr=e.nextElement();
			String assign=TranslationTable.getInstance().getAssignString( idr.Id(), props2,false);
			if(assign.length()>0) sourceLst.add(assign);
			props2=idr.Id();
		}
		if(n.nodeOptional1.present()||n.nodeOptional.present())
			doCatchArith(n.nodeOptional,n.nodeOptional1);
	}
	public  java.util.regex.Pattern numPattern=java.util.regex.Pattern.compile("[-+]?[0-9]*(\\.[0-9]+)?");
	private  java.util.regex.Pattern ZERO_PATTERN=java.util.regex.Pattern.compile("[-+]?[0]*(\\.[0]+)?");

	@Override
	public void visit(ExitStatement n) {
		if(consolidationInProgress) return;
		isLastExitStatement=true;
		// TuNT1 sourceLst.add("return doCobolExit();");
		//super.visit(n);
	}

	@Override
	public void visit(GotoStatement n) {
		super.visit(n);
		if(RESConfig.getInstance().getProcedureFormat()==(byte)2)//TODO NATIVE JAVA  METHOD
			return;
		NodeOptional nodeopt=(NodeOptional) ((NodeSequence)n.nodeChoice.choice).elementAt(1);
		((NodeSequence)n.nodeChoice.choice).elementAt(0).accept(this);
		if(nodeopt.present()) {
			NodeSequence nodeseq=(NodeSequence)nodeopt.node;
			nodeseq.elementAt(3).accept(this);
			if(props==null) return;
			sourceLst.add("switch("+new ExpressionString(props).toString()+") {");
			sourceLst.add("tab()");
			sourceLst.add("case 1:");
			NodeListOptional nlo = (NodeListOptional)nodeseq.elementAt(0);
			printCobolGoto(procedureProps);
			int pc=2;
			if(nlo.present()) {
				for(Enumeration<Node> e=nlo.elements();e.hasMoreElements();) {
					e.nextElement().accept(this);
					sourceLst.add("case " +
							String.valueOf(pc++) +
							":");
					printCobolGoto(procedureProps);
				}
			}
			sourceLst.add("backTab()");
			sourceLst.add("}");
		} else {
			printCobolGoto(procedureProps);
		}
	}

	private void printCobolGoto(SymbolProperties props) {
		//Integer paragraphMarkInteger=(Integer)props.getParagraphMark();
		//if(paragraphMarkInteger==null||paragraphMarkInteger.intValue()<=0) return;
		// TuNT1 sourceLst.add("return "+props.getJavaName1()+";");
		sourceLst.add(props.getJavaName1()+"();");
	}

	@Override
	public void visit(ParagraphName n) {
		super.visit(n);
		paragraphProps=SymbolTable.getScope().lookup(lastTokenString, SymbolConstants.PARAGRAPH);
		if(paragraphProps==null) {
			sectionProps=SymbolTable.getScope().lookup(lastTokenString, SymbolConstants.SECTION);
			if(sectionProps==null) {
				reportError(n,"Undefined Procedure Name "+lastTokenString+"\n"); 
				return;
			}
			sectionName=lastTokenString;
			return;
		}
		paragraphName=lastTokenString;
	}
	
	@Override
	public void visit(SectionName n) {
		super.visit(n);
		sectionName=lastTokenString;
		sectionProps=SymbolTable.getScope().lookup(sectionName, SymbolConstants.SECTION);
	}
	
	@Override
	public void visit(ProcedureName n) {
		sectionName=paragraphName=null;
		procedureProps=paragraphProps=sectionProps=null;
		n.nodeChoice.choice.accept(this);
		if(paragraphName!=null&&sectionName!=null) {
			paragraphProps=SymbolTable.getScope().lookup(paragraphName, sectionName);
			procedureProps=paragraphProps;
		} else
			if(sectionName!=null)
				procedureProps=sectionProps;
			else
				if(paragraphName!=null)
					procedureProps=paragraphProps;
		if(procedureProps==null) {
			reportError(n,"Unknown Paragraph "+paragraphName+((sectionName!=null)?" IN "+sectionName:""));
		}
	}

	@Override
	public void visit(ProcedureSection n) {
		
		n.sectionHeader.sectionName.accept(this);
		
		String declarative="";
		if(isDeclarative) declarative=",true";
		/* Update 20210623
		sourceLst.add("Section "+sectionProps.getJavaName1()+"=new Section(this"+declarative+") {");
		sourceLst.add("public CobolMethod run() {");
		sourceLst.add("tab()");
		*/
		n.paragraphs.nodeListOptional.accept(this);
		/* Update 20210623
		sourceLst.add("return super.run();");

		sourceLst.add("backTab()");
		sourceLst.add("}};");
		*/
		n.paragraphs.nodeListOptional1.accept(this);
		
	}

	private boolean consolidationInProgress=false;
	
	private  boolean isLastExitStatement=false;

	@Override
	public void visit(EntryStatement n) {
		
		n.literal.accept(this);
		paragraphProps=SymbolTable.getScope().lookup(
				paragraphName=RunTimeUtil.getInstance().stripQuotes(literal.toString(),true),SymbolConstants.PROGRAM);
		if(paragraphProps==null) {
			reportError(n,"Unknown Symbol :"+literal.toString());
			return;
		}
		/* Update 20210623
		sourceLst.add("public void "+paragraphProps.getJavaName1()+"Entry__(ProgramEnv __env) {");
		sourceLst.add("tab()");
        sourceLst.add("initialize(this); " );
        */
		/* Delete 20210827 S
		if(n.nodeOptional.present()) {
			NodeList nodelist = ((UsingArgs)n.nodeOptional.node).nodeList;
			processUsingArgs1(nodelist);
		} 
		    Delete 20210827 E*/
		/* Update 20210623
        sourceLst.add("doCobolGoto("+paragraphProps.getJavaName1()+");");
        */
		/* Delete 20210827 S
		if(n.nodeOptional.present()) {
			NodeList nodelist = ((UsingArgs)n.nodeOptional.node).nodeList;
			processUsingArgs2(nodelist);
		}
		 Delete 20210827 E */
		/* Update 20210623
		sourceLst.add("backTab()");
		//ClassFile.endMethodScope();

		ClassFile.doProgramScope(paragraphProps, false);
		sourceLst.add("tab()");
		sourceLst.add("public static void main(String[] args) {");
		sourceLst.add("tab()");
		sourceLst.add("__processCmdLineArgs(args);");
		sourceLst.add("new "+SymbolTable.getScope().getFirstProgram().getJavaName2()+"()."+
				paragraphProps.getJavaName1()+"Entry__(null);"
				);
		sourceLst.add("backTab()");
		//ClassFile.endMethodScope();

		sourceLst.add("public void execute(ProgramEnv __env) {");
		sourceLst.add("tab()");
		sourceLst.add("new "+SymbolTable.getScope().getFirstProgram().getJavaName2()+"()."+
				paragraphProps.getJavaName1()+"Entry__(__env);"
				);
		sourceLst.add("backTab()");
		//ClassFile.endMethodScope();
		sourceLst.add("backTab()");
		sourceLst.add("");
		ClassFile.endProgramScope();
	*/
	}

	@Override
	public void visit(Paragraph n) {
		
		if((paragraphProps=((RESNode)n).props)==null)
			n.nodeChoice.choice.accept(this);
		//paragraphProps=SymbolTable.getScope().lookup(lastTokenString, SymbolTypes.PARAGRAPH);
		if(paragraphProps==null) {
			//paragraphProps=SymbolTable.getScope().lookup(lastTokenString, SymbolTypes.SECTION);
			if(paragraphProps==null) {
				try{throw new Exception();} catch(Exception e) {
					reportError(n,"Undefined Paragraph Name "+lastTokenString+"\n"); 
					e.printStackTrace(); 
					return;
				}
			}
		}
		
		if(paragraphProps.isAlteredParagraph()) {//Altered Go To
			sourceLst.add("CobolMethod "+paragraphProps.getJavaName1()+"Altered__=null;");
		}
		
		if(paragraphProps.isConsolidateParagraph()&&!paragraphProps.isGotoTarget()) {
			return;
		}
		isLastExitStatement=false;
		String declarative="";
		if(isDeclarative)declarative=",true";
		// QuyTV
		// Add hadling don't show exit method
		/*
		 * if(paragraphProps.getParent()==nullEntry||paragraphProps.getParent().getType(
		 * )==SymbolConstants.SECTION)
		 * sourceLst.add("Paragraph "+paragraphProps.getJavaName1()
		 * +"=new Paragraph(this," +paragraphProps.getParent().getJavaName1()+
		 * declarative+") {"); else
		 * sourceLst.add("Paragraph "+paragraphProps.getJavaName1()
		 * +"=new Paragraph(this" +declarative+ ") {");
		 */
		// Update 20210828 S
		if (sourceLst.isEmpty()) { 
			if (strMainFunction==null)
				strMainFunction = paragraphProps.getJavaName1();
		}else {
			if  (sourceLstFirst.isEmpty() && strMainFunction == null)
			{
				sourceLstFirst=(ArrayList<String>)sourceLst.clone();
				sourceLst.clear();
			}
		}
		// Update 20210828 E
		if(!paragraphProps.getDataName().contains("-EXIT")) {
			sourceLst.add("public void " + paragraphProps.getJavaName1() + "() {");
			sourceLst.add("tab()");
			if (paragraphProps.isAlteredParagraph()) {// Not Altered
				/* TuNT1
				sourceLst.add("if(" + paragraphProps.getJavaName1() + "Altered__!=null)");
				sourceLst.add("\treturn " + paragraphProps.getJavaName1() + "Altered__;");
				TuNT1 */
			}
			n.nodeChoice1.accept(this);
			/*
			 * if (!n.deadStatement && !isLastExitStatement) {
			 * sourceLst.add("return doCobolExit();"); }
			 */
			sourceLst.add("backTab()");
			sourceLst.add("}");
		}
		

	}

	@Override
	public void visit(AlterStatement n) {
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			NodeSequence nodeseq=(NodeSequence)e.nextElement();
			nodeseq.elementAt(0).accept(this);
			SymbolProperties alt= procedureProps;
			nodeseq.elementAt(3).accept(this);
			sourceLst.add(alt.getJavaName1()+"Altered__="+procedureProps.getJavaName1()+";");
		}
	}

	@Override
	public void visit(Basis n) {
		boolean saveUMinus= doUMinus;
		int saveType=expressionType;
		n.nodeChoice.choice.accept(this);
		switch(n.nodeChoice.which) {
		case 0:
			literal=new ExpressionString(props);
			if(doUMinus) {
				if(props.getJavaType().type<CobolSymbol.BIGDECIMAL)
					literal.literal.insert(0,'-');
				else
					literal.literal.append(".negate()");
			}
			
			expression.push(TranslationTable.getInstance().convertType(
					literal, expressionType=Math.max(props.getJavaType().type, expressionType),
					props.getJavaType().type));
			break;
		case 1:
			literal=formatLiteral(literal);
			if(doUMinus)	literal.literal.insert(0,'-');
			expression.push(TranslationTable.getInstance().convertType(literal
					, Math.max(saveType, expressionType), expressionType));
			 expressionType=Math.max(saveType, expressionType);
			break;
		case 2:
			literal=expression.pop();
			literal.literal.insert(0,"(").append(")");
			if(saveUMinus)
				if(props.getJavaType().type<CobolSymbol.BIGDECIMAL)
					literal.literal.insert(0,'-');
				else
					literal.literal.append(".negate()");
			expression.push(literal);
		}
		
		doUMinus=false;

	}

	
	@Override
	public void visit(Power n) {
		String exprString="";
		doUMinus=false;
		if(n.nodeOptional.present()) {
			n.nodeOptional.node.accept(this);
			doUMinus=lastTokenString.charAt(0)=='-';
		}
		
		n.basis.accept(this);
		
		if(n.nodeListOptional.present()) {
			for(Enumeration<Node> e=n.nodeListOptional.elements();e.hasMoreElements();) {
				Node node = e.nextElement();
				node.accept(this);
				ExpressionString arg2=ExpressionString.getExpression();
				ExpressionString arg1=ExpressionString.getExpression();
				expression.push(TranslationTable.getInstance().doOp(
						TranslationTable.POW_OP,arg1,arg2));
			}
		}
	}

	
	@Override
	public void visit(ReturnStatement n) {
	
	}
	private boolean doingArithmeticExpression = false;
	@Override
	public void visit(ArithmeticExpression n) {
		doingArithmeticExpression = true;
		String exprString = "";
		n.timesDiv.accept(this);
		if(n.nodeListOptional.present()) {
			for(Enumeration<Node> e = n.nodeListOptional.elements();e.hasMoreElements();) {
				Node node=e.nextElement();
				((NodeSequence)node).elementAt(0).accept(this);
				String op=lastTokenString;
				node.accept(this);
				ExpressionString arg2=ExpressionString.getExpression();
				ExpressionString arg1=ExpressionString.getExpression();
				expression.push(TranslationTable.getInstance().doOp(op,arg1,arg2));
			}
			
		}
		doingArithmeticExpression=false;
	}

	@Override
	public void visit(TimesDiv n) {
		String exprString = "";
		n.power.accept(this);
		if(n.nodeListOptional.present()) {
			for(Enumeration<Node> e = n.nodeListOptional.elements();e.hasMoreElements();) {
				Node node=e.nextElement();
				((NodeSequence)node).elementAt(0).accept(this);
				String op=lastTokenString;
				((NodeSequence)node).elementAt(1).accept(this);
				ExpressionString arg2=ExpressionString.getExpression();
				ExpressionString arg1=ExpressionString.getExpression();
				expression.push(TranslationTable.getInstance().doOp(op,arg1,arg2));
			}
		}
	}

	@Override
	public void visit(AcceptStatement n) {
		n.identifier.accept(this);
		String exprString=null;
		if(n.nodeOptional.present()) {
			NodeChoice nodech = (NodeChoice) n.nodeOptional.node;
			NodeSequence nodeseq = (NodeSequence) nodech.choice;
			NodeChoice nodechoice = (NodeChoice)nodeseq.elementAt(1);
			switch(nodechoice.which){
			case 0://Mnemonic
			case 1://Environment
				nodechoice.accept(this);
				if(!lastTokenString.equalsIgnoreCase("CONSOLE")) {
					exprString="__getEnvironment("+formatLiteral(
							new ExpressionString('\"'+RunTimeUtil.getInstance().stripQuotes(lastTokenString)+'\"'))+")";
				}else
					exprString=doOneAccept();
				break;
			case 2://Date
				if(((NodeOptional)((NodeSequence)nodechoice.choice).elementAt(1)).present())
					exprString="Console.readDate2()";
				else
					exprString="Console.readDate()";
				break;
			case 3://Day
				if(((NodeOptional)((NodeSequence)nodechoice.choice).elementAt(1)).present())
					exprString="Console.readDay2()";
				else
					exprString="Console.readDay()";
				break;
			case 4://Day of Week
				exprString="Console.readDayOfWeek()";
				break;
			case 5://time
				exprString="Console.readTime()";
				break;
			}
			expression.push(exprString);
			expressionType=CobolSymbol.STRING;
			sourceLst.add(TranslationTable.getInstance().getAssignString( props, null,false));
			return;
		}
		else 
			exprString=doOneAccept();
		
		if(exprString!=null) {
			exprString=NameUtil.getJavaName(props,true).replace("%0", exprString);
			sourceLst.add(exprString+";");
		}
	}

	private String doOneAccept() {
		String exprString=null;
		CobolSymbol sym=(CobolSymbol) props.getJavaType();
		if(sym!=null) { 
			switch(sym.type) {
				case CobolSymbol.BYTE:exprString="(byte) Console.readChar()"; break;
				case CobolSymbol.CHAR:exprString="Console.readChar()"; break;
				case CobolSymbol.SHORT:exprString="(short)Console.readInt()"; break;
				case CobolSymbol.INTEGER:exprString="Console.readInt()"; break;
				case CobolSymbol.LONG:exprString="Console.readLong()"; break;
				case CobolSymbol.FLOAT:exprString="(float) Console.readDouble()"; break;
				case CobolSymbol.DOUBLE:exprString="Console.readDouble()"; break;
				case CobolSymbol.BIGDECIMAL:exprString="new BigDecimal(Console.readDouble())"; break;
				case CobolSymbol.STRING:exprString="Console.readLine()"; break;
				case CobolSymbol.GROUP:exprString="Console.readBytes()"; break;
			}
		}
		return exprString;

	}
	
	@Override
	public void visit(Identifier n) {
		if(n.nodeChoice.which==1)
			props=SymbolTable.getScope().lookup("RETURN-CODE");
		else
			super.visit(n);
	}
	
	@Override
	public void visit(IntrinsicFunction n) {
		int[] functionArgType=SymbolConstants.getFunctionArgTypes(((NodeChoice)n.nodeSequence.elementAt(1)).which);
		String exprString="__function__"+String.valueOf(((NodeChoice)n.nodeSequence.elementAt(1)).which)+"__";
		doit:
		if(((NodeOptional)n.nodeSequence.elementAt(2)).present()) {
			
			NodeOptional nodeopt=(NodeOptional)((NodeSequence)((NodeOptional)n.nodeSequence.elementAt(2)).node).elementAt(1);
			if(nodeopt.present()) {
				NodeChoice nodechoice= (NodeChoice)nodeopt.node;
				NodeSequence nodeseq=(NodeSequence) nodechoice.choice;
				int saveType = expressionType=0;
				nodeseq.elementAt(0).accept(this);
				saveType=Math.max(saveType,expressionType);
				if(nodechoice.which==0) {
					saveType=props.getJavaType().type;
					if(functionArgType[0]<0)
						exprString+=SymbolConstants.getSQL(props.getJavaType().type);
					else
					if(functionArgType.length>1) {
						for(int i=1;i<functionArgType.length;i++)
							if(functionArgType[i]>=saveType) 
							{saveType=functionArgType[i];break;}
						if(saveType==Constants.BIGDECIMAL) saveType=Constants.DOUBLE;
						exprString+=SymbolConstants.getSQL(saveType);
					}
					expressionType=saveType;
					exprString+="(";
					if(props!=null) {
						StringBuffer indexes = new StringBuffer();
						indexes.append("new __IndexAll(").
							append(props.findNearestCobolBean().getCobolBeanName()).append(",\"").
							append("get").append(props.getJavaName2()+"\"");
						for(SymbolProperties oc:props.getOccursParents())
							indexes.append(',').append(oc.getMaxOccurs());
						indexes.append(')');
						exprString+=indexes.toString();
						exprString+=")";
						break doit;
					}
				} else {
					exprString+="(";
					ArrayList<ExpressionString> exprStrings=new ArrayList<ExpressionString>();
					exprStrings.add(ExpressionString.getExpression());
					saveType=expressionType;
					if(((NodeListOptional)nodeseq.elementAt(1)).present()) {
						for(Enumeration<Node> e=((NodeListOptional)nodeseq.elementAt(1)).elements();e.hasMoreElements();) {
							expressionType=0;
							e.nextElement().accept(this);
							saveType=Math.max(saveType,expressionType);
							exprStrings.add(ExpressionString.getExpression());
						}
					}
					if(functionArgType.length>1) {
						for(int i=1;i<functionArgType.length;i++)
							if(functionArgType[i]>=saveType) 
							{saveType=functionArgType[i];break;}
						if(saveType==Constants.BIGDECIMAL) saveType=Constants.DOUBLE;
						expressionType=saveType;
						for(int i=0;i<exprStrings.size();++i) {
							exprString+=TranslationTable.getInstance().convertType(
									exprStrings.get(i).toString(),saveType,exprStrings.get(i).type);
							if(i<exprStrings.size()-1)exprString+=",";
						}
					}
				}
				exprString+=")";
			} else
				exprString+="()";
			if(functionArgType[0]>0)
				expressionType = functionArgType[0];
		} else 
			exprString+="()";
		expression.push(exprString);
		literal=ExpressionString.getExpression();
	}

	@Override
	public void visit(FunctionArgument n) {
		super.visit(n);
		switch(n.nodeChoice.which) {
		case 0:
			expression.push(NameUtil.getJavaName(props, false));
			expressionType = props.getJavaType().type;
			break;
		case 1:
			expression.push(formatLiteral(literal));
			break;
		}
	}

	private String declareTemp(SymbolProperties pr) {
		String ret;
		sourceLst.add(SymbolConstants.get(props.getJavaType().type)+" " +
				(ret="temp"+String.valueOf(SymbolTable.tempVariablesMark++))+
				";");
		return ret;
	}

	private String declareTempArr(SymbolProperties pr) {
		String ret;
		String modif="";
		for(int i=props.getNoOccursSubscripts();i>0;--i)modif+="[]";
		sourceLst.add(SymbolConstants.get(props.getJavaType().type)+modif+" " +
				(ret="temp"+String.valueOf(SymbolTable.tempVariablesMark++))+
				";");
		return ret;
	}

	@Override
	public void visit(SpecialRegister n) {
		switch(n.nodeChoice.which) {
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
			n.nodeChoice.choice.accept(this);
			props=SymbolTable.getScope().lookup(lastTokenString,SymbolConstants.DATA);
			if(props!=null) {
				literal=new ExpressionString(new ExpressionString(props).toString());
				literal.type=CobolSymbol.STRING;
				break;
			}
		default:
			 literal=new ExpressionString(0);break;
		case 1: 
			n.nodeChoice.choice.accept(this);
			literal=new ExpressionString(props.getLength());
			break;
		}
	}
	@SuppressWarnings("unchecked")
	@Override
	public void visit(NodeToken n) {
		//super.visit(n);
		lastTokenString=n.tokenImage;
		if(doNumericConstant)
			numericConstant+=n.tokenImage;
		if(doQualified) {
			if(qualified==null)
				qualified=new Stack();
			if(n.tokenImage!=null) {
				if(n.tokenImage.equalsIgnoreCase("IN")||
						n.tokenImage.equalsIgnoreCase("OF"))
					;
				else
					qualified.push(n.tokenImage);
			}
		} else 
			if(doLiteral) {
				literal.literal.append(n.tokenImage);
			} else
				if (doingAcceptOption) {
					if(optionStack==null)
						optionStack=new Stack();
					optionStack.push(n.tokenImage);
				} /*else
					if (doingRelationalOp){
						switch(n.kind) {
						case CobolParserConstants.NOT: break;
						case CobolParserConstants.GREATER: break;
						case CobolParserConstants.LESS: break;
						case CobolParserConstants.EQUAL: break;
						case CobolParserConstants.MORETHANCHAR: break;
						case CobolParserConstants.MORETHANOREQUAL: break;
						case CobolParserConstants.LESSTHANCHAR: case CobolParserConstants.LESSTHANOREQUAL: break;
						case CobolParserConstants.EQUALCHAR: default:break;
						}
					}*/
	}

	@Override
	public void visit(QualifiedDataName n) {
		qualified.clear();
		doQualified=true;
		super.visit(n);
		processQualifiedStack(n);
		doQualified=false;
	}


	private void processQualifiedStack(Node n) {

		SymbolProperties props2=null;props=null;
		if(qualified==null||qualified.size()<=0) return;
		String curr=null;
		String prev=null; 
		curr=(String)qualified.pop();
		do{
			if(qualified.size()>0) {
				prev=curr;
				curr=(String)qualified.pop();
				if(props2!=null) {
					props2=findChild(props2,curr);
				}else {
					props2 = SymbolTable.getScope().lookup(curr,prev);
				}
				if (props2==null) {
					reportError(n,"Unknown Symbol : "+ curr+" IN "+prev );
					break;
				}
			} else {
				props2 = SymbolTable.getScope().lookup(curr);
				if (props2==null) {
					reportError(n,"Unknown Symbol : "+curr);
					break;
				}
			}
			
			if(props2.isVaryingArray()&&props2.getParent().isVarying())
				props2=props2.getParent();
			
		} while(qualified!=null&&qualified.size()>0) ;
		
		if(props==null) {
			//try {
			props=(SymbolProperties) props2;//.clone();
			//} catch(CloneNotSupportedException ce) {
				//TODO One more error processing
			//}
			props.setIndexesWorkSpace(null);//subscripts will fill this
			props.setSubstringWorkSpace(null);//LeftMost... and Length will fill this
		}
	}

	private SymbolProperties findChild(SymbolProperties props,String name) {
		if(props.getChildren()==null||props.getChildren().size()<=0) return null;
		for(SymbolProperties ch:props.getChildren())
			if(ch.getDataName().equalsIgnoreCase(name))
				return ch;
		SymbolProperties ret=null;
		for(SymbolProperties ch:props.getChildren()) {
			ret=findChild(ch,name);
			if(ret!=null) return ret;
		}
		return ret;
	}
	
	private SymbolProperties findProgram(SymbolProperties props) {
		if(props==null) return null;
		do {
			props=props.getParent();
		} while(props!=null&&!props.isProgram());
		return props;
	}
	
	
	@Override
	public void visit(Subscript n) {
		SymbolProperties propsDataName=props;
		if(props.getIndexesWorkSpace()==null)
			props.setIndexesWorkSpace(new ArrayList<String>());
		//for (Enumeration e=n.nodeList.elements();e.hasMoreElements();) {
			//NodeChoice ch=(NodeChoice)e.nextElement();
			switch(n.nodeChoice.which){
			case 0:
				n.nodeChoice.choice.accept(this);
				int i=0,j=0;
				do {
					if((j=literal.literal.indexOf(",", i))<0) j=literal.literal.length();
					literal.literal.replace(i, j, stripLeadingZeros(literal.literal.substring(i,j)).toString());
					i=j+1;
				} while(i<literal.toString().length());
				propsDataName.getIndexesWorkSpace().add(literal.toString());
				break;
			case 1:
				NodeSequence seq =(NodeSequence)n.nodeChoice.choice;
				seq.elementAt(0).accept(this);
				String sub=new ExpressionString(props).toString();
				CobolSymbol sym=props.getJavaType();
				if(sym!=null) {
					if(sym.type==CobolSymbol.LONG)
						sub="(int)"+sub;
					else
						if(sym.type==CobolSymbol.BIGDECIMAL)
							sub+=".intValue()";
				}
				if(((NodeOptional)seq.elementAt(1)).present()) {
					NodeSequence seq2=(NodeSequence)((NodeOptional)seq.elementAt(1)).node;
					NodeChoice ch2=(NodeChoice)seq2.elementAt(0);
					switch(ch2.which) {
					case 0:
						sub+="+";break;
					case 1:
						sub+="-";break;
					}
					seq2.elementAt(1).accept(this);
					i=j=0;
					do {
						if((j=literal.literal.indexOf(",", i))<0) j=literal.literal.length();
						literal.literal.replace(i, j, stripLeadingZeros(literal.literal.substring(i,j)).toString());
						i=j+1;
					} while(i<literal.toString().length());
					sub+=literal.toString();
				}
				propsDataName.getIndexesWorkSpace().add(sub);
				break;
			case 2:
			}
		//}
		//propsDataName.setIndexesWorkSpace(a);
		props=propsDataName;
		//super.visit(n);
	}

	@Override
	public void visit(LeftmostCharacterPosition n) {
		SymbolProperties propsDataName=props;
		initExpressionType();
		super.visit(n);
		if(expression.size()>0&&expressionType<CobolSymbol.BIGDECIMAL) {//TODO REPORT ERROR ON NOT INTEGER SUBSCRIPT
			propsDataName.setSubstringWorkSpace(new ArrayList<String>());
			propsDataName.getSubstringWorkSpace().add(simplify(expression.pop3()+"-1"));
		}
		props=propsDataName;
	}

	@Override
	public void visit(Length n) {
		SymbolProperties propsDataName=props;
		initExpressionType();
		super.visit(n);
		if(propsDataName.getSubstringWorkSpace()==null) {
			propsDataName.setSubstringWorkSpace(new ArrayList<String>());
			propsDataName.getSubstringWorkSpace().add("0");
		}
		if(expression.size()>0&&expressionType<CobolSymbol.BIGDECIMAL)//TODO REPORT ERROR ON NOT INTEGER SUBSCRIPT
			propsDataName.getSubstringWorkSpace().add(expression.pop3());
		props=propsDataName;
	}

	@Override
	public void visit(ProgramIdParagraph n) {
		SymbolProperties pgm=SymbolTable.getScope().lookup(n.programName.cobolWord.nodeToken.tokenImage
				,SymbolConstants.PROGRAM);
		if (pgm==null){
			reportError(n,"Fatal Error: Cobol2Java.visit(ProgramIdParagraph):"+
					n.programName.cobolWord.nodeToken.tokenImage);
		}
		SymbolTable.getScope().startProgram(pgm);
		super.visit(n);
	}

	@Override
	public void visit(NestedProgramIdParagraph n) {
		SymbolProperties pgm=SymbolTable.getScope().lookup(n.programName.cobolWord.nodeToken.tokenImage
				,SymbolConstants.PROGRAM);
		if (pgm==null){
			reportError(n,"Fatal Error: Cobol2Java.visit(ProgramIdParagraph):"+
					n.programName.cobolWord.nodeToken.tokenImage);
		}
		SymbolTable.getScope().startProgram(pgm);
		super.visit(n);
	}

	@Override
	public void visit(Literal n) {
		literal=new ExpressionString();
		if(n.nodeChoice.which==3) {
			n.nodeChoice.choice.accept(this);
		}
		else if(n.nodeChoice.which==4) {
			n.nodeChoice.choice.accept(this);
		}
		else 
			if(n.nodeChoice.which==5) {
				NodeSequence nodeseq=(NodeSequence) n.nodeChoice.choice;
				if(((NodeOptional)nodeseq.elementAt(1)).present()) {
					((NodeOptional)nodeseq.elementAt(1)).accept(this);
				} else {
					props=null;
					for(SymbolProperties f:SymbolTable.getScope().getCurrentProgram().getChildren()){
						if(f.isFile()) {
							props=f;break;
						}
							
					}
				}
				if(props==null||!props.isFile()) {
					reportError(n,"Unknown LINAGE-COUNTER either file name missing or invalid.");
					return;
				}
				literal=new ExpressionString(props.getJavaName1()+".linageCounter()");
				literal.type=Constants.INTEGER;
			}
		else {
			doLiteral=true;
			n.nodeChoice.choice.accept(this);
			doLiteral=false;
		} 
		
		if(n.nodeOptional.present()&&(n.nodeChoice.which==0||n.nodeChoice.which==1)) {
			literal.isAll=true;
		}
	}

	private  String numericConstant="";
	private  boolean doNumericConstant=false;
	
	@Override
	public void visit(NumericConstant n) {
		numericConstant="";
		doNumericConstant=true;
		super.visit(n);
		literal=new ExpressionString(numericConstant);
		doNumericConstant=false;
	}

	private  boolean isPreviousRelationCondition=false;
	private  ExpressionString previousRelationSubject=null;
	private  int previousRelationOp=0;
	private  int relop=0;

	@Override
	public void visit(Condition n) {
		isPreviousRelationCondition=false;
		n.combinableCondition.accept(this);
		if (n.nodeListOptional.present()) {
			for (int i=0;i<n.nodeListOptional.size();++i) {
				NodeSequence nodeseq = (NodeSequence) n.nodeListOptional.elementAt(i);
				NodeChoice nodechoice = (NodeChoice) nodeseq.elementAt(0);
				nodeseq.elementAt(1).accept(this);
				int op = 0;
				switch (nodechoice.which) {
				case 0:
					op = TranslationTable.AND_CONDITION;break;
				case 1:
				default:
					op = TranslationTable.OR_CONDITION;break;
				}
				ExpressionString temp = new ExpressionString(condition.pop()); //For reversing the order
				condition.push(TranslationTable.getInstance().doCondition(
						op, 
						new ExpressionString(condition.pop()),
						temp,
						doReverseCondition));
			}
		}
	}

	@Override
	public void visit(RelationalOperator n) {
		super.visit(n);
		boolean isNot=n.nodeOptional1.present()?!doReverseCondition:doReverseCondition;
		mapRelOp(n.nodeChoice.which,isNot);
	}
	
	private int mapRelOp(int which,boolean isNot) {
		relop=TranslationTable.EQ_CONDITION;
		switch(which) {
			case 0:
			case 1:
				relop = TranslationTable.GE_CONDITION;break;
			case 2:
			case 3:
				relop= TranslationTable.LE_CONDITION;break;
			case 4:
			case 5:
				relop= TranslationTable.GT_CONDITION;break;
			case 6:
			case 7:
				relop= TranslationTable.LT_CONDITION;break;
			case 8:
			case 9:
				relop= TranslationTable.EQ_CONDITION;break;
			case 10:
				relop= TranslationTable.NE_CONDITION;break;
			default:
		}
		return relop=TranslationTable.getInstance().getReverseOp(relop,isNot);
	}

	//private  int conditionType=0;
	
	@Override
	public void visit(AbbreviationLeaf n) {
		super.visit(n);
	}

	@Override
	public void visit(AbbreviationRest n) {
		if(!isPreviousRelationCondition)
			return;//TODO Syntax error reporting
		for(int i=0;i<n.nodeList.size();++i) {
			NodeSequence seq=(NodeSequence)n.nodeList.elementAt(i);

			if(((NodeOptional)seq.elementAt(1)).present()) {
				if(((NodeOptional)seq.elementAt(0)).present())
					doReverseCondition=!doReverseCondition;//for formatComparison
				seq.elementAt(1).accept(this);
				if(((NodeOptional)seq.elementAt(0)).present())
					doReverseCondition=!doReverseCondition;//Restore
			} else {
				relop = mapRelOp(relop, ((NodeOptional)seq.elementAt(0)).present());
			}
			seq.elementAt(2).accept(this);
			String top=TranslationTable.getInstance().doCondition(relop, previousRelationSubject, ExpressionString.getExpression(),false);;
			//while(condition.size()>0)
				//top=condition.pop()+top;
			condition.push(top);
		}
	}

	@Override
	public void visit(RelationCondition n) {
		isPreviousRelationCondition=true;
		initExpressionType();
		n.arithmeticExpression.accept(this);
		previousRelationSubject=ExpressionString.getExpression();
		initExpressionType();
		n.nodeChoice.accept(this);
		if(n.nodeChoice.which == 0) {
			String top=TranslationTable.getInstance().doCondition(relop, previousRelationSubject,
					ExpressionString.getExpression(),false);
			//while(condition.size()>0)
				//top=condition.pop()+top;
			condition.push(top);
		}
	}

	private  String formatCompareToRelop(int relopInt) {
		switch(relopInt) {
		case 0: return " < 0";
		case 1: return " <= 0";
		case 2: return " > 0";
		case 3: return " >= 0";
		case 4: return " == 0";
		case 5: return " !=0";
		}
		return "";
	}

	@Override
	public void visit(CombinableCondition n) {
		if(n.nodeOptional.present())
			doReverseCondition=!doReverseCondition;
		super.visit(n);
		if(n.nodeOptional.present())
			doReverseCondition=!doReverseCondition;
	}

	@Override
	public void visit(ClassCondition n) {
		isPreviousRelationCondition=false;
		n.identifier.accept(this);
		StringBuffer name=new StringBuffer(new ExpressionString(props).toString());
		int begin=name.lastIndexOf(".")+1;
		int end = name.indexOf("(");

		if (end < begin)
		{
		   end = name.lastIndexOf("(");
		}

		switch(n.nodeChoice.which){
		case 0:
			if(props.isNumericTested())
				condition.push(name.replace(begin,end, "is"+props.getJavaName2()+"Numeric").toString());
			else
				condition.push(name.toString());
			break;
		case 1:
			if(props.isAlphabeticTested())
				condition.push(name.replace(begin,end, "is"+props.getJavaName2()+"Alphabetic").toString());
			else
				condition.push(name.toString());
			break;
		case 2:
			if(props.isAlphabeticTested())
				condition.push(name.replace(begin,end, "is"+props.getJavaName2()+"AlphabeticLower").toString());
			else
				condition.push(name.toString());
			break;
		case 3:
			if(props.isAlphabeticTested())
				condition.push(name.replace(begin,end, "is"+props.getJavaName2()+"AlphabeticUpper").toString());
			else
				condition.push(name.toString());
			break;
		case 4:
		case 5:
		case 6:
			condition.push(name.toString());
		}
		if(n.nodeOptional1.present()) {
			condition.push("!"+condition.pop());
		}
	}

	@Override
	public void visit(SignCondition n) {
		isPreviousRelationCondition=false;
		super.visit(n);
		boolean isNot = n.nodeOptional1.present()?!doReverseCondition:doReverseCondition;
		int op = 0;
		switch(n.nodeChoice.which) {
		case 0: op = TranslationTable.POSITIVE_CONDITION;break;
		case 1: op = TranslationTable.NEGATIVE_CONDITION;break;
		case 2: op = TranslationTable.ZERO_CONDITION;
		}
		if(isNot)
			condition.push('!'+TranslationTable.getInstance().doCondition(op, 
				previousRelationSubject, false));
		else
			condition.push(TranslationTable.getInstance().doCondition(op, 
					previousRelationSubject, false));
		expressionType=CobolSymbol.INTEGER;
	}

	@Override
	public void visit(ConditionNameCondition n) {
		props=null;
		isPreviousRelationCondition=false;
		super.visit(n);
		String top=((doReverseCondition)?"!(%0)":"%0");
		if(props==null) return;
		if(props.getLevelNumber()==88||previousRelationSubject==null)
			top=top.replace("%0",new ExpressionString(props).toString());
		else {
			expression.push(new ExpressionString(props).toString());expressionType=props.getJavaType().type;
			top=top.replace("%0",
					TranslationTable.getInstance().
					doCondition(relop, previousRelationSubject, ExpressionString.getExpression(),false));
		}
		condition.push(top);
	}

	@Override
	public void visit(ConditionNameReference n) {
		qualified.clear();
		doQualified=true;
		n.conditionName.accept(this);
		switch(n.nodeChoice.which) {
		case 0:
			NodeSequence nodeseq=(NodeSequence)n.nodeChoice.choice;
			nodeseq.elementAt(0).accept(this);
			nodeseq.elementAt(1).accept(this);
			doQualified=false;
			processQualifiedStack(n);
			nodeseq.elementAt(2).accept(this);
			break;
		case 1:
			n.nodeChoice.choice.accept(this);
			doQualified=false;
			processQualifiedStack(n);
		}
	}
	@Override
	public void visit(SimpleCondition n) {
		switch(n.nodeChoice.which) {
		case 3:
			if (n.nodeChoice.choice instanceof NodeSequence){
				//String top="";
				//if (condition.size()>0)
					//top=condition.pop();
				NodeSequence nodeseq = (NodeSequence) n.nodeChoice.choice;
				nodeseq.elementAt(1).accept(this);
				condition.push("("+condition.pop()+")");
			}
			break;
		case 1:
		case 2:
		case 0:
			n.nodeChoice.choice.accept(this);
		}
	}
	
	private  boolean doReverseCondition=false;
	private  boolean doUMinus=false;
	
	@Override
	public void visit(IfStatement n) {
		initExpressionType();
		doReverseCondition=false;
		condition=new Stack<String>();
		n.condition.accept(this);
		doReverseCondition=false;
		sourceLst.add("if("+condition.pop()+") {");
		sourceLst.add("tab()");
		switch(n.nodeChoice.which) {
		case 0:
			NodeSequence seq =(NodeSequence)n.nodeChoice.choice;
			seq.elementAt(0).accept(this);
			break;
		case 1:
		default:
		}
		//n.statementList.accept(this);
		
		sourceLst.add("backTab()");
		if(n.nodeOptional1.present()) {
			sourceLst.add("} else {");
			sourceLst.add("tab()");
			if (n.nodeOptional1.node instanceof NodeSequence) {
				NodeSequence seq2=(NodeSequence)n.nodeOptional1.node;
				NodeChoice choice = (NodeChoice) seq2.elementAt(1);
				switch(choice.which) {
				case 0:
					NodeSequence seq =(NodeSequence)choice.choice;
					seq.elementAt(0).accept(this);
					break;
				case 1:
				default:
				}
			}
			sourceLst.add("backTab()");
			sourceLst.add("}");
		} else 
			sourceLst.add("}");
	}
	
	@Override
	public void visit(PerformBody arg0) {
		//super.visit(arg0);
		switch(arg0.nodeChoice.which) {
		case 1:
			if(arg0.nodeChoice.choice instanceof NodeSequence) {
				NodeSequence nodeseq=(NodeSequence)arg0.nodeChoice.choice;
				if(nodeseq.nodes.size()==2) {
					nodeseq.accept(this);//do the descent for both
					if(procedureProps==null)
						return;
					if(!someOrAllConsolidated(procedureProps,procedureThruProps)) {
						sourceLst.add( procedureProps.getJavaName1() +"();");
						/*
						 * sourceLst.add("doCobolPerform(" + procedureProps.getJavaName1() +
						 * ((procedureThruProps == null) ? "" : ("," +
						 * procedureThruProps.getJavaName1())) + ");");
						 */
					}
					if(((NodeOptional)nodeseq.elementAt(1)).present()&&
						endBlockScopeCriterion!=null&&endBlockScopeCriterion.size()>0) {
						Stack<String> st = endBlockScopeCriterion.pop(); 
						while(st.size()>0) {
							String endScope=st.pop();
							if(endScope.charAt(0)=='}')
								sourceLst.add("backTab()");
							sourceLst.add(endScope);
						}
					}
				}
			}
			break;
		case 0:
			if(arg0.nodeChoice.choice instanceof NodeSequence) {
				NodeSequence nodeseq=(NodeSequence)arg0.nodeChoice.choice;
				nodeseq.elementAt(0).accept(this);//do the descent for both
				nodeseq.elementAt(1).accept(this);
				if(endBlockScopeCriterion!=null&&endBlockScopeCriterion.size()>0) {
					Stack<String> st = endBlockScopeCriterion.pop(); 
					while(st.size()>0) {
						String endScope=st.pop();
						if(endScope.charAt(0)=='}')
							sourceLst.add("backTab()");
						sourceLst.add(endScope);
					}
				}
			}
			break;
		default:
		}
	}

	private boolean someOrAllConsolidated(SymbolProperties from, SymbolProperties thru) {
		/*
		if(thru==null) thru=from;
		if(thru.getParagraphMark()>=from.getParagraphMark()) {
			SymbolProperties sectionF=null,paraF=null,sectionT=null,paraT=null;
			if(from.getType()==SymbolConstants.PARAGRAPH) {
				sectionF=from.getParent();
				paraF=from;
			} else {
				sectionF=from;
				paraF=from.getChildren().get(0);
			}
			if(thru.getType()==SymbolConstants.PARAGRAPH) {
				sectionT=thru.getParent();
				paraT=thru;
			} else {
				sectionT=thru;
				paraT=thru.getChildren().get(0);
			}
			allUnConsolidated: {
				someConsolidated: {
					if(!sectionF.getDataName().equalsIgnoreCase(sectionT.getDataName())) {
						props=sectionF.getChildren().get(sectionF.getChildren().size()-1);
					} else {
						props=paraT;
					}
					for(paragraphProps=paraF;paragraphProps!=null&&paragraphProps.getParagraphMark()<=props.getParagraphMark();paragraphProps=paragraphProps.getSibling()) {
						if(paragraphProps.isConsolidateParagraph()&&!paragraphProps.isGotoTarget()) break someConsolidated;
					}
					if(!sectionF.getDataName().equalsIgnoreCase(sectionT.getDataName())) {
						paragraphProps=sectionT.getChildren().get(0);
					} else {
						break allUnConsolidated;
					}
					for(;paragraphProps!=null&&paragraphProps.getParagraphMark()<=paraT.getParagraphMark();paragraphProps=paragraphProps.getSibling()) {
						if(paragraphProps.isConsolidateParagraph()&&!paragraphProps.isGotoTarget()) break someConsolidated;
					}
				}
				//Some Consolidated
				if(!sectionF.getDataName().equalsIgnoreCase(sectionT.getDataName())) {
					unConsolidate(paraF,sectionF.getChildren().get(sectionF.getChildren().size()-1));
				} else {
					unConsolidate(paraF,paraT);
				}
				if(!sectionF.getDataName().equalsIgnoreCase(sectionT.getDataName())) {
					if(!sectionF.getDataName().equalsIgnoreCase(sectionT.getDataName())) {
						paragraphProps=sectionT.getChildren().get(0);
						unConsolidate(paragraphProps,paraT);
					} else {
					}
				} else {
				}
				return true;
			}
			//allUnConsolidation
		}*/
		return false;
	}
	/*
	private void unConsolidate(SymbolProperties paraF,SymbolProperties thru) {
		consolidationInProgress=true;
		if(thru==null) thru=paraF;
		SymbolProperties temp=null,temp2=null;boolean lastOneToBePrnted=false;
		for(props=paraF;props!=null&&props.getParagraphMark()<=thru.getParagraphMark();props=props.getSibling()) {
			if(props.isConsolidateParagraph()&&!props.isGotoTarget()) {
				if(lastOneToBePrnted) {
					
					sourceLst.add("doCobolPerform("+temp.getJavaName1()+","+((temp2==null)?"null":temp2.getJavaName1())+");");
					temp=temp2=null;lastOneToBePrnted=false;
				}
				((Node)props.getOtherData2()).accept(this);
			} else {
				if(temp==null)
					temp=props;
				else
					temp2=props;
				lastOneToBePrnted=true;
			}
		}
		if(lastOneToBePrnted) {
			sourceLst.add("doCobolPerform("+temp.getJavaName1()+","+((temp2==null)?"null":temp2.getJavaName1())+");");
			temp=temp2=null;lastOneToBePrnted=false;
		}
		consolidationInProgress=false;
	}
	*/
	@Override
	public void visit(PerformOption arg0) {
		//super.visit(arg0);
		Stack<String> st = new Stack<String>();
		switch(arg0.nodeChoice.which) {
		case 0:
			String timesVar=null;
			if(arg0.nodeChoice.choice instanceof NodeSequence) {
				NodeSequence nodeseq = (NodeSequence) arg0.nodeChoice.choice;
				nodeseq.accept(this);
				if(nodeseq.elementAt(0) instanceof NodeChoice){
					switch(((NodeChoice)nodeseq.elementAt(0)).which) {
					case 0:
						timesVar=new ExpressionString(props).toString();
						expressionType=props.getJavaType().type;
						break;//out of ((NodeChoice)nodeseq.elementAt(0)).which
					case 1:
						timesVar=formatLiteral(literal).toString();
						break;//out of ((NodeChoice)nodeseq.elementAt(0)).which
					default:
					}
					timesVar=TranslationTable.getInstance().convertType(timesVar, Constants.INTEGER, expressionType);
					String tempVar="i"+new Integer(SymbolTable.tempVariablesMark++).toString().trim();
					sourceLst.add("for(int "+tempVar+"="+timesVar+
							";"+tempVar+">0;"+"--"+tempVar+") {");
					sourceLst.add("tab()");
					st.push("}");
				}
			}
			break;
		case 1:
			if(arg0.nodeChoice.choice instanceof NodeSequence) {
				NodeSequence nodeseq = (NodeSequence) arg0.nodeChoice.choice;
				NodeOptional nodeOpt=(NodeOptional)nodeseq.elementAt(0);
				beforeOrAfter=0;
				if(nodeOpt.present()) {
					nodeOpt.accept(this);
				}
				doReverseCondition=true;
				condition=new Stack<String>();;
				nodeseq.elementAt(2).accept(this);
				if(condition!=null&&condition.size()>0) {
					if(beforeOrAfter==0){	//Before
						sourceLst.add("while("+condition.pop()+") {");
						sourceLst.add("tab()");
						//20210703 update S
						st.push("}");
						//20210703 update E						
					} else {	//After
						sourceLst.add("do {");
						sourceLst.add("tab()");
						st.push("} while("+condition.pop()+");");
					}
				}
			}
			break;
		case 2:
			if(arg0.nodeChoice.choice instanceof NodeSequence) {
				endBlockScopeCriterion.push(st);//pushed ahead and returns instead of break
				NodeSequence nodeseq = (NodeSequence) arg0.nodeChoice.choice;
				NodeOptional nodeOpt=(NodeOptional)nodeseq.elementAt(0);
				beforeOrAfter=0;
				if(nodeOpt.present()) {
					nodeOpt.accept(this);
				}
				nodeseq.elementAt(2).accept(this);
				return;
			}
		default:
		}
		endBlockScopeCriterion.push(st);
}
	
	private  Stack<Stack<String>> endBlockScopeCriterion=new Stack<Stack<String>>();
	
	@Override
	public void visit(PerformProcedure arg0) {
		if(arg0.nodeOptional.present()) {
			arg0.nodeOptional.accept(this);
			procedureThruProps=procedureProps;
		} else {
			procedureThruProps=null;
		}
		arg0.procedureName.accept(this);
	}

	@Override
	public void visit(PerformStatement n) {
		super.visit(n);
	}

	@Override
	public void visit(PerformTest arg0) {
		super.visit(arg0);
	}

	private  int beforeOrAfter =0;
	
	@Override
	public void visit(BeforeOrAfter n) {
		beforeOrAfter=n.nodeChoice.which;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(PerformVarying arg0) {

		arg0.identifier.accept(this);
		SymbolProperties saveProps=props;
		idOrLiteralList=new Vector();
		String fromVar,byVar;
		arg0.idOrLiteral.accept(this);
		IdOrLit o =idOrLiteralList.get(0);
		if(o.Id()!=null) {
			fromVar=new ExpressionString(o.Id()).toString();
			expressionType=o.Id().getJavaType().type;
		} else {
			fromVar=formatLiteral(new ExpressionString(o.Lit())).toString();
		}
		expression.push(fromVar);
		String init = TranslationTable.getInstance().getAssignString(saveProps, null, false,false);
		idOrLiteralList=new Vector();
		arg0.idOrLiteral1.accept(this);
		o =idOrLiteralList.get(0);
		
		if(o.Id()!=null) {
			byVar=new ExpressionString(o.Id()).toString();
			expressionType=o.Id().getJavaType().type;
		} else {
			byVar=formatLiteral(new ExpressionString(o.Lit())).toString();
		}

		expression.push(byVar);
		TranslationTable.getInstance().doIdOp(saveProps, "+");
		String incr = TranslationTable.getInstance().getAssignString(saveProps, null, false,false);
		
		if(beforeOrAfter==0) {
			doReverseCondition=true;
			condition=new Stack<String>();
			arg0.condition.accept(this);			
			sourceLst.add("for("+init+";"+condition.pop()+";"+incr+") {");
			sourceLst.add("tab()");
			endBlockScopeCriterion.peek().add("}");
		} else {
			doReverseCondition=false;
			condition=new Stack<String>();
			arg0.condition.accept(this);
			sourceLst.add(init+";");
			sourceLst.add("do {");
			sourceLst.add("tab()");
			endBlockScopeCriterion.peek().add("} while(true);");
			endBlockScopeCriterion.peek().add(incr+";");
			endBlockScopeCriterion.peek().add("if("+condition.pop()+") break;");
		}
	}

	
	@Override
	public void visit(IntegerConstant n) {
		if(doLiteral||doNumericConstant)
			super.visit(n);
		else
			literal=new ExpressionString(((NodeToken)n.nodeChoice.choice).tokenImage);
	}

	@Override
	public void visit(NonNumericConstant n) {
		switch(n.nodeChoice.which) {
		case 0:
			literal=new ExpressionString(((NodeToken)n.nodeChoice.choice).tokenImage);
			break;
		case 1:
			literal=new ExpressionString(((NodeToken)n.nodeChoice.choice).tokenImage);
			literal=new ExpressionString("0"+literal.toString().replace("\'", "").replace("\"",""));
		}
	}

	@Override
	public void visit(FigurativeConstant n) {
		String lit=null;
		switch(n.nodeChoice.which) {
			case 0: 
			case 1: 
			case 2: lit="0";break;
			case 3: 
			case 4: lit="SPACES";break;
			case 5: 
			case 6:
				lit="__function__"+String.valueOf(50)+"__()";
				expressionType=Constants.STRING;
				break;
			case 7: 
			case 8: 
				lit="__function__"+String.valueOf(51)+"__()";
				expressionType=Constants.STRING;
				break;
			case 9: 
			case 10: lit="\"\\\"\"";break;
			case 11: 
			case 12:
			default:
					lit="0";break;
		}
		if(doLiteral)
			literal.literal.append(lit);
		else
			literal=new ExpressionString(lit);
	}

	@Override
	public void visit(PerformVaryingList arg0) {
		//super.visit(arg0);
		arg0.performVarying.accept(this);
		for (int i=0;((i<4)&&arg0.nodeListOptional.present()&&
				i<arg0.nodeListOptional.size());++i) {
			NodeSequence nodeseq=(NodeSequence)arg0.nodeListOptional.elementAt(i);
			nodeseq.elementAt(1).accept(this);
		}
	}

	@Override
	public void visit(StringStatement n) {

		StringBuffer line=new StringBuffer("");
		StringBuffer single=new StringBuffer("");
		n.identifier.accept(this);
		if(props==null) return;
		SymbolProperties saveProps=props;
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			String delimitedBy=null;
			NodeSequence nodeseq=(NodeSequence) e.nextElement();
			if(((NodeOptional)nodeseq.elementAt(1)).present()) {
				NodeSequence opt=(NodeSequence)((NodeOptional)nodeseq.elementAt(1)).node;
				NodeChoice nodech=(NodeChoice)opt.elementAt(2);
				nodech.accept(this);
				switch(nodech.which) {
				case 0:
					delimitedBy=new ExpressionString(props).toString();break;
				case 1:
					delimitedBy=formatLiteral(literal).toString(); break;
				default:delimitedBy=null;
				}
			}
			for(Enumeration<Node> e2=((NodeList)nodeseq.elementAt(0)).elements();e2.hasMoreElements();) {
				props=null;literal=null;
				NodeChoice nodechoice=(NodeChoice)e2.nextElement();
				nodechoice.accept(this);
				switch(nodechoice.which) {
				case 0:
					single.append(TranslationTable.getInstance().
							converSymbolToStringType(props));
					break;
				case 1:
					single.append(TranslationTable.getInstance().convertType(
							formatLiteral(literal).toString(), 
							Constants.STRING, expressionType));
				}

				if(delimitedBy!=null)
					line.append("__delimitedBy(").append(single).append(','+delimitedBy).append(')');
				else
					line.append(single);
				if(e2.hasMoreElements()){
					line.append('+');
				}
				single.setLength(0);
			}
			if(e.hasMoreElements()){
				line.append('+');
			}
		}
		if(line.length()>0) {
			expression.push(line.toString());
			expressionType=Constants.STRING;
			sourceLst.add(TranslationTable.getInstance().getAssignString(saveProps, null,false));
		}
	}

	private SymbolProperties inspectSourceSym=null;
	private SymbolProperties inspectCounterSym=null;
	//These two lists should match
	
	@Override
	public void visit(InspectStatement n) {
		n.identifier.accept(this);
		inspectSourceSym=props;
		super.visit(n);
	}
	
	/*
	 *  
  <REPLACING>
  ( 
	  	<CHARACTERS> <BY> 
	  		( Identifier() | Literal() ) ( BeforeAfterPhrase() )*
	  | ( <ALL> | <LEADING> | <FIRST> ) 
	  		( ( Identifier() | Literal() )
	                          <BY> ( Identifier() | Literal() )
	                                      ( BeforeAfterPhrase() )*
	        )+
  )+
	 */

    /*
  <TALLYING>
  ( Identifier() <FOR> ( <CHARACTERS> ( BeforeAfterPhrase() )*  | 
  						 ( <ALL> | <LEADING> ) 
  						 		( 
  						 			( Identifier() | Literal() ) ( BeforeAfterPhrase() )*
            	                )+
                       )+
  )+
     */

	@SuppressWarnings("unchecked")
	@Override
	public void visit(TallyingPhrase n) {
		aLine.push(new StringBuffer());
		for(Enumeration e=n.nodeList.elements();e.hasMoreElements();) {
			aLine.peek().append("new CobolString("+new ExpressionString(inspectSourceSym).toString()+")");
			NodeSequence seq=(NodeSequence)e.nextElement();
			seq.elementAt(0).accept(this);
			inspectCounterSym=props;
			NodeList nodelist=(NodeList)seq.elementAt(2);
			//String tempLine=aLine;
			for(Enumeration e2=nodelist.elements();e2.hasMoreElements();) {
				NodeChoice nodechoice=(NodeChoice)e2.nextElement();
				if(nodechoice.which==0) {
					nodechoice.choice.accept(this);
					aLine.peek().append(".tally()");
				} 
				else {
					String thisChar="";
					NodeSequence seq2=(NodeSequence)nodechoice.choice;
					NodeChoice allLeading=(NodeChoice)seq2.elementAt(0);
					switch(allLeading.which) {
					case 0:aLine.peek().append(".all()");	break;
					case 1:aLine.peek().append(".leading()");break;
					default:continue;
					}
					NodeList list=(NodeList)seq2.elementAt(1);
					for(Enumeration e3=list.elements();e3.hasMoreElements();) {
						NodeSequence seq3=(NodeSequence) e3.nextElement();
						seq3.elementAt(1).accept(this);
						thisChar=doInspectPhrase1(seq3.elementAt(0));
						aLine.peek().append(".tally(").append(thisChar).append(")");
					}
				}
			}
			aLine.peek().append(".tallyCount()");
			aLine.peek().append("+").append(new ExpressionString(inspectCounterSym).toString());
			line=NameUtil.getJavaName(inspectCounterSym,true).replace("%0",aLine.peek().toString())+";";
			sourceLst.add(line);
			aLine.peek().setLength(1);aLine.peek().setCharAt(0, '\t');
		}
		aLine.pop();
		n.nodeOptional.accept(this);
	}
	@Override
	public void visit(ReplacingPhrase n) {
		String byChar,thisChar;
		aLine.push(new StringBuffer());
		aLine.peek().append("new CobolString("+new ExpressionString(inspectSourceSym).toString()+")");
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			NodeChoice nodechoice = (NodeChoice) e.nextElement();
			NodeSequence seq=(NodeSequence)nodechoice.choice;
			if(nodechoice.which==0) {
				byChar=doInspectPhrase1(seq.elementAt(2));
				nodechoice.choice.accept(this);
				aLine.peek().append(".replace(").append(byChar).append(")");
			}
			else {
				NodeSequence seq2=(NodeSequence)nodechoice.choice;
				NodeChoice allLeading=(NodeChoice)seq2.elementAt(0);
				switch(allLeading.which) {
				case 0:aLine.peek().append(".all()");	break;
				case 1:aLine.peek().append(".leading()");break;
				case 2:aLine.peek().append(".first()");break;
				default:continue;
				}
				NodeList list=(NodeList)seq2.elementAt(1);
				for(Enumeration<Node> e2=list.elements();e2.hasMoreElements();) {
					NodeSequence seq3=(NodeSequence) e2.nextElement();
					seq3.elementAt(3).accept(this);
					thisChar=doInspectPhrase1(seq3.elementAt(0));
					byChar=doInspectPhrase1(seq3.elementAt(2));
					aLine.peek().append(".replace(").append(thisChar).append(',').append(byChar).append(")");
				}
			}
		}
		expression.push(aLine.peek().append(".toString()").toString());
		expressionType=CobolSymbol.STRING;
		line=TranslationTable.getInstance().getAssignString(inspectSourceSym,null,false,true);
		sourceLst.add(line);
		aLine.pop();
	}


	@Override
	public void visit(ConvertingPhrase n) {
		String from=doInspectPhrase1(n.nodeChoice);
		String to=doInspectPhrase1(n.nodeChoice1);
		aLine.push(new StringBuffer());
		aLine.peek().append("new CobolString("+new ExpressionString(inspectSourceSym)+")");
		n.nodeListOptional.accept(this);
		aLine.peek().append(".convert(").append(from).append(',').append(to).append(')').append(".toString()");
		line=NameUtil.getJavaName(inspectSourceSym,true).replace("%0",aLine.peek().toString())+";";
		sourceLst.add(line);
		aLine.pop();
	}
	
	//private String inspectLine=null;
    private String doInspectPhrase1(Node n) {
    	props=null;literal=null;
    	n.accept(this);
    	if(props!=null){
    		return	TranslationTable.getInstance().convertType(
    					new ExpressionString(props).toString(), 
    					CobolSymbol.STRING, 
    					props.getJavaType().type);
			}
    	else
    		if(literal!=null) {
    			 literal=formatLiteral(literal);
    			return TranslationTable.getInstance().convertType(
    					literal.toString(), 
    					CobolSymbol.STRING, 
    					expressionType);
    		}
    	return "";
    }
        
	@Override
	public void visit(BeforeAfterPhrase n) {
		if(aLine==null) return;
		props=null;literal=null;
		n.nodeChoice1.accept(this);
		if(n.nodeChoice.which==0) {
			aLine.peek().append(".before(");
                } else {
			aLine.peek().append(".after(");
		}
		if(props!=null) {
			aLine.peek().append(new ExpressionString(props).toString());
		} else {
			if(literal!=null){
				aLine.peek().append(formatLiteral(literal).toString());
			}
		}
		aLine.peek().append(")");
	}

	@Override
	public void visit(SetStatement n) {
		SymbolProperties saveProps=null;props=null;literal=null;
		n.nodeChoice.accept(this);
		saveProps=props;
		for (Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			e.nextElement().accept(this);
			if(props==null) continue;
			switch(n.nodeChoice.which) {
			case 0:
				NodeChoice nodech=(NodeChoice)((NodeSequence)n.nodeChoice.choice).elementAt(1);
				switch(nodech.which) {
				case 0://Identifier
					if(saveProps==null) continue;
					Cobol2Java.getInstance().expression.push(new ExpressionString(saveProps).toString());
					Cobol2Java.getInstance().expressionType=saveProps.getJavaType().type;
					sourceLst.add(TranslationTable.getInstance().getAssignString(props, null, false));
					continue;
				case 1://True
					if(props.getLevelNumber()==88) {
						String onValue=props.getValues().get(0).value1.toString();
						if(onValue!=null) {
							expression.push( Cobol2Java.getInstance().formatLiteral(new ExpressionString(onValue)).toString());
							sourceLst.add(TranslationTable.getInstance().getAssignString(props.getParent(), null, 
								props.getParent().getJavaType().type==Constants.STRING||props.getJavaType().type==Constants.GROUP));
						}
					}
					continue;
				case 2://False - Not geiven Enterprise Cobol
				case 3://On
				case 4://Of
				case 5://Literal
					if(literal==null) continue; 
					Cobol2Java.getInstance().expression.push(Cobol2Java.getInstance().
							formatLiteral(literal).toString());
					sourceLst.add(TranslationTable.getInstance().getAssignString(props, null, false));
					continue;
				}
			case 1:
				NodeSequence nodeseq=(NodeSequence) n.nodeChoice.choice;
				String op=null;
				switch(((NodeChoice)nodeseq.elementAt(0)).which) {
					case 0 :
						op="+";break;
					case 1:
						op="-";
				}
				if(op==null) return;
				switch(((NodeChoice)nodeseq.elementAt(2)).which) {
				case 0:
					if(saveProps==null) continue;
					Cobol2Java.getInstance().expression.push(new ExpressionString(saveProps, false)+op+new ExpressionString(props).toString());
					Cobol2Java.getInstance().expressionType=saveProps.getJavaType().type;
					sourceLst.add(TranslationTable.getInstance().getAssignString(props, null, false));
					continue;
				case 1:
					if(literal==null) continue; 
					Cobol2Java.getInstance().expression.push(Cobol2Java.getInstance().
							formatLiteral(literal).toString()+op+new ExpressionString(props).toString());
					sourceLst.add(TranslationTable.getInstance().getAssignString(props, null, false));
					continue;
				}
			}
			
		}
		//super.visit(n);
	}
	
	private  TreeToCommentFormatter dumper=new TreeToCommentFormatter(false);
    public  boolean isLastGotoStatement=false; 
    
	@Override
	public void visit(StatementList n) {
		isLastGotoStatement=false;
		super.visit(n);
	}

	@Override
	public void visit(Statement n) {
		initExpressionType();
		if(n.deadStatement)
			return;
		isLastGotoStatement=n.nodeChoice.choice instanceof GotoStatement;
		isLastExitStatement=false;
		if(RESConfig.getInstance().isPrintCobolStatementsAsComments()
				||RESConfig.getInstance().isRetainCobolComments()) {
			dumper.startAtNextToken();
			dumper.visit(n);
			if(RESConfig.getInstance().isPrintCobolStatementsAsComments())
				sourceLst.add("");
			if(execSql2Java!=null)
				execSql2Java.setStatementNode(n);
		}
		super.visit(n);
	}
	
    @SuppressWarnings("unchecked")
	public void initializeSymbols() {
    	
		/*
		 * ClassFile.doMethodScope("public " +
		 * SymbolTable.getScope().getCurrentProgram().getJavaName2() + "() {"); if
		 * (SymbolTable.getScope().getCurrentProgram().getAdjustedLength() > 0) if
		 * (SymbolTable.getScope().getCurrentProgram().getNoLivingFillers() > 0)
		 * sourceLst.add("\tsuper(" +
		 * String.valueOf(SymbolTable.getScope().getCurrentProgram().getNoLivingFillers(
		 * )) + "," + "new CobolBytes(" +
		 * String.valueOf(SymbolTable.getScope().getCurrentProgram().getAdjustedLength()
		 * ) + "));"); else sourceLst.add("\tsuper(" + "new CobolBytes(" +
		 * String.valueOf(SymbolTable.getScope().getCurrentProgram().getAdjustedLength()
		 * ) + "));"); else ; sourceLst.add("}");
		 * sourceLst.add("public void initialize(Program p) {"); sourceLst.add("tab()");
		 * sourceLst.add("if(__initialized) return; else __initialized=true;");
		 * sourceLst.add("__setProgram(p);"); if
		 * (Main.getContext().isDecimalPointIsComma())
		 * sourceLst.add("__setDecimalPointIsComma();"); if
		 * (Main.getContext().getCurrencySign() != null)
		 * sourceLst.add("__setCurrencySign(" + Main.getContext().getCurrencySign()
		 * + ");"); ArrayList a = (ArrayList)
		 * SymbolTable.getScope().getCurrentProgram().getChildren(); if (a != null) {
		 * for (Iterator e = a.iterator(); e.hasNext();) { SymbolProperties o2 =
		 * (SymbolProperties) e.next(); if (!(o2.getRef() || o2.getMod())) continue;
		 * initializeOneSymbol(o2); } } sourceLst.add("backTab()"); //ClassFile.endMethodScope();
		 */
		//}
		sourceLst.add("backTab()");
    	//ClassFile.endMethodScope();
    }
    
	public void initializeOneSymbol(SymbolProperties initProps)
	{
    	//try{
		
	    	if(initProps==null||
	    			!(initProps.getRef()||initProps.getMod())||
	    			initProps.getType()==SymbolConstants.PARAGRAPH||
	    			initProps.getType()==SymbolConstants.SECTION) return ;
	    	if(initProps.getIsSuppressed()) {
	    		return;
	    	}
	    	int lvl=initProps.getLevelNumber();
	    	
	    	if(lvl==88||lvl==66||lvl==78) return;

	    	if(initProps.is01Group()){
	    		if(initProps.getValues()!=null&&initProps.getValues().size()>0) {
	    			SymbolProperties.CoupleValue values=initProps.getValues().get(0);
					expression.push(formatLiteral(values.value1).toString());
					String exprString=TranslationTable.getInstance().getAssignString( initProps, null,false);
					exprString=exprString.substring(exprString.substring(0,exprString.indexOf('(')+1).indexOf('.')+1);
					sourceLst.add(exprString);
	    		} else 
	    			sourceLst.add(initProps.getJavaName1()+".initialize(this);");
	    		return;
	    	}
	    	
			if(initProps.getValues()!=null&&initProps.getValues().size()>0) {
				SymbolProperties.CoupleValue values=initProps.getValues().get(0);
				String temp=formatLiteral(values.value1).toString();
				if(values.value1.isAll)
					temp="__all("+temp+','+String.valueOf(initProps.getLength())+")";
				expression.push(temp);
				if(!initProps.getIsFiller()&&initProps.getNoOccursSubscripts()>0) {
					int idx=1;
					if(initProps.getIndexesWorkSpace()!=null)
						initProps.getIndexesWorkSpace().clear();
					else
						initProps.setIndexesWorkSpace(new ArrayList<String>());
					for(SymbolProperties par:initProps.getOccursParents()) {
						String idxStr="idx"+String.valueOf(idx++);
						sourceLst.add("for(int "+idxStr+"=1;"+
								idxStr+"<="+String.valueOf(par.getMaxOccurs())+
								";++"+idxStr+") {");
						sourceLst.add("tab()");
						initProps.getIndexesWorkSpace().add(idxStr);
					}
				}
				
				if(!initProps.getIsFiller()) {
					temp=TranslationTable.getInstance().getAssignString( initProps, null,false);
					temp=temp.substring(temp.substring(0,temp.indexOf('(')+1).indexOf('.')+1);
				} else {
					String exprString=TranslationTable.getInstance().convertType(expression.pop3(), initProps.getJavaType().type, expressionType);
					temp=SymbolUtil.getInstance().getLocalFillerSetter(initProps,exprString)+';';
				}
				

				if(temp!=null&&temp.trim().length()>0)
					sourceLst.add(temp);
				if(!initProps.getIsFiller()&&initProps.getNoOccursSubscripts()>0) {
					initProps.getIndexesWorkSpace().clear();
					for(int idx=0;idx<initProps.getNoOccursSubscripts();++idx) {
						sourceLst.add("backTab()");
						sourceLst.add("}");
					}
				}
			}	    	
    	//} catch(Exception e) {
    		//e.printStackTrace();
    	//}
    	return ;
    
	}
	
	private String doOneAll(ExpressionString lit,int len) {
		if(lit.isAll&&len>0) 
			return lit.literal.insert(0,"__all(").append(',').append(len).append(')').toString();
		else
			return lit.toString();
	}
	
	/** Level-88 condition getters waiting to be written into the generated class body. */
	public static final ArrayList<String> level88Getters = new ArrayList<String>();

	public void doOne88(SymbolProperties o1) {
		try{
			if(o1==null) return;
			if(o1.getIsSuppressed())
				return;

			int lvl=o1.getLevelNumber();

			if(lvl!=88||!(o1.getParent().getRef()||o1.getParent().getMod())) return;

			if(o1.getValues()==null||o1.getValues().size()<=0) return;
			String getSubscripts="";
			if(o1.getParent().isOccurs()||o1.getParent().isAParentInOccurs()) {
				int size;
				if(o1.getParent().getOccursParents()!=null&&(size=o1.getParent().getOccursParents().size())>0) {
					for(int i=1; i<=size;++i)
						getSubscripts+="int idx"+String.valueOf(i)+((i!=size)?",":"");
				}
			}

			// The whole getter is written here, header to closing brace. It used to be split: the
			// header went to the class file and the body into sourceLst, which is flushed into
			// whatever statement comes next - leaving a method with no body and a stray return
			// inside execute(). Operands are rendered like every other condition (Compare.eq(...)),
			// and the value is read through the same accessor chain (model.getX()).
			String operand=NameUtil.getJavaName(o1.getParent(),false);
			int length=o1.getParent().getLength();
			StringBuilder condition=new StringBuilder();
			for(SymbolProperties.CoupleValue values: o1.getValues()) {
				String one;
				String low=doOneAll(formatLiteral(values.value1),length);
				if(values.value2==null) {
					one="Compare.eq("+operand+","+low+")";
				} else {
					String high=doOneAll(formatLiteral(values.value2),length);
					one="(Compare.ge("+operand+","+low+") && Compare.le("+operand+","+high+"))";
				}
				if(condition.length()>0) condition.append(" || ");
				condition.append(one);
			}

			// Collected here and written into the Tasklet body by printTaskletDeclare: the data
			// division is walked before the class is opened, so printing now lands above "package".
			level88Getters.add("public boolean get"+o1.getJavaName2()+"(" +getSubscripts+") {");
			level88Getters.add("\treturn "+condition+";");
			level88Getters.add("}");
			level88Getters.add("");
		} catch(Exception e) {
			e.printStackTrace();
		}
		return ;

	}

	private ExecSql2Java execSql2Java = new ExecSql2Java(this);
	
	@Override
	public void visit(ExecSqlStatement n) {
		execSql2Java.setSourceLst(sourceLst);
		execSql2Java.visit(n);
		sourceLst= execSql2Java.getSourceLst();
		//super.visit(n);
	}
	@Override
	public void visit(OpenStatement n) {
		doTry();
		boolean inp=false,op=false,iop=false,ext=false;
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			NodeChoice nodech=(NodeChoice)e.nextElement();
			NodeSequence nodeseq=(NodeSequence)nodech.choice;
			switch(((NodeToken)nodeseq.elementAt(0)).kind) {
			case CobolParserConstants.INPUT:
				inp=true;break;
			case CobolParserConstants.OUTPUT:
				op=true;break;
			case CobolParserConstants.INPUT_OUTPUT:
			case CobolParserConstants.I_O:
				iop=true;break;
			case CobolParserConstants.EXTEND:
				ext=true;break;
			default:
			}
			for(Enumeration<Node> e2=((NodeList)nodeseq.elementAt(1)).elements();
						e2.hasMoreElements();) {
				e2.nextElement().accept(this);
				if (props == null)
					continue;
				if (inp)
					//sourceLst.add(props.getJavaName1() + ".openInput();");
					// QuyTV update method Input/Open file
					sourceLst.add(props.getJavaName1() + ".openI();");
				if (op)
					//sourceLst.add(props.getJavaName1() + ".openOutput();");
					// QuyTV update method Input/Open file
					sourceLst.add(props.getJavaName1() + ".openO();");
				if (iop)
					sourceLst.add(props.getJavaName1() + ".openIO();");
				if (ext)
					sourceLst.add(props.getJavaName1() + ".openExtend();");

			}
			inp = op = iop = ext = false;
		}
		printFileStatus(props);
		sourceLst.add("backTab()");
		sourceLst.add("} catch(java.io.IOException ioe) {");
		printFileStatus(props);
		exceptionPrintStackTrace("ioe");
		String declConditions=props.getDataName();
		if(props.isFileOpenedInput()) {
			declConditions+=",1";
		}
		if(props.isFileOpenedOutput()) {
			declConditions+=",2";
		}
		if(props.isFileOpenedIO()) {
			declConditions+=",3";
		}
		if(props.isFileOpenedExtend()) {
			declConditions+=",4";
		}
		doDeclaratives(declConditions.split(","));
		sourceLst.add("}");
	}
	private void doDeclaratives(String... declConditions) {
		if(!isDeclarative&&useConditions!=null) {
			for(UseCondition u: useConditions) {
				for(String s:declConditions) {
					if((u.openCondition!=null&&u.openCondition.equalsIgnoreCase(s)&&u.isError)) {
						sourceLst.add("doCobolPerform("+u.section.getJavaName1()+",null);");
						return;
					}
				}
			}
		}
	}

	@Override
	public void visit(ReadStatement n) {
		n.fileName.accept(this);
		if(props==null) return;
		SymbolProperties fileProps=props;
	
		if(fileProps.getChildren()==null||fileProps.getChildren().size()<=0) return;
		props=fileProps.getChildren().get(0);
		if(props.getLevelNumber()!=1) return;
		doTry();
		String assign=fileProps.getJavaName1()+".read";
		if(fileProps.isIndexedFile()) {
			if(n.nodeOptional.present()&&fileProps.getOtherData1()==Constants.DYNAMIC_ACCESS) {
				assign+="Next";
			}
			assign+="()";
		} else {
			if(props.is01Group()) {
				assign=assign+"("+props.getJavaName1();
			} else {
				assign=assign+"(super.getProgram().get()," +props.getOffset()+','+props.getLength();
			}
			if(fileProps.getOtherData2()!=null)
				assign+="," +
						NameUtil.getJavaName((SymbolProperties) fileProps.getOtherData2(),false);
			assign+=")";
		}
		sourceLst.add(assign+";");
		if(n.nodeOptional2.present()) {
			saveProps=props;props=null;
			n.nodeOptional2.accept(this);
			if(props!=null) {
				sourceLst.add(TranslationTable.getInstance().getAssignString(props, saveProps, false));
			}
		} 
		printFileStatus(fileProps);
		n.nodeOptional5.accept(this);
		n.nodeOptional7.accept(this);
		sourceLst.add("backTab()");
		String declConditions=fileProps.getDataName();
		if(fileProps.isFileOpenedInput()) {
			declConditions+=",1";
		}
		if(fileProps.isFileOpenedIO()) {
			declConditions+=",3";
		}
		sourceLst.add("} catch(java.io.EOFException ioe) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ioe");
		printFileStatus(fileProps);
		if(!n.nodeOptional6.present()) {
			doDeclaratives(declConditions.split(","));
		} else {
			n.nodeOptional6.accept(this);
		}
		sourceLst.add("backTab()");
		sourceLst.add("} catch(InvalidKeyException ike) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ike");
		printFileStatus(fileProps);
		if(n.nodeOptional4.present()) {
			n.nodeOptional4.accept(this);
		} else {
			doDeclaratives(declConditions.split(","));
		}
		sourceLst.add("backTab()");
		sourceLst.add("} catch(java.io.IOException ioe) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ioe");
		doDeclaratives(declConditions.split(","));
		sourceLst.add("backTab()");
		sourceLst.add("}");
	}

	private SymbolProperties saveProps=null;
	
	@Override
	public void visit(WriteStatement n) {
		n.recordName.accept(this);
		if(props==null) return;
		SymbolProperties fileProps=props.getParent();
		saveProps=props;
		if(!fileProps.isFile()) return;
		doTry();
		if(n.nodeOptional.present()) {
			saveProps=props;props=null;literal=null;
			n.nodeOptional.accept(this);
			if(props!=null) {
				sourceLst.add(TranslationTable.getInstance().getAssignString(saveProps, props, false));
			} else {
				literal=formatLiteral(literal);
				expression.push(literal.toString());
				sourceLst.add(TranslationTable.getInstance().getAssignString(saveProps, null, false));
			}
		} 
		props=saveProps;
		if(fileProps.isIndexedFile()) {
			sourceLst.add(fileProps.getJavaName1()+".write();");
		} else {
			if(n.nodeOptional1.present()&&((AdvancingPhrase)n.nodeOptional1.node).nodeChoice.which==1) 
				n.nodeOptional1.accept(this);
			if(n.nodeOptional.present()) {
				props=null;literal=null;
				n.nodeOptional.accept(this);
			}
			if(props!=null||literal!=null) {
				String assign;
				if(props==null) {
					assign=formatLiteral(literal).toString();
					assign=TranslationTable.getInstance().convertType(assign,CobolSymbol.GROUP, expressionType);
				} else {
					assign=new ExpressionString(props).toString();
					assign=TranslationTable.getInstance().convertType(assign,CobolSymbol.GROUP, props.getJavaType().type);
				}
				sourceLst.add(fileProps.getJavaName1()+".write("+assign+");");
			}
			if(n.nodeOptional1.present()&&((AdvancingPhrase)n.nodeOptional1.node).nodeChoice.which==0) 
				n.nodeOptional1.accept(this);
		}
		printFileStatus(fileProps);
		n.nodeOptional5.accept(this);
		String declConditions=fileProps.getDataName();
		if(fileProps.isFileOpenedOutput()) declConditions+=",2";
		if(fileProps.isFileOpenedIO()) declConditions+=",3";
		if(fileProps.isFileOpenedExtend()) declConditions+=",4";
		
		sourceLst.add("backTab()");
		sourceLst.add("} catch(InvalidKeyException ike) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ike");
		printFileStatus(fileProps);
		if(!n.nodeOptional4.present()) {
			doDeclaratives(declConditions.split(","));
		} else {
			n.nodeOptional4.accept(this);
		}
		sourceLst.add("backTab()");
		sourceLst.add("} catch(java.io.IOException ioe) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ioe");
		printFileStatus(fileProps);
		doDeclaratives(declConditions.split(","));
		sourceLst.add("backTab()");
		sourceLst.add("}");
	}

	@Override
	public void visit(AdvancingPhrase n) {
		if(saveProps==null) return;
		switch(n.nodeChoice1.which){
		case 0:
			sourceLst.add(saveProps.getParent().getJavaName1()+".advancePage();");break;
		case 1:
			props=null;
			n.nodeChoice1.choice.accept(this);
			if(props!=null) {
				expression.push(new ExpressionString(props).toString());
				expressionType=props.getJavaType().type;
			}
			else {
				expression.push(formatLiteral(literal).toString());
			}
			sourceLst.add(saveProps.getParent().getJavaName1()+".advanceLines(" +
				TranslationTable.getInstance().convertType(expression.pop3(), CobolSymbol.INTEGER, expressionType)+");");
			props=saveProps;
			break;
		case 2:
			n.nodeChoice1.accept(this);
			literal=formatLiteral(literal);
			sourceLst.add(saveProps.getParent().getJavaName1()+".advanceLines(" +literal.toString()+
					");");
			break;
		}
	}
	@Override
	public void visit(RewriteStatement n) {

		n.recordName.accept(this);
		if(props==null) return;
		SymbolProperties fileProps=props.getParent();
		props=fileProps.getChildren().get(0);
		if(!fileProps.isFile()) return;
		if(n.nodeOptional.present()) {
			props=null;literal=null;
			n.nodeOptional.accept(this);
			if(props==null&&literal==null) return;
		}
		doTry();
		String assign;
		assign="";
		if(fileProps.isIndexedFile()) {
			assign="";
			if(!fileProps.getChildren().get(0).getDataName().equalsIgnoreCase(props.getDataName())) {
				expression.push(new ExpressionString(props).toString());
				sourceLst.add(TranslationTable.getInstance().getAssignString(fileProps.getChildren().get(0),null,false));
			}
		} else 
		if(props==null) {
			assign=formatLiteral(literal).toString();
			assign=TranslationTable.getInstance().convertType(assign,CobolSymbol.GROUP, expressionType);
		} else {
			assign=new ExpressionString(props).toString();
			assign=TranslationTable.getInstance().convertType(assign,CobolSymbol.GROUP, props.getJavaType().type);
		}
		sourceLst.add(fileProps.getJavaName1()+".rewrite("+assign+");");
		printFileStatus(fileProps);
		n.nodeOptional2.accept(this);
		sourceLst.add("backTab()");
		String declConditions=fileProps.getDataName();
		if(fileProps.isFileOpenedOutput()) {
			declConditions+=",2";
		}
		if(fileProps.isFileOpenedIO()) {
			declConditions+=",3";
		}
		if(props.isFileOpenedExtend()) {
			declConditions+=",4";
		}
		sourceLst.add("} catch(InvalidKeyException ike) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ike");
		printFileStatus(fileProps);
		if(!n.nodeOptional1.present()) {
			doDeclaratives(declConditions.split(","));
		} else {
			n.nodeOptional1.accept(this);
		}
		sourceLst.add("backTab()");
		sourceLst.add("} catch(java.io.IOException ioe) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("ioe");
		printFileStatus(fileProps);
		doDeclaratives(declConditions.split(","));
		sourceLst.add("backTab()");
		sourceLst.add("}");
	}
	
	@Override
	public void visit(StartStatement n) {
		doTry();
		n.fileName.accept(this);
		if(props==null) return;
		SymbolProperties fileProps = props;props=null;
		if(n.nodeOptional.present()) {
			SymbolProperties record=null;
			for(SymbolProperties k:fileProps.getChildren()) {
				if(!k.isIndexedFileRecord())continue;
				record=k;break;
			}
			((NodeSequence)n.nodeOptional.node).elementAt(3).accept(this);
			if(props!=null&&record!=null) {
				SymbolProperties pk = (SymbolProperties) fileProps.getOtherData2();
				String keyStr=record.getJavaName1()+".primaryKey()";
				if(pk==null||!(pk.getOffset()==props.getOffset())) {
					keyStr=TranslationTable.getInstance().convertType(
							new ExpressionString(props), Constants.STRING, props.getJavaType().type).toString();
					keyStr+=","+String.valueOf(props.getOffset())+","+String.valueOf(props.getLength());
				}
				else
					if(pk.getLength()!=props.getLength()) {
						keyStr+=","+String.valueOf(props.getLength());
					}
				NodeChoice nodechoice = (NodeChoice)((NodeSequence)n.nodeOptional.node).elementAt(2);
				switch(nodechoice.which) {
				case 0:
				case 1:
					sourceLst.add(fileProps.getJavaName1()+".startEq("+keyStr+");");
					break;
				case 2://ge
				case 7://>=
				case 5://Not LESS
				case 6://Not <
					sourceLst.add(fileProps.getJavaName1()+".startGe(" +keyStr+");");
					break;
				case 3://gt
				case 4://>
					sourceLst.add(fileProps.getJavaName1()+".startGt("+keyStr+");");
					break;
				default:
					sourceLst.add(fileProps.getJavaName1()+".start();");
				}
			} else {
				sourceLst.add(fileProps.getJavaName1()+".start();");
			}
		} else {
			sourceLst.add(fileProps.getJavaName1()+".start();");
		}
		
		printFileStatus(fileProps);
		String declConditions=fileProps.getDataName();
		if(fileProps.isFileOpenedOutput()) {
			declConditions+=",2";
		}
		if(fileProps.isFileOpenedIO()) {
			declConditions+=",3";
		}
		if(fileProps.isFileOpenedExtend()) {
			declConditions+=",4";
		}
		sourceLst.add("backTab()");
		sourceLst.add("} catch(InvalidKeyException e) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("e");
		printFileStatus(fileProps);
		if(n.nodeOptional1.present())
			n.nodeOptional1.accept(this);
		else
			doDeclaratives(declConditions.split(","));
		sourceLst.add("backTab()");
		sourceLst.add("} catch(java.io.IOException e) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("e");
		printFileStatus(fileProps);
		sourceLst.add("backTab()");
		sourceLst.add("}");
	
	}
	@Override
	public void visit(DeleteStatement n) {
		doTry();
		n.fileName.accept(this);
		if(props==null) return;
		String arg="";
		if(!props.isIndexedFile())
			arg=NameUtil.getJavaName((SymbolProperties)props.getOtherData2(), false);
		sourceLst.add(props.getJavaName1()+".delete("+arg+");");
		n.nodeOptional2.accept(this);
		printFileStatus(props);
		String declConditions=props.getDataName();
		if(props.isFileOpenedOutput()) {
			declConditions+=",2";
		}
		if(props.isFileOpenedIO()) {
			declConditions+=",3";
		}
		if(props.isFileOpenedExtend()) {
			declConditions+=",4";
		}
		sourceLst.add("backTab()");
		sourceLst.add("} catch(InvalidKeyException e) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("e");
		printFileStatus(props);
		if(n.nodeOptional1.present())
			n.nodeOptional1.accept(this);
		else
			doDeclaratives(declConditions.split(","));
		sourceLst.add("backTab()");
		sourceLst.add("} catch(java.io.IOException e) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("e");
		printFileStatus(props);
		sourceLst.add("backTab()");
		sourceLst.add("}");
	}
	
	private void printFileStatus(SymbolProperties fileProps) {
		if(fileProps.getFileStatus()==null) return;
		if(fileProps.getFileStatus()==null||fileProps.getFileStatus().size()<=0) return;
		SymbolProperties fs=null;
		doit: {
			for(SymbolProperties s: fileProps.getFileStatus()) {
				if(findProgram(s).equals(SymbolTable.getScope().getCurrentProgram())) {
					fs=s;
					break doit;
				}
			}
			return;
		}
		expression.push(fileProps.getJavaName1()+".getFileStatus()");
		expressionType=CobolSymbol.STRING;
		doMove(ExpressionString.getExpression(), fs);
	}
	
	@Override
	public void visit(CloseStatement n) {
		boolean doExceptions=false;
		
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			((NodeSequence)e.nextElement()).elementAt(0).accept(this);
			if(props!=null) {
				if(!doExceptions) doTry();
				doExceptions=true;
				sourceLst.add(props.getJavaName1()+".close();");
				printFileStatus(props);
			}
		}
		if(doExceptions) { 
			sourceLst.add("backTab()");
			sourceLst.add("} catch(java.io.IOException ioe) {");
			sourceLst.add("tab()");
			exceptionPrintStackTrace("ioe");
			printFileStatus(props);
			String declConditions=props.getDataName();
			if(props.isFileOpenedInput()) {
				declConditions+=",1";
			}
			if(props.isFileOpenedOutput()) {
				declConditions+=",2";
			}
			if(props.isFileOpenedIO()) {
				declConditions+=",3";
			}
			if(props.isFileOpenedExtend()) {
				declConditions+=",4";
			}
			doDeclaratives(declConditions.split(","));
			sourceLst.add("backTab()");
			sourceLst.add("}");
		}
	}

	@Override
	public void visit(FileName n) {
		props=SymbolTable.getScope().lookup(n.cobolWord.nodeToken.tokenImage, SymbolConstants.FILE);
	}

	public void reportError(Node n,String msg) {
		System.out.println("@CobolSourceFile(\""+((RESNode)n).sourceFile+"\","+((RESNode)n).line+"):"+msg);
		System.exit(0);
	}
	
	public void reportError(Node n,String msg,boolean exit) {
		System.out.println("@CobolSourceFile(\""+((RESNode)n).sourceFile+"\","+((RESNode)n).line+"):"+msg);
		if(exit)System.exit(0);
	}

	public  SymbolProperties getCurrentProgram() {
		return SymbolTable.getScope().getCurrentProgram();
	}

	public  String getAssignString(TranslationTable translationTable,
			SymbolProperties lhs, SymbolProperties rhs) {
		return translationTable.getAssignString(lhs, rhs,false);
	}

	public void doTail() {
		initializeSymbols();
		sourceLst.add("backTab()");
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void visit(CallStatement n) {
		if(idOrLiteralList!=null)
			idOrLiteralList.clear();
		else
			idOrLiteralList=new Vector();
		StringBuffer line=new StringBuffer("call");
		StringBuffer argLine =new StringBuffer();
		n.nodeChoice.choice.accept(this);
		switch(n.nodeChoice.which) {
		case 0:
			break;
		case 1:
			line.append(NameUtil.convertCobolNameToJava(formatLiteral(literal).toString().replaceAll("\"",""),true));
			line.append("();");
			break;
		}
		sourceLst.add(line.toString());
		n.nodeOptional.accept(this);
		for (Enumeration<IdOrLit> e = idOrLiteralList.elements(); e.hasMoreElements();) {
			if (argLine.length()>0) argLine.append(";");
			//line.append(',');
			argLine.append(e.nextElement().get());
		}
		taskletList.put(NameUtil.convertCobolNameToJava(formatLiteral(literal).toString().replaceAll("\"",""),true), argLine.toString());
		/* TuNT1 S
		doTry();
		if(idOrLiteralList!=null)
			idOrLiteralList.clear();
		else
			idOrLiteralList=new Vector();
		StringBuffer line=new StringBuffer("doCobolCall(");
		bLine.push(new StringBuffer());
		cLine.push(new StringBuffer());
		callParmIdx=0;
		n.nodeChoice.choice.accept(this);
		switch(n.nodeChoice.which) {
		case 0:
			expression.push(new ExpressionString(props).toString());
			expressionType=props.getJavaType().type;
			break;
		case 1:
			expression.push(formatLiteral(literal).toString());
			break;
		}
		
		// QuyTV	
		// Add item to taskletList
		taskletList.add(literal.toString().toLowerCase().replace("\"", "")+"Tasklet");
		//line.append(TranslationTable.getInstance().convertType(expression.pop3(), CobolSymbol.STRING, expressionType));
		n.nodeOptional.accept(this);
		for (Enumeration<IdOrLit> e = idOrLiteralList.elements(); e.hasMoreElements();) {
			//line.append(',');
			line.append(e.nextElement().get());
		}
		line.append(");");
		sourceLst.add(line.toString());
		bLine.peek().append(cLine.peek().toString());
		sourceLst.add(bLine.peek().toString());
		n.nodeOptional3.accept(this);
		sourceLst.add("backTab()");
		bLine.pop();
		cLine.pop();
		sourceLst.add("} catch(Exception e) {");
		sourceLst.add("tab()");
		exceptionPrintStackTrace("e");
		n.nodeOptional1.accept(this);
		n.nodeOptional2.accept(this);
		sourceLst.add("backTab()");
		sourceLst.add("}");
		TuNT1 E*/
	}
	
	@Override
	public void visit(CancelStatement n) {
		aLine.push(new StringBuffer());
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			String exprString=null;
			props=null;literal=null;
			e.nextElement().accept(this);
			if(props!=null) {
				exprString=TranslationTable.getInstance().convertType(
					NameUtil.getJavaName(props,false),Constants.STRING,props.getJavaType().type);
			} else {
				literal=formatLiteral(literal);
				exprString=TranslationTable.getInstance().convertType(
						literal.toString(),Constants.STRING,expressionType);
			}
			if(aLine.peek().length()>0) aLine.peek().append(',');
			aLine.peek().append(exprString);
		}
		if(aLine.peek().length()>0){
			sourceLst.add("doCobolCancel("+aLine.peek().toString()+");");
		}
		aLine.pop();
	}

	@Override
	public void visit(CallByContentArgs n) {
		switch(n.nodeChoice.which) {
		case 0:
			n.nodeChoice.choice.accept(this);
			if(props==null) {
				idOrLiteralList.add(new IdOrLit("null"));
			} else
			if(((NodeOptional)((NodeSequence)n.nodeChoice.choice).elementAt(0)).present())
				idOrLiteralList.add(new IdOrLit(String.valueOf(props.getLength())));
			else
				idOrLiteralList.add(new IdOrLit(new ExpressionString(props).toString()));
			break;
		case 1:
			idOrLiteralList.add(new IdOrLit("null"));break;
		case 2:
			n.nodeChoice.choice.accept(this);
			idOrLiteralList.add(new IdOrLit(formatLiteral(literal).toString()));
		}
		callParmIdx++;
	}
	
	@Override
	public void visit(CallByReferenceArgs n) {
		
		switch(n.nodeChoice.which) {
		case 0:
		case 2:
			n.nodeChoice.choice.accept(this);
			if(props==null) {
				idOrLiteralList.add(new IdOrLit("null"));
			} else
			idOrLiteralList.add(new IdOrLit(props));
			expression.push("__get"+SymbolConstants.getSQL(props.getJavaType().type)+"Result("+
					String.valueOf(callParmIdx)+")");
			expressionType=props.getJavaType().type;
			if(props.isAChildHasDependingOn()||(props.isOccurs()&&props.getDependingOnOccurs()!=null))
				cLine.peek().append(TranslationTable.getInstance().getAssignString(props,null,false));
			else
				//TuNT1 bLine.peek().append(TranslationTable.getInstance().getAssignString(props,null,false));
			break;
		case 1:
			idOrLiteralList.add(new IdOrLit("null"));
			break;
		}
		callParmIdx++; 
	}
	private Hashtable<Integer,String> replacingValues;
	@Override
	public void visit(InitializeStatement n) {
	
		replacingValues= new Hashtable<Integer,String>();
		if(n.nodeOptional.present()) {
			NodeSequence nodeseq=(NodeSequence)n.nodeOptional.node;
			for(Enumeration<Node> e=((NodeList)nodeseq.elementAt(1)).elements();e.hasMoreElements();){
				props=null;literal=null;String replacingVal=null;
				NodeSequence nodeseq2=(NodeSequence)e.nextElement();
				nodeseq2.elementAt(3).accept(this);
				if(props!=null) {
					replacingVal=new ExpressionString(props).toString();
				} else if(literal!=null)
					replacingVal=formatLiteral(literal).toString();
				if(replacingVal==null) continue;
				replacingValues.put(((NodeChoice)nodeseq2.elementAt(0)).which,replacingVal);				
			}
		}
		aLine.push(new StringBuffer());
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			e.nextElement().accept(this);
			if(props==null) continue;
			if(props.isOccurs()&&props.getIndexesWorkSpace()==null) reportError(n,"Warning: A table may not be directly specified with INITIALIZE verb without subscript.",false);
			int i=0;
			if(props.isOccurs()||props.isAParentInOccurs()&&props.getIndexesWorkSpace()==null) {
				for(SymbolProperties occursPar:props.getOccursParents()){
					i++;
					if(aLine.peek().length()>0)	aLine.peek().append(',');
					String idx="idx"+String.valueOf(i+1);
					aLine.peek().append(idx);
					sourceLst.add("for(int "+idx+"=1;"+idx+"<="+String.valueOf(occursPar.getMaxOccurs())+";++"+idx+") {");
					sourceLst.add("tab()");
				}
			}
			doInitializeVerb(props,i);
			if(props.isOccurs()||props.isAParentInOccurs()) {
				for(i=0;i<props.getOccursParents().size();++i){
					sourceLst.add("backTab()");
					sourceLst.add("}");
				}
			}
		}
		aLine.pop();

	}
	private void doInitializeVerb(ArrayList<SymbolProperties> a,int idx) {
		if(a==null||a.size()<=0) return;
		int saveIdx=idx,i=idx;
		for(SymbolProperties ch:a){
			if(ch.isOccurs()) {
				for(;i<ch.getOccursParents().size();++i){
					SymbolProperties occursPar=ch.getOccursParents().get(i);
					if(aLine.peek().length()>0)aLine.peek().append(',');
					String ix="idx"+String.valueOf(i+1);
					aLine.peek().append(ix);
					sourceLst.add("for(int "+ix+"=1;" +ix+"<="+
							String.valueOf(occursPar.getMaxOccurs())+";++"+ix+") {");
					sourceLst.add("tab()");
				
				}
			}
			if(!ch.isElementData())	doInitializeVerb(ch.getChildren(),i);
			else doInitializeVerb(ch,i);
			if(ch.isOccurs()) {
				for(;i>saveIdx;--i){
					if(aLine.peek().length()>0) {
						int temp;
						aLine.peek().delete((((temp=aLine.peek().lastIndexOf(","))<0)?0:temp),aLine.peek().length());
					}
					sourceLst.add("backTab()");
					sourceLst.add("}");
				}
			}
		}
	}
	
	private void doInitializeVerb(SymbolProperties p,int idx) {
		int saveIdx=idx,i=idx;
		if(p.isOccurs()) {
			for(;i<p.getOccursParents().size();++i){
				SymbolProperties occursPar=p.getOccursParents().get(i);
				if(aLine.peek().length()>0)aLine.peek().append(',');
				String ix="idx"+String.valueOf(i+1);
				aLine.peek().append(ix);
				sourceLst.add("for(int "+ix+"=1;" +ix+"<="+
						String.valueOf(occursPar.getMaxOccurs())+";++"+ix+") {");
				sourceLst.add("tab()");
			}
		}
		if(!p.isElementData()) doInitializeVerb(p.getChildren(),i);
		else	{
			p.setIndexesWorkSpace(new ArrayList<String>());
			p.getIndexesWorkSpace().add(aLine.peek().toString());
			switch(p.getJavaType().type) {
			case Constants.CHAR:
				expression.push("\' \'");expressionType=CobolSymbol.CHAR;
				sourceLst.add(TranslationTable.getInstance().getAssignString(p, null, false,true));break;
			case Constants.BYTE:
			case Constants.SHORT:
			case Constants.INTEGER:
			case Constants.LONG:
				expression.push("0");expressionType=CobolSymbol.INTEGER;
				sourceLst.add(TranslationTable.getInstance().getAssignString(p, null, false,true));break;
			case Constants.FLOAT:
			case Constants.DOUBLE:
				expression.push("0.00");expressionType=CobolSymbol.FLOAT;
				sourceLst.add(TranslationTable.getInstance().getAssignString(p, null, false,true));break;
			case Constants.BIGDECIMAL:
				expression.push("BigDecimal.ZERO");expressionType=CobolSymbol.BIGDECIMAL;
				sourceLst.add(TranslationTable.getInstance().getAssignString(p, null, false,true));break;
			case Constants.STRING:
				expression.push("\" \"");expressionType=CobolSymbol.STRING;
				sourceLst.add(TranslationTable.getInstance().getAssignString(p, null, false,true));break;
			case Constants.GROUP:
				if(p.hasChildren())doInitializeVerb(p.getChildren(),idx);break;
			default:
				sourceLst.add(NameUtil.getJavaName(p,true).replace("%0", "null")+';');break;
			}
		}
		if(p.isOccurs()) {
			for(;i>saveIdx;--i){
				if(aLine.peek().length()>0) {
					int temp;
					aLine.peek().delete((((temp=aLine.lastIndexOf(","))<0)?0:temp),aLine.peek().length());
				}
				sourceLst.add("backTab()");
				sourceLst.add("}");
			}
		}
	}

	@Override
	public void visit(EvaluateStatement n) {
		
		if(evaluateValue!=null)evaluateValue.clear();
		n.evaluateValue.accept(this);
		n.nodeListOptional.accept(this);
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {//WHEN...ALSO..Statement()+
			NodeSequence node =  (NodeSequence) e.nextElement();
			evaluateValueIndex=0;
			aLine.push(new StringBuffer());
			aLine.peek().append("if(");
			for(Enumeration<Node> e2=((NodeList)node.elementAt(0)).elements();e2.hasMoreElements();) {//WHEN
				NodeSequence nodeseq=(NodeSequence)e2.nextElement();
				nodeseq.elementAt(2).accept(this);
				if(((NodeListOptional)nodeseq.elementAt(3)).present()) {//ALSO LIST
					for(Enumeration<Node> e3=((NodeListOptional)nodeseq.elementAt(3)).elements();e3.hasMoreElements();) {
						aLine.peek().append("&&");evaluateValueIndex++;
						e3.nextElement().accept(this);
					}
				}
				if(e2.hasMoreElements())
					aLine.peek().append("||");
			}
			aLine.peek().append(") {");
			sourceLst.add(aLine.peek().toString());
			sourceLst.add("tab()");
			node.elementAt(1).accept(this);
			sourceLst.add("backTab()");
			if(e.hasMoreElements())
				sourceLst.add("} else ");
			else
				sourceLst.add("}");
			aLine.pop();
		}
		if(n.nodeOptional.present()) {
			sourceLst.add("else {");
			sourceLst.add("tab()");
			n.nodeOptional.accept(this);
			sourceLst.add("backTab()");
			sourceLst.add("}");
		}
		
	}

	private ArrayList<Object> evaluateValue = null;
	private int evaluateValueIndex = 0;
	
	@Override
	public void visit(EvaluatePhrase n) {
		
		switch(n.nodeChoice.which) {
		case 0:
			aLine.peek().append("true");return;
		case 3:
			aLine.peek().append("");break;
		case 4:
			aLine.peek().append("!");break;
		case 2:
			doReverseCondition=false;
			n.nodeChoice.choice.accept(this);
			aLine.peek().append(condition.pop()).append("==");break;
		case 1:
			StringBuffer subphrase=new StringBuffer();
			NodeSequence nodeSeq = (NodeSequence) n.nodeChoice.choice;
			((NodeOptional)(nodeSeq.elementAt(0))).present();//Not
			NodeChoice nodech1 =((NodeChoice)(nodeSeq.elementAt(1)));
			nodech1.accept(this);
			switch(nodech1.which) {
			case 0:
				expressionType=props.getJavaType().type;
				expression.push(new ExpressionString(props).toString());break;
			case 1:
				expression.push(formatLiteral(literal).toString());break;
			case 2:
			}
			String top="";
			ExpressionString expr1=ExpressionString.getExpression();
			if(((NodeOptional)nodeSeq.elementAt(2)).present()) {
				expression.push(appendEvaluateValue());
				top=TranslationTable.getInstance().doCondition(
						TranslationTable.LE_CONDITION, 
						expr1, ExpressionString.getExpression(),false);;
				subphrase.append(top).append("&&");
				NodeSequence nodeseq2=(NodeSequence)((NodeOptional)nodeSeq.elementAt(2)).node;
				NodeChoice nodech2 =((NodeChoice)(nodeseq2.elementAt(1)));
				nodech2.accept(this);
				switch(nodech2.which) {
				case 0:
					expressionType=props.getJavaType().type;
					expression.push(new ExpressionString(props).toString());break;
				case 1:
					expression.push(formatLiteral(literal).toString());break;
				case 2:
				}
				expr1=ExpressionString.getExpression();
				expression.push(appendEvaluateValue());
				top=TranslationTable.getInstance().doCondition(
						TranslationTable.GE_CONDITION, 
						expr1, ExpressionString.getExpression(),false);;
				subphrase.append(top);
			} else {
				if(appendEvaluateValue().equals("true")) {
					subphrase.append(expr1.toString());
				} else 
				if(appendEvaluateValue().equals("false")) {
					subphrase.append('!').append(expr1.toString());
				} else {
					expression.push(appendEvaluateValue());
					top=TranslationTable.getInstance().doCondition(TranslationTable.EQ_CONDITION,
							expr1,ExpressionString.getExpression() ,false);
					subphrase.append(top);
				}
			}
			if(((NodeOptional)(nodeSeq.elementAt(0))).present())
				aLine.peek().append("!(").append(subphrase).append(')');
			else
				aLine.peek().append(subphrase);
			return;
		}
		aLine.peek().append(appendEvaluateValue());
	}
		
	private String appendEvaluateValue() {
		if(evaluateValueIndex>=0&&evaluateValueIndex<evaluateValue.size()) {
			if(evaluateValue.get(evaluateValueIndex) instanceof ExpressionString ) {
				expressionType=((ExpressionString)evaluateValue.get(evaluateValueIndex)).type;
				return evaluateValue.get(evaluateValueIndex).toString();
			}
			return (String)evaluateValue.get(evaluateValueIndex);
		}
		return "0";
	}

	@Override
	public void visit(EvaluateValue n) {
		if(evaluateValue==null) 
			evaluateValue=new ArrayList<Object>();
		n.nodeChoice.accept(this);
		switch(n.nodeChoice.which) {
		case 2:
			literal=new ExpressionString(expression.pop3());
			literal.type=expressionType;
			evaluateValue.add(literal);
			break;
		case 0:
			evaluateValue.add(new ExpressionString(props).toString());break;
		case 3:
			evaluateValue.add(formatLiteral(literal));break;
		case 1:
			evaluateValue.add(condition.pop());;break;
		case 4:
			evaluateValue.add("true");break;
		case 5:
			evaluateValue.add("false");break;
		}
		//super.visit(n);
	}
	

	public SymbolProperties getLastVisitedIdentifer() {
		return props;
	}
	public void setLastVisitedIdentifer(SymbolProperties props) {
		this.props = props;
	}

	public static Cobol2Java getInstance() {
		if(thiz==null) thiz=new Cobol2Java();
		return thiz;
	}
	//QuyTV
	//print tasklet declare
	public static void printTaskletDeclare(ArrayList<String> headerLst,ArrayList<String> classLst,
											ArrayList<SymbolProperties> fileLst) {
		// For class
		if (fileLst != null) {
			for (SymbolProperties child : fileLst) {
				// Indexed and line sequential files used to be skipped here, yet the assign-name
				// loop below and every READ/WRITE still referred to them, so the Tasklet did not
				// compile. Every file gets its FileRecord.
				{
					String strLen;
					/*
					 * if (child.getChildren() != null) { strLen =
					 * child.getChildren().get(0).getJavaName2() + ".Length";
					 * headerLst.add("import jp.co.alsm.batch." + child.getParent().getJavaName1() +
					 * "." + child.getParent().getJavaName2() + "Model." +
					 * child.getChildren().get(0).getJavaName2()); } else {
					 */
					strLen = String.valueOf(child.getLength()) + ((child.getAdjustedLength() != child.getLength())
							? ("," + String.valueOf(child.getAdjustedLength()))
							: "");
					classLst.add("private FileRecord " + child.getJavaName1() + " = (new FileRecord()).withRecLen("
							+ strLen + ");");
					// }
				}
			}
		}
		classLst.add("");
		classLst.addAll(level88Getters);
		level88Getters.clear();
		classLst.add("@Override");
		classLst.add(
				"public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {");
		classLst.add("tab()");
		classLst.add("try {");
		classLst.add("tab()");
		if (fileLst != null) {
			classLst.add("");
			for (SymbolProperties child : fileLst) {
				// The file field is declared as getJavaName1(), and the job parameter name must not carry
				// the COBOL quotes (they used to end up doubled inside the Java string).
				String assignName = child.getOtherName().replace("\"", "");
				if (assignName.startsWith("S-"))
					assignName = assignName.substring(2);
				classLst.add(child.getJavaName1() + ".withAssignName((String) jobParameters.get(\""
						+ assignName + "\"));");
			}
			classLst.add("");
		}
		classLst.add("//TODO Need to check called fuction");
		if (strMainFunction != null)
			classLst.add(strMainFunction + "();");
		else
			classLst.addAll(sourceLstFirst);
		// classLst.add("init();");
		// classLst.add("while (true) {");
		// classLst.add("tab()");
		// classLst.add("\\\\TODO Need to check called fuction");
		// classLst.add("\\\\if (StringHelper.isNul(XXXX) {");
		// classLst.add("tab()");
		// classLst.add("\\\\break;");
		// classLst.add("backTab()");
		// classLst.add("}");
		// classLst.add("main();");
		// classLst.add("backTab()");
		// classLst.add("finl();");
		classLst.add("backTab()");
		classLst.add("} catch (Exception e) {");
		classLst.add("tab()");
		classLst.add("this.logger.error(e.getMessage());");
		classLst.add("return RepeatStatus.FINISHED;");
		classLst.add("backTab()");
		classLst.add("}");
		classLst.add("");
		//classLst.add("backTab()");
		classLst.add("return RepeatStatus.FINISHED;");
		classLst.add("backTab()");
		classLst.add("}");

		if (taskletList != null) {
			Enumeration e = taskletList.keys();
			sourceLst.add("tab()");
			while (e.hasMoreElements()) {
				String key = (String) e.nextElement();
				// A called program is written into the same program package as the caller.
				String calledBase = RESConfig.getInstance().getProgramPackage() == null ? "jp.co.alsm.batch"
						: RESConfig.getInstance().getProgramPackage().replace('\\', '.').replace("src.main.java.", "");
				headerLst.add("import " + calledBase + "." + key.toLowerCase() + "." +
				// NameUtil.convertCobolNameToJava(key.toUpperCase(),true) +"Tasklet;");
						key + "Tasklet;");
				// System.out.println(key + " : " + taskletList.get(key));
				sourceLst.add("private void call" + key + "() throws Exception {");
				sourceLst.add("tab()");
				sourceLst.add(key + "Tasklet " + key.toLowerCase() + "Tasklet = new " + key + "Tasklet();");
				if (taskletList.get(key).length() > 0) {
					String[] itemLst = taskletList.get(key).split(";");
					for (String s : itemLst) {
						sourceLst.add("model." + s + ".write(" + key.toLowerCase() + "Tasklet.execute(model." + s
								+ ").toString());");
					}
				}
				sourceLst.add("backTab()");
				sourceLst.add("}");
			}
			sourceLst.add("backTab()");
		}
		for (String item : headerLst) {
			printLn(item);
		}
		for (String item : classLst) {
			printLn(item);
		}
		for (String item : sourceLst) {
			printLn(item);
		}
		headerLst.removeAll(headerLst);
		classLst.removeAll(classLst);
		sourceLst.removeAll(sourceLst);
		/* Add 20210827 S */
		 taskletList.clear();
		 sourceLstFirst.clear();
		 /*Add 20210827 E*/
		strMainFunction = null;
		/*
		 * if (fileLst != null) { for (SymbolProperties item : fileLst) {
		 * printLn(item.getDataName() +" "+ item.getOtherName()); } }
		 */
		/*
		 * sourceLst.add(""); String name =""; for (String item : taskletList) { name =
		 * item.substring(0, 1).toUpperCase() + item.substring(1);
		 * sourceLst.add("    @Autowired"); sourceLst.add("    " +name +"  " + item
		 * +";"); sourceLst.add(""); }
		 */
	}
	private static void printLn(String strLn) {
		if (strLn.equals("tab()")) ClassFile.tab();
		else if (strLn.equals("backTab()")) ClassFile.backTab();
		else ClassFile.println(strLn);
	}
	
	@Override
	public void visit(UnstringStatement n) {
		if(n.nodeOptional3.present()||n.nodeOptional4.present())
			doTry();
		n.identifier.accept(this);
		if(props==null) return;
		aLine.push(new StringBuffer());bLine.push(new StringBuffer());
		aLine.peek().append("new CobolString(").append(new ExpressionString(props).toString()).append(')');
		if(n.nodeOptional.present()) {
			props=null;literal=null;
			NodeSequence nodeseq=(NodeSequence)n.nodeOptional.node;
			nodeseq.elementAt(3).accept(this);
			bLine.peek().append('.').append("delimitedBy(");
			if(props!=null)bLine.peek().append(new ExpressionString(props).toString());
			else bLine.peek().append(formatLiteral(literal).toString());
			if(((NodeOptional)nodeseq.elementAt(2)).present())
				bLine.peek().append(",true");
			bLine.peek().append(')');
			NodeListOptional orList=(NodeListOptional)nodeseq.elementAt(4);
			if(orList.present()) {
				for(Enumeration<Node> e=orList.elements();e.hasMoreElements();) {
					NodeSequence nodeseq2 = (NodeSequence) e.nextElement();
					nodeseq2.elementAt(2).accept(this);
					bLine.peek().append('.').append("delimitedBy(");
					if(props!=null)bLine.peek().append(new ExpressionString(props).toString());
					else bLine.peek().append(formatLiteral(literal).toString());
					if(((NodeOptional)nodeseq2.elementAt(1)).present())
						bLine.peek().append(",true");
					bLine.peek().append(')');
				}
			}
		}
		aLine.peek().append(bLine.peek()).append(".unString();");
		String tempVar="temp"+String.valueOf(SymbolTable.tempVariablesMark++);
		aLine.peek().insert(0,"CobolString "+tempVar+" = ");
		sourceLst.add(aLine.peek().toString());int i=0;
		for(Enumeration<Node> e2=n.nodeList.elements();e2.hasMoreElements();) {
			NodeSequence nodeseq3=(NodeSequence)e2.nextElement();
			nodeseq3.elementAt(0).accept(this);
			bLine.push(new StringBuffer());
			expressionType=Constants.STRING;
			expression.push(bLine.peek().append(tempVar).append(".getUnString()").toString());
			sourceLst.add(TranslationTable.getInstance().getAssignString(props,null, false));
			NodeOptional opt = (NodeOptional)nodeseq3.elementAt(1);
			if(opt.present()) {
				bLine.push(new StringBuffer());
				opt.accept(this);
				expression.push(bLine.peek().append(tempVar).append(".getUnStringDelimiter()").toString());
				sourceLst.add(TranslationTable.getInstance().getAssignString(props,null, false));
			}
			opt = (NodeOptional)nodeseq3.elementAt(2);
			if(opt.present()) {
				expressionType=Constants.INTEGER;
				bLine.push(new StringBuffer());
				opt.accept(this);
				expression.push(bLine.peek().append(tempVar).append(".getUnStringCount()").toString());
				sourceLst.add(TranslationTable.getInstance().getAssignString(props,null, false));
			}
			i++;
		}
		if(n.nodeOptional1.present()) {
			bLine.push(new StringBuffer());
			bLine.peek().append(tempVar).append(".getUnStringPointer()");
			n.nodeOptional1.accept(this);
			expression.push(bLine.peek().toString());
			expressionType=Constants.INTEGER;
			sourceLst.add(TranslationTable.getInstance().getAssignString(props,null, false));
		}
		bLine.push(new StringBuffer());
		bLine.peek().append(tempVar).append(".getUnStringTally()");
		if(n.nodeOptional2.present()) {
			n.nodeOptional2.accept(this);
			expression.push(bLine.peek().toString());
			expressionType=Constants.INTEGER;
			sourceLst.add(TranslationTable.getInstance().getAssignString(props,null, false));
		}

		aLine.pop();bLine.pop();
		
		if(n.nodeOptional3.present()||n.nodeOptional4.present()) {
			sourceLst.add("__assertException();");
			if(n.nodeOptional4.present()) {//Not On OverFlow
				n.nodeOptional4.accept(this);
			} 
			sourceLst.add("backTab()");
			sourceLst.add("} catch(OverflowException ofe) {");
			sourceLst.add("tab()");
			exceptionPrintStackTrace("ofe");
			if(n.nodeOptional3.present()) {//On OverFlow
				n.nodeOptional3.accept(this);
			}
			sourceLst.add("backTab()");
			sourceLst.add("}");
		}
	}

	@Override
	public void visit(SearchStatement n) {
		n.qualifiedDataName.accept(this);
		if(!props.isOccurs()) reportError(n,"Search Statement is valid only on tables with Occurs clause.");
		if(props.getMajorIndex()==null)reportError(n,"Search Statement is valid only on tables with INDEXED BY.");
		SymbolProperties table = props;
		props=null;
		n.nodeOptional1.accept(this);
		SymbolProperties varying=props;
		sourceLst.add("do {");
		sourceLst.add("tab()");
		sourceLst.add("if("+NameUtil.getJavaName(table.getMajorIndex(), false)+">"+
				table.getMaxOccurs()+") {" );
		sourceLst.add("tab()");
		n.nodeOptional2.accept(this);
		if(!isLastGotoStatement)
			sourceLst.add(" break;");
		sourceLst.add("backTab()");
		sourceLst.add("}");
		for(Enumeration<Node> e=n.nodeList.elements();e.hasMoreElements();) {
			NodeSequence nodeseq =  (NodeSequence)e.nextElement();
			nodeseq.elementAt(1).accept(this);
			sourceLst.add("if("+condition.pop()+") {" );
			sourceLst.add("tab()");
			NodeChoice nodech = (NodeChoice) nodeseq.elementAt(2);
			if(nodech.which==0) {
				nodech.accept(this);
				if(!isLastGotoStatement)
					sourceLst.add(" break;");
			} else {
				sourceLst.add(" break;");
			}
			sourceLst.add("backTab()");
			sourceLst.add("}");
		}
		sourceLst.add(getIncrement(table.getMajorIndex())+";");
		if(varying!=null&&!varying.equals(table.getMajorIndex())) {
			sourceLst.add(getIncrement(varying)+";");
		}
		sourceLst.add("backTab()");
		sourceLst.add("} while(true);");
	}

	public String getIncrement(SymbolProperties props2) {
		return NameUtil.getJavaName(props2, true).replace("%0",NameUtil.getJavaName(props2,false)+"+1");
	}
}
