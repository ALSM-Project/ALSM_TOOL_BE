package com.res.java.translation.engine;
/*****************************************************************************
Copyright 2009 Venkat Krishnamurthy
This file is part of RES.

RES is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RES is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RES.  If not, see <http://www.gnu.org/licenses/>.

@author VenkatK mailto: open.cobol.to.java at gmail.com
******************************************************************************/

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.regex.Pattern;

import com.res.cobol.RESNode;
import com.res.cobol.TreeToCommentFormatter;
import com.res.cobol.syntaxtree.Arguments;
import com.res.cobol.syntaxtree.ColumnValues;
import com.res.cobol.syntaxtree.CommitStatement;
import com.res.cobol.syntaxtree.DeclareCursorStatement;
import com.res.cobol.syntaxtree.ExecSqlStatement;
import com.res.cobol.syntaxtree.FetchStatement;
import com.res.cobol.syntaxtree.InsertStatement;
import com.res.cobol.syntaxtree.IntoClause;
import com.res.cobol.syntaxtree.LockTableStatement;
import com.res.cobol.syntaxtree.Node;
import com.res.cobol.syntaxtree.NodeChoice;
import com.res.cobol.syntaxtree.NodeSequence;
import com.res.cobol.syntaxtree.NodeToken;
import com.res.cobol.syntaxtree.QueryStatement;
import com.res.cobol.syntaxtree.RollbackStatement;
import com.res.cobol.syntaxtree.SQLCloseStatement;
import com.res.cobol.syntaxtree.SQLDeleteStatement;
import com.res.cobol.syntaxtree.SQLOpenStatement;
import com.res.cobol.syntaxtree.SQLStatement;
import com.res.cobol.syntaxtree.SavepointStatement;
import com.res.cobol.syntaxtree.SelectStatement;
import com.res.cobol.syntaxtree.SelectWithoutOrder;
import com.res.cobol.syntaxtree.SetTransactionStatement;
import com.res.cobol.syntaxtree.SetVariableStatement;
import com.res.cobol.syntaxtree.Statement;
import com.res.cobol.syntaxtree.UpdateStatement;
import com.res.cobol.visitor.DepthFirstVisitor;
import com.res.common.StringTree;
import com.res.java.lib.CobolSymbol;
import com.res.java.lib.Constants;
import com.res.java.lib.RunTimeUtil;
import com.res.java.translation.symbol.SymbolConstants;
import com.res.java.translation.symbol.SymbolProperties;
import com.res.java.translation.symbol.SymbolTable;
import com.res.java.util.ClassFile;
import com.res.java.util.NameUtil;

public class ExecSql2Java extends DepthFirstVisitor {

	private SymbolProperties props=null;
	private ByteArrayOutputStream baos=new ByteArrayOutputStream();
	private StringTree dumper=new StringTree(baos);
	private Cobol2Java cobol2Java=null;
	
	private  ArrayList<String> sourceLst = new ArrayList<>();
	
	public void setSourceLst( ArrayList<String> srcLst) {
		this.sourceLst=srcLst;
	}
	public ArrayList<String> getSourceLst(){
		return this.sourceLst;
	}
	
	
	public void visit(ExecSqlStatement  n) {
		sql=sqlInto=null;
		if(n.nodeChoice1.which==0) { //WHENEVER
			switch(((NodeChoice)((NodeSequence)n.nodeChoice1.choice).elementAt(1)).which) {
			case 0: sql="SQLNOTFOUND";break;
			case 1: sql="SQLERROR";break;
			case 2: sql="SQLWARNING";break;
			}
			sourceLst.add("super.registerHandler(" +sql +",new Paragraph(this) {");
			sourceLst.add("tab()");
			sourceLst.add("public CobolMethod run() {");
			((NodeSequence)n.nodeChoice1.choice).elementAt(2).accept(cobol2Java);
			if(!cobol2Java.isLastGotoStatement)
				sourceLst.add("return null;");
			sourceLst.add("backTab()");
			sourceLst.add("}});");
		} else  {
			NodeChoice nodechoice=(NodeChoice)(((NodeSequence)n.nodeChoice1.choice).elementAt(0));
			switch(nodechoice.which) {
			case 0://Simple Query
				nodechoice.accept(this);
				break;
			case 1://Declare Cursor
				nodechoice.accept(this);
				break;
			case 2://Prepare from
				String cursorName=((NodeToken)((NodeSequence)nodechoice.choice).elementAt(1)).tokenImage;
				String statementVar=((NodeToken)((NodeSequence)nodechoice.choice).elementAt(3)).tokenImage;
				if(statementVar.charAt(0)==':') 
					printDynamicPrepare(cursorName, statementVar);
				break;
		
			case 3://Alter...
				printStoredProcedure(sql=getSQLString(nodechoice.choice));
				break;
			case 4://EXECUTE STORED PROECDURE
				printStoredProcedure(sql=getSQLString(((NodeSequence)nodechoice.choice).elementAt(1)));
				break;
			case 5://CONNECT :parm
				printConnect(sql=getSQLString(((NodeSequence)nodechoice.choice).elementAt(1)));
				break;
			case 6://EXEC SQL Foo1 Foo2 END-EXEC.
				printStoredProcedure(sql=getSQLString(nodechoice.choice));
				break;
			}
		}
	}
	public void visit(DeclareCursorStatement n)
	{
		String cursorName=n.nodeToken1.tokenImage;
		switch(n.nodeChoice.which) {
		case 0:
			String statementName=((NodeToken)n.nodeChoice.choice).tokenImage;
			cursorName=formatCursorName(cursorName);
			printDeclareDynamicCursor(cursorName,
					formatCursorName(statementName));
			break;
		case 1:
			sql=getSQLString(n.nodeChoice.choice);
			printQuery(formatCursorName(cursorName));
			break;
		}
	}
	
	@Override
	public void visit(SQLStatement  n) {
		n.nodeChoice.choice.accept(this);
	}

	@Override
	public void visit(CommitStatement n) {
		printProgramExceptionHdr();
		sourceLst.add("__dao().commit"+"();");
		printProgramExceptionTail();
	}


	@Override
	public void visit(RollbackStatement n) {
		printProgramExceptionHdr();
		if(n.nodeOptional1.present()) {
			sourceLst.add("__dao().rollback"+"(" +
					formatCursorName(getSQLString(((NodeSequence)n.nodeOptional1.node).elementAt(2)))+
					");");
		} else {
			sourceLst.add("__dao().rollback"+"();");
		}
		printProgramExceptionTail();
	}


	@Override
	public void visit(SavepointStatement n) {
		printProgramExceptionHdr();
		sourceLst.add("__dao().savePoint"+"(" +
					formatCursorName(getSQLString(n.relObjectName))+
					");");
		printProgramExceptionTail();
	}


	@Override
	public void visit(SQLDeleteStatement n) {
		sql=getSQLString(n);
		printUpdate();
	}
	@Override
	public void visit(SetTransactionStatement n) {
		sql=getSQLString(n);
		printUpdate();
	}


	@Override
	public void visit(InsertStatement n) {
		sql=getSQLString(n);
		setParameters(sqlInto);
		printUpdate();
	}


	@Override
	public void visit(LockTableStatement n) {
		sql=getSQLString(n);
		setParameters(sqlInto);
	}


	@Override
	public void visit(SetVariableStatement n) {
		sql=getSQLString(n);
		setParameters(sqlInto);
	}

	@Override
	public void visit(FetchStatement n) {
		printProgramExceptionHdr();
		String subscript="";
		sourceLst.add("__dao().fetchCursor(" +
				formatCursorName(getSQLString(n.nodeSequence))+");" );
		if(n.nodeOptional.present()) {
			NodeChoice nodech = (NodeChoice)((NodeSequence)n.nodeOptional.node).elementAt(1);
			sql=getSQLString(nodech.choice);
			switch(nodech.which) {
			case 0:
				sql=Cobol2Java.getInstance().formatLiteral(sql).toString(); break;
			case 1:
				if((props=TranUtil.get().getParameter(sql))!=null)
					sql=NameUtil.getJavaName(props, false);
				else {
					RunTimeUtil.getInstance().reportError("Unknown SQL Binding Variable : "+ sql,false);
					//sql=null;
				}
			}
			if(sql!=null) {
				subscript="i"+String.valueOf(++SymbolTable.tempVariablesMark);
				sourceLst.add("for(int "+subscript+"=1;"+subscript+"<="+sql+"&&__dao().resultExists();++"+subscript+") {");
				sourceLst.add("tab()");
			}
		}
		else {
		sourceLst.add("if(__dao().resultExists()) {");
		sourceLst.add("tab()");
		}
		printSetResults(" "+getSQLString(n.nodeChoice,n.nodeListOptional)+" ",subscript);
		sourceLst.add("backTab()");
		sourceLst.add("}");
		printProgramExceptionTail();
	}

	
	private void printSetResults(String sqlString) {
		printSetResults(sqlString,"");
	}
	private void printSetResults(String sqlString,String sub) {
		int i=1;int j;
		for(String s:TranUtil.get().getParameters(sqlString,true)) {
			if(s==null||s.trim().length()<=0)
				continue;
			String s2;
			if(s.indexOf('.')>0){
				props=SymbolTable.getScope().lookup(s.substring((j=s.indexOf('.'))+1),
						s.substring(1,j));
				s2=s.substring(j+1);
			}
			else {
				props=TranUtil.get().getParameter(s);
				if(s.charAt(0)==':')
					s2=s.substring(1);
				s2=s.trim();
			}
			if(props!=null){
				if(sub.length()>0) {
					if(props.getIndexesWorkSpace()==null)
						props.setIndexesWorkSpace(new ArrayList<String>());
					props.getIndexesWorkSpace().add(sub);
				}
				Cobol2Java.getInstance().expression.push(
						"__dao().get"+
						SymbolConstants.getSQL(props.getJavaType().type)+
						"(" +String.valueOf(i)+")");
				Cobol2Java.getInstance().expressionType=props.getJavaType().type;
				String assign=TranslationTable.getInstance().getAssignString(props, null,false);
				SymbolProperties indprops=TranUtil.get().getIndicator(s);
				if(indprops==null) {
					sourceLst.add(assign);
				} else {
					if(indprops.isOccurs()||props.isAParentInOccurs()) {
						indprops.setIndexesWorkSpace(new ArrayList<String>());
						indprops.getIndexesWorkSpace().add(sub);
					}
					sourceLst.add("if(__dao().get"+SymbolConstants.getSQL(props.getJavaType().type)+
							"(" +String.valueOf(i)+")"+"==null)"+
							" "+NameUtil.getJavaName(indprops, true).replace("%0","-1")+";");
					sourceLst.add("else {"+NameUtil.getJavaName(indprops, true).replace("%0","0")+";");
					sourceLst.add("\t"+assign+"}");
				}
			} else {
				RunTimeUtil.getInstance().reportError("Unknown Host Symbol in SQL: "+s2,false);
				return;
			}
			i++;
		}
	}

	
	@Override
	public void visit(Arguments n) {
		sql=getSQLString(n);
	}


	@Override
	public void visit(SQLOpenStatement n) {
		printProgramExceptionHdr();
		sourceLst.add("__dao().executeStatement(" +
				formatCursorName(getSQLString(n.relObjectName)) +
				");");
		sql=null;
		n.nodeOptional.accept(this);
		if(sql!=null) {
			setParameters(sql);
		}
		printProgramExceptionTail();
	}

	@Override
	public void visit(SQLCloseStatement n) {
		printProgramExceptionHdr();
		sourceLst.add("__dao().close(" +
				formatCursorName(getSQLString(n.relObjectName))+");" );
		printProgramExceptionTail();
	}

		
	@Override
	public void visit(SelectStatement n) {
		n.selectWithoutOrder.accept(this);
		sql+=' '+getSQLString(n.nodeOptional);
		sql+=' '+getSQLString(n.nodeOptional1);
	}


	@Override
	public void visit(QueryStatement  n) {
		n.selectStatement.accept(this);
		printQuery(null);
	}
	
	@Override
	public void visit(UpdateStatement n) {
		sql=getSQLString(n);
		printUpdate();
	}
	
	@Override
	public void visit(ColumnValues n) {
		super.visit(n);
	}
	
	private String sql=null;
	@Override
	public void visit(IntoClause n) {
		sqlInto=getSQLString(n.intoItem,n.nodeListOptional);
	}

	private String sqlInto=null;
	@Override
	public void visit(SelectWithoutOrder  n) {
		sql=getSQLString(n.nodeToken,n.nodeOptional,
			n.selectList,n.fromClause,n.nodeOptional2,n.nodeOptional3,n.nodeOptional4,n.nodeOptional5);
		sqlInto=null;
		n.nodeOptional1.accept(this);
	}

	public void reportError(Node n,String msg) {
		System.out.println("@CobolSourceFile(\""+((RESNode)n).sourceFile+"\","+"):"+msg);
		System.exit(1);
	}

	public ExecSql2Java(Cobol2Java from) {
		cobol2Java=from;
	}

	private void printQuery(String cursorName) {
		
		printProgramExceptionHdr();
		sourceLst.add("__dao().prepareStatement("+formatSQLString(sql)+") ;");
			
		setParameters(sql); 
	
		if(cursorName!=null) {
			sourceLst.add("__dao().saveStatement("+formatCursorName(cursorName)+");");
		}
		
		sourceLst.add("__dao().executeQuery();");
		
		if(sqlInto!=null) {
			sourceLst.add("if(__dao().resultExists()) {");
			sourceLst.add("tab()");
			printSetResults(sqlInto);
			sourceLst.add("backTab()");
			sourceLst.add("}");
		}
		printProgramExceptionTail();
	}

	private void printUpdate() {
		
		printProgramExceptionHdr();
		sourceLst.add("__dao().prepareStatement("+formatSQLString(sql)+") ;");
			
		setParameters(sql); 
	
		sourceLst.add("__dao().executeUpdate();");

		printProgramExceptionTail();
	}

	private int setParameters(String sqlString) {
		int i=1;
		for(String parm:TranUtil.get().getParameters(sqlString,false)){					
			if(parm==null||parm.trim().length()<=2) continue;
			props=TranUtil.get().getParameter(parm);
			SymbolProperties indProps=null;
			if(props!=null) {
				String line = ("__dao().set"+SymbolConstants.getSQL(props.getJavaType().type)+"("
						+String.valueOf(i++)+","+
								NameUtil.getJavaName(props, false)+");");
				if((indProps=TranUtil.get().getIndicator(parm))!=null) {
					sourceLst.add("if("+NameUtil.getJavaName(indProps, false)+"<0)"+
							"__dao().set"+SymbolConstants.getSQL(props.getJavaType().type)+"("
							+String.valueOf(i-1)+","+
									null+"); else "+line);
				}
				else  sourceLst.add(line);
			}
		}
		return i;
	}
	Pattern STORED_PROCEDURE_PATTERN = Pattern.compile("(BEGIN.*)|(CREATE([ \r\n\t]+OR[ \r\n\t]+REPLACE)?[ \r\n\t]+PROCEDURE.*)|(CALL.*)");
	private void printStoredProcedure(String storedProcedure) {
		printProgramExceptionHdr();
		if(STORED_PROCEDURE_PATTERN.matcher(storedProcedure.replace('\r', ' ').replace('\n',' ').replace('\t', ' ').replace('\f',  ' ' )
				.toUpperCase())
				.matches()) {
			sourceLst.add("if(__dao().prepareCall("+formatSQLString(storedProcedure)+")) {");
			sourceLst.add("tab()");
			SymbolProperties props=null,indProps=null;int i=1;
			for(String parm:TranUtil.get().getParameters(storedProcedure,false)) {
				if((props=TranUtil.get().getParameter(parm))==null) continue;
				sourceLst.add("__dao().registerOutParameter(" +
						String.valueOf(i++) +
						"," +SymbolConstants.getSQL2(props.getJavaType().type) +
						");");
			}
			setParameters(storedProcedure);
			sourceLst.add("__dao().executeStatement();");i=1;
			for(String parm:TranUtil.get().getParameters(storedProcedure,false)) {
				if((props=TranUtil.get().getParameter(parm))==null) continue;
				if((indProps=TranUtil.get().getIndicator(parm))==null)
					sourceLst.add(NameUtil.getJavaName(props, true).replace("%0", ("__dao().getCallable" +
						SymbolConstants.getSQL(props.getJavaType().type) +"(" +String.valueOf(i++)) +")")+";");
				else {
				sourceLst.add("if(__dao().getCallable"+SymbolConstants.getSQL(props.getJavaType().type)+"("+String.valueOf(i-1)+")== null)"
						+NameUtil.getJavaName(indProps,true).replace("%0", "-1")+";"+" else {"+NameUtil.getJavaName(indProps,true).replace("%0", "0")+"; "+NameUtil.getJavaName(props, true).replace("%0", ("__dao().getCallable" +
						SymbolConstants.getSQL(props.getJavaType().type) +"(" +String.valueOf(i-1)) +")")+";}");
				}
			}
			sourceLst.add("backTab()");sourceLst.add("}");	
		} else {
			sourceLst.add("if(__dao().prepareStatement("+formatSQLString(storedProcedure)+")) {");
			sourceLst.add("tab()");
			setParameters(storedProcedure);
			sourceLst.add("__dao().executeStatement();");
			sourceLst.add("backTab()");
			sourceLst.add("}");	
		} 
		printProgramExceptionTail();
	}
	
	private void printDynamicPrepare(String cursorName, String statementVar) {
		ArrayList<String> parms=TranUtil.get().getParameters(formatCursorName(statementVar),false);
		if(parms!=null&&parms.size()>0) {
			SymbolProperties props=SymbolTable.getScope().lookup(parms.get(0).substring(1));
			if(props==null)	return;
			cursorName=formatCursorName(cursorName);
			printProgramExceptionHdr();
			sourceLst.add("__dao().prepareStatement("+formatCursorName(cursorName)+","+
			TranslationTable.getInstance().convertType(NameUtil.getJavaName(props, false), CobolSymbol.STRING, props.getJavaType().type)+");");
			sourceLst.add("//TODO Add here if needed: __dao().setParameter(parameterIndex,parameter) OR __dao().setParameter(parameterIndex,parameter,scale)");
			sourceLst.add("__dao().executeStatement();");
			printProgramExceptionTail();
		}
	}

	public void printDeclareDynamicCursor(String cursorName,String statementName) {
		cursorName=formatCursorName(cursorName);statementName=formatCursorName(statementName);
		printProgramExceptionHdr();
		sourceLst.add("__dao().declareDynamicCursor(" +cursorName+","+statementName+");");
		printProgramExceptionTail();
		return;
	}
	
	
	public void setStatementNode(Statement statementNode) {
		this.statementNode = statementNode;
	}
	public Statement getStatementNode() {
		return statementNode;
	}
	private Statement statementNode = null;
	
	public TreeToCommentFormatter formatter=null ;

	private String formatCursorName(String name) {
		if(name==null) return "";
		return Cobol2Java.getInstance().formatLiteral(
				"\""+RunTimeUtil.getInstance().stripQuotes(name.toUpperCase())+"\"").toString()
			.trim().replaceAll("([\r\n]+)"," ").
				replaceAll("([ ]+)", " ");
	}
	
	private void printProgramExceptionHdr() {
		sourceLst.add("try {");
		sourceLst.add("tab()");
	}
	
	private void printProgramExceptionTail() {
		sourceLst.add("backTab()");
		sourceLst.add("} catch(SQLException se) { se.printStackTrace(); ");
		sourceLst.add("} catch(Exception e) { e.printStackTrace(); System.exit(1); }");
	}
	
	private String getSQLString(Node... n) {
		baos.reset();
		dumper.startAtNextToken();
		for(Node nd:n) {
			nd.accept(dumper);
			baos.write((byte)' ');
		}
		return new String(baos.toByteArray()).
			trim().replaceAll("[ \t]+"," ").
			replaceAll("[\r\n]+[ ]*","\r\n").
			replaceAll("(\r\n)+","\r\n"+ClassFile.current.getTabs());
	}
	
	private String formatSQLString(String name) {
		if(name==null||name.trim().length()==0) return "";
		return formatLiteral((
				ClassFile.current.getTabs()+"\""+
				name.trim()
			.replaceAll("(\r\n[ \t]*)", "\r\n"+ClassFile.current.getTabs()+"//")
				+"\"").trim()).toString().
			replace("//","\"").replaceAll("(\r\n)"," \"+\r\n").
			replaceAll("(:[a-zA-Z0-9\\-\\.]+([ \r\n]*:[a-zA-Z0-9\\-\\.]+)?)","?");
	}
	
	private String formatLiteral(String lit) {
		int j;
		if((lit.indexOf('\"')==0&&(j=lit.lastIndexOf('\"'))>=0)) {
		return '\"'+lit.substring(1,j).replace("\"\"", "\"").
		replace("\\", "\\\\").
		replace("\"", "\\\'")+'\"';
			
		}
		return lit;
		
	}
	
	private void printConnect(String connectionParam) {
		printProgramExceptionHdr();
		ArrayList<String> a=TranUtil.get().getParameters(formatCursorName(connectionParam),false);
		if(a!=null&&a.size()>0&&a.get(0).charAt(0)==':') {
			props=SymbolTable.getScope().lookup(a.get(0).substring(1),SymbolConstants.DATA);
			if(props.getJavaType().type!=Constants.STRING)
				props=null;
		} else {
			props=null;
		}
		sourceLst.add("__dao().openConnection(null,"+((props!=null)?NameUtil.getJavaName(props, false):"null")+");");
		printProgramExceptionTail();
	}

}
