package com.res.java.translation.symbol;

import com.res.java.translation.engine.Cobol2Java;
import com.res.java.util.NameUtil;

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

public class ExpressionString  {
	public StringBuffer literal;
	public SymbolProperties props=null;
	public int type=-1;
	public boolean isAll=false;
	public boolean isJustRight=false;
	public int length = -1;
	
	public String toString() {
		return literal.toString();
	}
	public ExpressionString(SymbolProperties sym) {
		props=sym;
		if(sym==null) literal=new StringBuffer("");
		else {
			literal=new StringBuffer(NameUtil.getJavaName(props, false));
			length=props.getLength();type=props.getJavaType().type;
		}
	}
	public ExpressionString(SymbolProperties sym,boolean isGet) {
		props=sym;
		if(sym==null) literal=new StringBuffer("");
		else {
			literal=new StringBuffer(NameUtil.getJavaName(props, false));
			length=props.getLength();type=props.getJavaType().type;
		}
	}
	public ExpressionString(String lit) {
		if(lit==null) lit="";
		literal=new StringBuffer(lit);
	}
	public ExpressionString(String lit, int type) {
		this(lit);this.type=type;
	}
	public ExpressionString(ExpressionString lit) {
		literal=lit.literal;
		isAll=lit.isAll;type=lit.type;props=lit.props;
	}
	public ExpressionString() {
		literal=new StringBuffer();
	}
	public ExpressionString(int lit) {
		this(String.valueOf(lit));
	}
	public ExpressionString set(String lit) {
		literal=new StringBuffer(lit); return this;
	}
	public ExpressionString set(int lit) {
		literal=new StringBuffer(String.valueOf(lit)); return this;
	}
	public ExpressionString setAll(boolean b) {
		isAll=b; return this;
	}
	public ExpressionString setType(int  t) {
		type=t; return this;
	}
	public ExpressionString setLength(int  l) {
		length=l; return this;
	}
	public static ExpressionString getExpression() {
		ExpressionString thiz=new ExpressionString(Cobol2Java.getInstance().expression.pop());
		thiz.type=Cobol2Java.getInstance().expressionType;
		return thiz;
	}
}