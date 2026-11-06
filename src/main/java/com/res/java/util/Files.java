package com.res.java.util;
import com.res.common.CobolTranslationException;
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

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Calendar;

import com.res.common.RESConfig;
import com.res.java.lib.RunTimeUtil;
import com.res.java.translation.symbol.SymbolProperties;

public class Files {
	
	public static PrintStream openClassFile(SymbolProperties props,boolean isData) {
		//Create file and folder for output
			String path=NameUtil.getPackageName(props,!isData).replace('.',File.separatorChar);
// DaoNH MOD Start: Check and make directory for each Cobol program
		String outputDir = "";
		String filePath = "";
//		path+=File.separatorChar+NameUtil.getFileName(props,isData);
		if(RESConfig.getInstance().getOutputDir().isEmpty()) {
			outputDir = path + File.separatorChar + NameUtil.getJavaName1(props) + File.separatorChar;
			filePath = outputDir + NameUtil.getFileName(props,isData);
			RESConfig.getInstance().setOutputDir(outputDir);
		} else {
			outputDir = RESConfig.getInstance().getOutputDir();
			filePath = outputDir + NameUtil.getFileName(props,isData);
		}
//		return openClassFile(path);
		return openClassFile(outputDir, filePath);
// DaoNH MOD Start: Check and make directory for each Cobol program
	}
	
	public static PrintStream openBeanInfoFile(SymbolProperties props,boolean isData) {
		String path=NameUtil.getPackageName(props,!isData).replace('.',File.separatorChar);
		path+=File.separatorChar+NameUtil.getBeanInfoFileName(props,isData);	
		return openClassFile(path);
}
	
	public static boolean exists(SymbolProperties props,boolean isData) {
		try {
			
			String path=(NameUtil.getPackageName(props,!isData) + File.separatorChar + NameUtil.getJavaName2(props)).replace('.',File.separatorChar);
			
			path+=File.separatorChar+NameUtil.getFileName(props,isData);		
			int i = path.indexOf(File.separatorChar);
			String dirStr = "";
			File dir = null;
			do {
				dirStr+=path.substring(0, i)+File.separatorChar;
				path=path.substring(i+1);			
				dir = new File(dirStr);
				if (dir.exists())
					if (dir.isDirectory())
					;
					else {
						return true;
					}
				else 
					return false;
				i = path.indexOf(File.separatorChar);
			} while (i>0);
			if(path!=null&&path.length()>0&&path.indexOf(".java")>0) {
				if (dir !=null)
					return new File(dir,path).exists();
				else
					return new File(path).exists();
			}
			return false;
		} catch(Exception e){
			return true;
		}
	
	}

	public static PrintStream openClassFile(String path) {
		try {
			int i = path.indexOf(File.separatorChar);
			String dirStr =
				RunTimeUtil.getInstance().stripQuotes(
						RESConfig.getInstance().getOutputDir().trim()
				);
			if(dirStr==null) dirStr="";
			else { 
				dirStr.replace('/',File.separatorChar);
				if(dirStr.length()>0&&dirStr.lastIndexOf(File.separatorChar)!=dirStr.length()-1) {
					dirStr+=File.separatorChar;
				}
			}
			File dir = null;
			do {
				dirStr+=path.substring(0, i)+File.separatorChar;
				path=path.substring(i+1);			
				dir = new File(dirStr);
				if (dir.exists())
					if (dir.isDirectory())
					;
					else {
						throw new CobolTranslationException("Cannot create output directory " + dir + ": a file of that name exists");
					}
				else 
					if(dir.mkdirs())
						;
					else {
						throw new CobolTranslationException("Cannot create output directory " + dir);
					}
				i = path.indexOf(File.separatorChar);
			} while (i>0);
			if(path!=null&&path.length()>0&&path.indexOf(".java")>0) {
				if(!RESConfig.getInstance().isOverwriteJavaFiles()) 
					if(new File(dir,path).exists()) {
							return null;
						}
					else
						if(new File(path).exists()) {
							return null;
						}
				File file;	
				if (dir !=null) {
					file=new File(dir,path);						
				}
				else {
					file=new File(path);
				}
				if(file.exists())
					file.delete();
				return new PrintStream(new FileOutputStream(file));
			}
		} catch(Exception e){
			e.printStackTrace();
			throw new CobolTranslationException("Could not open file " + path, null, -1, e);
		}
		return null;
	}

// DaoNH ADD Start: Check and create directory for each Cobol file
	/**
	 * Check and create directory for each Cobol file
	 * 
	 * @param outputDir output directory
	 * @param filePath file path
	 * @return
	 */
	public static PrintStream openClassFile(String outputDir, String filePath) {
		try {
			int i = filePath.indexOf(File.separatorChar);
//			String dirStr =
//				RunTimeUtil.getInstance().stripQuotes(
//						RESConfig.getInstance().getOutputDir().trim()
//				);
//			if(dirStr==null) dirStr="";
//			else { 
//				dirStr.replace('/',File.separatorChar);
//				if(dirStr.length()>0&&dirStr.lastIndexOf(File.separatorChar)!=dirStr.length()-1) {
//					dirStr+=File.separatorChar;
//				}
//			}
			File dir = null;
			String dirStr = "";
			do {
				dirStr+=filePath.substring(0, i)+File.separatorChar;
				filePath=filePath.substring(i+1);			
				dir = new File(dirStr);
				if (dir.exists())
					if (dir.isDirectory())
					;
					else {
						throw new CobolTranslationException("Cannot create output directory " + dir + ": a file of that name exists");
					}
				else 
					if(dir.mkdirs())
						;
					else {
						throw new CobolTranslationException("Cannot create output directory " + dir);
					}
				i = filePath.indexOf(File.separatorChar);
			} while (i>0);
			if(filePath!=null&&filePath.length()>0&&filePath.indexOf(".java")>0) {
				if(!RESConfig.getInstance().isOverwriteJavaFiles()) 
					if(new File(dir,filePath).exists()) {
							return null;
						}
					else
						if(new File(filePath).exists()) {
							return null;
						}
				File file;	
				if (dir !=null) {
					file=new File(dir,filePath);						
				}
				else {
					file=new File(filePath);
				}
				if(file.exists())
					file.delete();
				return new PrintStream(new FileOutputStream(file));
			}
		} catch(Exception e){
			e.printStackTrace();
			throw new CobolTranslationException("Could not open file " + filePath, null, -1, e);
		}
		return null;
	}
// DaoNH ADD End: Check and create directory for each Cobol file
	
	public static String getCurrentTime() {
		String time = "";
		Calendar cal =Calendar.getInstance();
		time+=new Integer(cal.get(Calendar.HOUR_OF_DAY)).toString().trim()+":"+
			new Integer(cal.get(Calendar.MINUTE)).toString().trim()+":"+
			new Integer(cal.get(Calendar.SECOND)).toString().trim();
		return time;
	}
	
	public static String getCurrentDate() {
		String date = "";
		Calendar cal =Calendar.getInstance();
		date+=new Integer(cal.get(Calendar.MONTH)).toString().trim()+"/"+
			new Integer(cal.get(Calendar.DAY_OF_MONTH)).toString().trim()+"/"+
			new Integer(cal.get(Calendar.YEAR)).toString().trim();
		return date;
	}
}
