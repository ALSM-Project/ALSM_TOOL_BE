package com.res.java.util;
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
import com.res.java.translation.symbol.SymbolTable;

public class Files {
	
	public static PrintStream openClassFile(SymbolProperties props,boolean isData) {
		//Create file and folder for output
			String path=NameUtil.getPackageName(props,!isData).replace('.',File.separatorChar);
// DaoNH MOD Start: Check and make directory for each Cobol program
		String outputDir = "";
		String filePath = "";
//		path+=File.separatorChar+NameUtil.getFileName(props,isData);
		if(RESConfig.getInstance().getOutputDir().isEmpty()) {
			// 2025 Code doi ung cho create fill, folder
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
			//2025 Code doi ung for create File/Folder
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
			// Was `do { ... } while (i>0)`: when path has no separator at all (a bare file
			// name, e.g. just "Cbact01cTasklet.java"), indexOf returns -1 and the do-while's
			// unconditional first iteration called path.substring(0, -1), throwing
			// StringIndexOutOfBoundsException and aborting translation for the whole program
			// with no real Java code ever written beyond the header. Checking the condition
			// before entering the loop (while, not do-while) skips directory-splitting
			// entirely for a bare file name instead of crashing on it.
			while (i>0) {
				dirStr+=path.substring(0, i)+File.separatorChar;
				path=path.substring(i+1);
				dir = new File(dirStr);
				if (dir.exists())
					if (dir.isDirectory())
					;
					else {
						System.out.println("Cannot create output directory. File of that name exists.");
						System.exit(-1);
					}
				else
					if(dir.mkdir())
						;
					else {
						System.out.println("Cannot create output directory. Failed.");
						System.exit(-1);
					}
				i = path.indexOf(File.separatorChar);
			}
			if (dir == null && dirStr.length() > 0) {
				// path was a bare file name - still honor the configured output directory
				// (dirStr, derived from RESConfig above) instead of silently falling back to
				// the process's current working directory.
				dir = new File(dirStr);
				if (!dir.exists()) dir.mkdirs();
			}
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
			System.out.println("Could not open file "+path);
			System.out.println("Errors Encountered. Translation terminated.");
			System.exit(0);
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
			// Real crash reproduced against actual CardDemo COBOL sources (e.g.
			// CBACT01C.CBL): filePath is frequently a bare file name with no directory
			// separator at all once outputDir has already been folded into it by the
			// caller (see openClassFile(SymbolProperties,boolean) above). indexOf then
			// returns -1, and the old `do { ... } while (i>0)` still ran its body once
			// unconditionally, calling filePath.substring(0, -1) ->
			// StringIndexOutOfBoundsException("Range [0, -1) out of bounds"), caught by
			// the catch block below, which printed the stack trace and called
			// System.exit(0) - silently terminating translation for the entire program
			// after only the header comment had been written. Checking the loop
			// condition before entering (while, not do-while) fixes this exact crash.
			while (i>0) {
				dirStr+=filePath.substring(0, i)+File.separatorChar;
				filePath=filePath.substring(i+1);
				dir = new File(dirStr);
				if (dir.exists())
					if (dir.isDirectory())
					;
					else {
						System.out.println("Cannot create output directory. File of that name exists.");
						System.exit(-1);
					}
				else
					if(dir.mkdir())
						;
					else {
						System.out.println("Cannot create output directory. Failed.");
						System.exit(-1);
					}
				i = filePath.indexOf(File.separatorChar);
			}
			if (dir == null && outputDir != null && !outputDir.isEmpty()) {
				// filePath had no directory component of its own - still honor the
				// outputDir the caller explicitly asked for instead of silently falling
				// back to the process's current working directory.
				dir = new File(outputDir);
				if (!dir.exists()) dir.mkdirs();
			}
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
			System.out.println("Could not open file "+filePath);
			System.out.println("Errors Encountered. Translation terminated.");
			System.exit(0);
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
