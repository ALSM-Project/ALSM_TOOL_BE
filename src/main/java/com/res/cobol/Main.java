package com.res.cobol;

import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import java.util.ArrayList;

import com.res.cobol.parser.CobolParser;
import com.res.cobol.parser.CobolParserTokenManager;
import com.res.cobol.parser.JavaCharStream;
import com.res.cobol.parser.ParseException;
import com.res.cobol.parser.RESCharStream;
import com.res.cobol.parser.TokenMgrError;
import com.res.cobol.syntaxtree.CompilationUnit;
import com.res.cobol.visitor.TreeDumper;
import com.res.common.RESConfig;
import com.res.common.RESContext;
import com.res.java.translation.engine.Cobol2Java;
import com.res.java.translation.engine.CobolFillTable;
import com.res.java.translation.engine.CobolRecode;
import com.res.java.translation.engine.VsamIsam2DbGen;
import com.res.java.translation.symbol.SymbolProperties;
import com.res.java.translation.symbol.SymbolTable;
import com.res.java.util.ClassFile;


public class Main {

	private RESConfig config = RESConfig.getInstance();
	private static RESContext context = new RESContext();

	private File srcF;
	// DaoNH MOD Start: Moved this config to RESConfig Object
	// private boolean doListDir = false;
	// DaoNH MOD End: Moved this config to RESConfig Object
	// private boolean doListDir=true;
	private static Preprocessor preprocessor = new Preprocessor();
	private long startClock;
	
	private static ArrayList<String> headerLst;
	private static ArrayList<String> classLst;
	private static ArrayList<SymbolProperties> fileLst;
	
	public static void setHeaderList(ArrayList<String> headerlst) {
		headerLst=headerlst;
	}
	
	public static void setClassList(ArrayList<String> classlst) {
		classLst=classlst;
	}

	public static void setFileList(ArrayList<SymbolProperties> filelst) {
		fileLst=filelst;
	}
	
	class CobolFileFilter implements FilenameFilter {

		@Override
		public boolean accept(File arg0, String arg1) {
			if ((arg1 = arg1.trim()).length() <= 4)
				return false;
			arg1 = arg1.substring(arg1.length() - 4);
			if (arg1.equalsIgnoreCase(".COB") || arg1.equalsIgnoreCase(".CBL"))
				return true;
			return false;
		}

	}

	public static void main(String[] args) {

		Main instance = new Main();

		try {
			int i = instance.getOptions(args);

			// DaoNH MOD Start: Moved source to getOptions method
			/*
			 * if (args.length > 1) { int iFolder = Integer.parseInt(args[args.length -
			 * 1].trim()); if (iFolder == 1) { instance.doListDir = true; } else {
			 * instance.doListDir = false; }
			 * 
			 * }
			 */
			// DaoNH MOD End: Moved source to getOptions method
			instance.startClock = System.currentTimeMillis();

			System.out
					.println("akaMigration Cobol 2 Java (02/09/2020) - COPYRIGHT 2021");
			if (context.isProcessVerbose()) {
				System.out.println("Options: " + context.getOptionsVerbose().toString());
			}
			if (!RESConfig.getInstance().isDoListDir()) {
				instance.execute(args[i]);
			} else {
				File dotDir = new File(args[i]);
				System.out.println("The Java classes are under the folder: " + new File(".").getAbsolutePath());
				System.out.println("Classes from translation of programs will reside in the package: "
						+ RESConfig.getInstance().getProgramPackage());
				// DaoNH DEL Start: Source batch and model in the same directory
//				if (RESConfig.getInstance().getDataPackage() != null
//						&& RESConfig.getInstance().getDataPackage().length() > 0)
//					System.out.println("Classes from translation of data levels will reside in the package: "
//							+ RESConfig.getInstance().getDataPackage());
				// DaoNH DEL End: Source batch and model in the same directory
				
				// DaoNH MOD Start: Create new directory for each cobol program name
				for (String s : dotDir.list(instance.new CobolFileFilter())) {
					instance.execute(dotDir.getPath() + "\\" + s);
				}
				// DaoNH MOD Start: Create new directory for each cobol program name
			}
/*
			// DaoNH ADD Start: Skip copy file in case parsing process has error
			if (!RESConfig.getInstance().isInError()) {
			// DaoNH ADD End: Skip copy file in case parsing process has error
				String strpath = new File("").getAbsolutePath();
				java.nio.file.Path sourcePath = java.nio.file.Paths.get(strpath + "\\com\\service\\");
				java.nio.file.Path destinationPath = java.nio.file.Paths.get(strpath + "\\src\\main\\java\\com\\service\\");
				System.out.println("Start copy");
				System.out.println(strpath);
				try {
					File destDir = new File(strpath + "\\src\\main\\java\\com\\service");
					deleteFolder(destDir);
					System.out.println(strpath + "\\com\\service\\");
					System.out.println(strpath + "\\src\\main\\java\\com\\service\\");
					java.nio.file.Files.move(sourcePath, destinationPath,
							java.nio.file.StandardCopyOption.REPLACE_EXISTING);
					sourcePath = java.nio.file.Paths.get(strpath + "\\com\\");
					java.nio.file.Files.delete(sourcePath);
				} catch (IOException e) {
					// moving file failed.
					e.printStackTrace();
				}
			// DaoNH ADD Start: Skip copy file in case parsing java files processing has error
			}
			// DaoNH ADD End: Skip copy file in case parsing java files processing has error
*/
		} catch (Exception e) {
			System.err.println(args[args.length - 1]);
			e.printStackTrace();
		}

		System.out
				.println("Done in " + Math.round((System.currentTimeMillis() - instance.startClock) / 1000) + "s.\r\n");
		System.exit(0);
	}

	public void execute(String fileName) {
		File destF = null;
		try {

			System.err.println(fileName);

			RESConfig.getInstance().setInError(false);
	    	  context = new RESContext();
	    	  preprocessor = new Preprocessor();
			context.setSourceFileName(
					fileName.substring(fileName.replace('/', File.separatorChar).lastIndexOf(File.separatorChar) + 1));

			srcF = new File(fileName);

			// DaoNH MOD start: Directories recursive mode
			// if (!srcF.exists() || !srcF.isFile() || !srcF.canRead()) {
			if (!RESConfig.getInstance().isDoListDir() && (!srcF.exists() || !srcF.isFile() || !srcF.canRead())) {
				// DaoNH MOD End: Directories recursive mode
				System.out.println("Invalid Source File: " + fileName);
			} else {
				String dest = "$temporary.file";
				if (context.isProcessPreprocessOnly()) {
					preprocessor.preprocess(fileName, null);
				} else {
					destF = new File(dest);
					if (destF.exists())
						destF.delete();
					preprocessor.preprocess(fileName, dest);
					if (!destF.exists())
						return;			
					System.out.println("Parsing Cobol started for: " + srcF.getAbsolutePath() + "");
					// DaoNH MOD End: Support unicode processing
//					context.setSourceFile(new BufferedReader(
//							new InputStreamReader(new FileInputStream(destF), Charset.forName("SJIS"))));
					context.setSourceIn(new FileInputStream(destF));
//					if (!context.getSourceFile().ready())
					if (context.getSourceIn() == null)
						throw new ParseException();
					if (context.isProcessParseOnly()) {
						parse(context);
					} else {
						CompilationUnit unit = parse(context);
						if (RESConfig.getInstance().isInError()) {
							System.out.println("Errors encountered. Processing terminated.");
						} else
							translate(unit);
						unit = null;
					}

					// destF.delete();
					destF = null;
				}
			}
		} catch (TokenMgrError pe) {
			System.err.println(srcF.getAbsolutePath());
			pe.printStackTrace();
		} catch (ParseException pe) {
			System.err.println(srcF.getAbsolutePath());
			pe.printStackTrace();
		} catch (IOException eio) {
			System.err.println(srcF.getAbsolutePath());
			eio.printStackTrace();
		}

// DaoNH MOD start: Directories recursive mode
		// if (doListDir && srcF != null)
		if (RESConfig.getInstance().isDoListDir() && srcF != null) {
			RESConfig.getInstance().setOutputDir("");
			System.out.println("Done: " + srcF.getAbsolutePath() + "\n");
		}
// DaoNH MOD end: Directories recursive mode
		if (destF != null)
			destF.delete();
		if (context.getSourceFile() != null)
			try {
				context.getSourceFile().close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		context.setSourceFile(null);
		destF = null;
		srcF = null;
		CobolParserTokenManager.commentLines.clear();
		CobolParserTokenManager.lastToken = null;
		SymbolTable.clear();
		System.gc();
	}

	private CobolParser cobolParser = null;

	public CompilationUnit parse(RESContext context) {

		CompilationUnit unit;

		try {

			// DaoNH MOD Start: Support unicode processing
//			context.setCharStream(new RESCharStream(context.getSourceFile()));
//			context.setCharStream(new JavaCharStream(context.getSourceFile()));
			context.setCharStream(new JavaCharStream(context.getSourceIn(), "UTF-8"));		
			// DaoNH MOD End: Support unicode processing

			cobolParser = new CobolParser(new CobolParserTokenManager(context.getCharStream()));
			if (!context.isTraceOn() || context.getTraceLevel() % 2 == 0)
				cobolParser.disable_tracing();

			unit = cobolParser.CompilationUnit();

			if (context.isTraceOn() && context.getTraceLevel() % 2 != 0) {
				System.out.println("************************************************************************");
				TreeDumper dump = new TreeDumper();
				dump.visit(unit);
				return null;
			}

			context.getCharStream().Done();
			cobolParser = null;
		} catch (ParseException pe) {
			pe.printStackTrace();
			unit = null;
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			unit = null;
		}
		return unit;
	}

	public void translate(CompilationUnit unit) {

		try {
			// Create Symbol Table
			unit.accept(CobolFillTable.getInstance());
			if (RESConfig.getInstance().isInError()) {
				System.out.println("Errors encountered. Processing terminated.");
				return;
			}
			
			System.out.println("Translation to Java started.");
// DaoNH MOD Start: Moved directory recursive mode into RESConfig Object
			// if (!doListDir) {
			if (!RESConfig.getInstance().isDoListDir()) {
// DaoNH MOD End: Moved directory recursive mode into RESConfig Object
				printPackageHeader();
			}

			if (context.isTraceOn())
				System.out.println("Removing Dead Code...");

			// Do Dead-Code Removal and later Restructuring if any
			unit.accept(CobolRecode.getInstance());

			if (context.isTraceOn()) {
				System.out.println("************************************************************************");
				SymbolTable.getScope().display();
			}

			// Do cobol 2 java
			if (context.isTraceOn())
				System.out.println("Translating Cobol 2 Java...");

			unit.accept(Cobol2Java.getInstance());
			
			//For test
			/*
			for(String item : headerLst) {
				System.out.println(item);
			}
			for(String item : classLst) {
				System.out.println(item);
			}

			if (fileLst != null) {
				for (SymbolProperties item : fileLst) {
					System.out.println(item.getDataName() +" "+ item.getOtherName());
				}
			}
	*/



		// QuyTV 
		// Print tasklet declare
		// EMT ADD FOR TASKLET 2025
		ClassFile.doProgramScopeTaskLetTemplate();	
		//
		Cobol2Java.printTaskletDeclare(headerLst,classLst, fileLst);
		ClassFile.endProgramScope();
		
	    // EMD ADD STEP TEMPLATE 2025
		ClassFile.doProgramScopeConstTemplate();
		ClassFile.endProgramScope();
		//
		// EMD ADD STEP TEMPLATE 2025
	    ClassFile.doProgramScopeStepTemplate();
		ClassFile.endProgramScope();
		//
		 //QuyTV Print job template
		ClassFile.doProgramScopeJobTemplate();
		ClassFile.endProgramScope();

		
		SymbolTable.getScope().endProgram();

		if (config.isToGenVSAMISAMDb())
			genDB(unit);

		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

	private static final String DB_GEN_PACKAGE = ".db_gen";

	public void genDB(CompilationUnit unit) {
		System.out.println("Generation of classes to migrate VSAM/ISAM to DB tables started.");
		System.out.println("Migration classes reside in the package: " + RESConfig.getInstance().getProgramPackage()
				+ DB_GEN_PACKAGE);
		unit.accept(VsamIsam2DbGen.getInstance());
	}

	private static void printPackageHeader() {
		try {
			System.out.println("The java classes are under the folder: " + new File(".").getCanonicalPath());
		} catch (IOException e) {
			System.out.println("Errors encountered: " + e.getMessage() + ". Processing terminated.");
			return;
		}
		System.out.println("Classes from translation of programs reside in the package: "
				+ RESConfig.getInstance().getProgramPackage());
		if (RESConfig.getInstance().getDataPackage() != null && RESConfig.getInstance().getDataPackage().length() > 0)
			System.out.println("Classes from translation of data levels reside in the package: "
					+ RESConfig.getInstance().getDataPackage());
	}

	private int getOptions(String[] args) {
		int i = 0;

		if (i >= args.length)
			Main.displayProgramUsageAndExit();

		if (args[0].equalsIgnoreCase("-dbmigrate")) {
			return getDBMigrateOptions(args);
		}
		// process command line options
		for (i = 0; i < args.length; i++) {
			if (!args[i].startsWith("-"))
				break;
			if (args[i].substring(1).equalsIgnoreCase("dp")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				config.setDataPackage(args[i]);
				context.getOptionsVerbose().append("dataPackageName=").append(config.getDataPackage()).append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("odir")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				config.setOutputDir(args[i]);
			} else if (args[i].substring(1).equalsIgnoreCase("config")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				config.setConfigFile(args[i]);
			} else if (args[i].substring(1).equalsIgnoreCase("pp")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				config.setProgramPackage(args[i]);
				context.getOptionsVerbose().append("programPackageName=").append(config.getDataPackage()).append(';');
			} // DaoNH MOD start: Read input cobol source directory with recursive mode.
			else if (args[i].substring(1).equalsIgnoreCase("dld")) {
				config.setDoListDir(true);
			}
			// DaoNH MOD end: Read input cobol source directory with recursive mode.
			else if (args[i].substring(1).equalsIgnoreCase("dp0")) {
				config.setLongDataPackageName(false);
				context.getOptionsVerbose().append("longDataPackageName=").append("false").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("dp1")) {
				config.setLongDataPackageName(true);
				context.getOptionsVerbose().append("longDataPackageName=").append("true").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("fixed")) {
				preprocessor.setSourceFormat(Preprocessor.FORMAT_FIXED);
				context.getOptionsVerbose().append("sourceFormat=").append("fixed").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("free")) {
				preprocessor.setSourceFormat(Preprocessor.FORMAT_VARIABLE);
				config.setFixedFormat(false);
				context.getOptionsVerbose().append("sourceFormat=").append("free").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("e")) {
				context.setProcessPreprocessOnly(true);
			} else if (args[i].substring(1).equalsIgnoreCase("p")) {
				context.setProcessParseOnly(true);
			} else if (args[i].substring(1).equalsIgnoreCase("v")) {
				context.setProcessVerbose(true);
			} else if (args[i].substring(1).equalsIgnoreCase("c0")) {
				config.setPrintCobolStatementsAsComments(false);
				config.setRetainCobolComments(false);
				context.getOptionsVerbose().append("cobolComments=").append("false").append(';');
				context.getOptionsVerbose().append("cobolStatementsASComments=").append("false").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("c1")) {
				config.setPrintCobolStatementsAsComments(true);
				config.setRetainCobolComments(false);
				context.getOptionsVerbose().append("cobolComments=").append("false").append(';');
				context.getOptionsVerbose().append("cobolStatementsASComments=").append("true").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("c2")) {
				config.setPrintCobolStatementsAsComments(false);
				config.setRetainCobolComments(true);
				context.getOptionsVerbose().append("cobolComments=").append("true").append(';');
				context.getOptionsVerbose().append("cobolStatementsASComments=").append("false").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("c3")) {
				config.setPrintCobolStatementsAsComments(true);
				config.setRetainCobolComments(true);
				context.getOptionsVerbose().append("cobolComments=").append("true").append(';');
				context.getOptionsVerbose().append("cobolStatementsASComments=").append("true").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("overwrite")) {
				config.setOverwriteJavaFiles(true);
				context.getOptionsVerbose().append("overwrite=").append("true").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("nooverwrite")) {
				config.setOverwriteJavaFiles(false);
				context.getOptionsVerbose().append("overwrite=").append("false").append(';');
			}
// DaoNH DEL start: Moved this configuration to RESConfig Object
			/*
			 * else if (args[i].substring(1).equalsIgnoreCase("dolistdir")) { doListDir =
			 * true; }
			 */
// DaoNH DEL end: Moved this configuration to RESConfig Object
			else if (args[i].substring(1).equalsIgnoreCase("opt0")
					|| args[i].substring(1).equalsIgnoreCase("nojavatype")) {
				config.setOptimizeAlgorithm(0);
				context.getOptionsVerbose().append("javaType=").append("none").append(';');

			} else if (args[i].substring(1).equalsIgnoreCase("opt1")
					|| args[i].substring(1).equalsIgnoreCase("javatype")) {
				config.setOptimizeAlgorithm(1);
				context.getOptionsVerbose().append("javaType=").append("more").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("opt2")
					|| args[i].substring(1).equalsIgnoreCase("javatype2")) {
				config.setOptimizeAlgorithm(2);
				context.getOptionsVerbose().append("javaType=").append("most").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("allsym")) {
				config.setAllSymbols(true);
				context.getOptionsVerbose().append("allSymbols=").append("true").append(';');
			} else if (args[i].substring(1).equalsIgnoreCase("inline")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				config.setInlineStatement(Integer.parseInt(args[i]));
			} else if (args[i].substring(1).equalsIgnoreCase("gendb")) {
				config.setToGenVSAMISAMDb(true);
			} else if (args[i].substring(1).equalsIgnoreCase("nogendb")) {
				config.setToGenVSAMISAMDb(false);
			} else if (args[i].substring(1).equalsIgnoreCase("trace")) {
				if (++i >= args.length || args[i].startsWith("-")) {
					System.out.println("-trace option must specify PARSER or TRANSLATION OR ALL");
					break;
				}
				if (args[i].equalsIgnoreCase("PARSER"))
					context.setTraceLevel(1);
				else if (args[i].equalsIgnoreCase("TRANSLATION"))
					context.setTraceLevel(2);
				else
					context.setTraceLevel(3);
				context.setTraceOn(true);
			} else
				displayProgramUsageAndExit();
		}
		if (i >= args.length)
			Main.displayProgramUsageAndExit();

		return i;
	}

	// private String migrateVSAMISAMFileName=null;
	// private String migrateDataFileName=null;
	private int getDBMigrateOptions(String[] args) {
		int i = 1;
		// process command line options
		for (i = 0; i < args.length; i++) {
			if (!args[i].startsWith("-"))
				break;
			if (args[i].substring(1).equalsIgnoreCase("dp")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				config.setDataPackage(args[i]);
			} else if (args[i].substring(1).equalsIgnoreCase("file")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				// migrateVSAMISAMFileName=args[i];
			} else if (args[i].substring(1).equalsIgnoreCase("data")) {
				if (++i >= args.length || args[i].startsWith("-"))
					break;
				// migrateDataFileName=args[i];
			}
		}
		return i;
	}

	public static RESContext getContext() {
		return context;
	}
	
	public static Preprocessor getPreprocessor() {
		return preprocessor;
	}

	private static void displayProgramUsageAndExit() {
		System.out.println("\nUsage: java -jar AkaMig.jar [options] <Source-File-Name>");
		System.out.println("\nOptions for COBOL translation may one or more of the following.\n");
		System.out.println("\t-odir output-direcory: specifies directory where target packages reside.");
		System.out.println("\t-dp package-name: spcifies the package for data classes.");
		System.out.println("\t-pp Package-Name: spcifies the package for program classes.");
		System.out.println("\n\t-dp0 \t\t: creates packages like \"coboldataclasses\".");
		System.out.println("\t-dp1 \t\t: creates packages like \"coboldataclasses.programname\".");
		System.out.println("\n\t-fixed \t\t: fixed source format");
		System.out.println("\t-free  \t\t: free/variable source format");
		System.out.println("\n\t-e \t\t: run only preprocesser.");
		System.out.println("\t-p \t\t: run only preprocesser and parser");
		System.out.println("\n\t-c0 \t\t: target Java files contain no Cobol comments.");
		System.out.println("\t-c1 \t\t: print Cobol statements in comments.");
		System.out.println("\t-c2 \t\t: retain Cobol comments.");
		System.out.println("\t-c3 \t\t: retain Cobol comments and statements in comments.");
		System.out.println("\n\t-nojavatype \t ");
		System.out.println("\t-opt0 \t\t: force use  of CobolBytes stream.");
		System.out.println("\n\t-javatype  ");
		System.out.println("\t-opt1 \t\t: use native Java types whereever possible.");
		System.out.println("\n\t-javatype2  ");
		System.out.println("\t-opt2 \t\t: use native Java types for all. enables cache.");
		System.out.println("\n\t-allsym\t\t: translate all symbols including not REFeferenced or MODified.");
		System.out.println("\n\t-inline stmt-count: Inline all paras & sections with no. of statements < count.");
		System.out.println("\n\t-gendb\t\t: generate db migration classes for ISAM/VSAM files.");
		System.out.println("\t-nogendb\t: suppress generation of db migration classes.");
		System.out.println("\n\t-v \t\t: verbose output.");
		System.out.println("\nThese options override the defaults in Config.properties file.");
		System.out.println("\nOR\n\nUsage: java -jar RES.jar -dbmigrate [options]");
		System.out.println("\nOptions for VSAM/ISAM Migration may be one or more of the following.\n");
		System.out.println("\t-dp Package-Name: spcifies the package for program classes.");
		System.out.println("\t    Default package is coboldataclasses.db_gen from translation.");
		System.out.println("\n\t-file File-name: specifies the COBOL FD name for VSAM/ISAM file.");
		System.out.println("\t    Default is  all files generated from translation.");
		System.out.println("\n\t-data File-name: specifies the sequential file with VSAM/ISAM data.");
		System.out.println("\t    Default is ASSIGN name of generated file.");
		System.out.println("\n\t-readback\t: read back from DB and compare for validation.");
		System.out.println("\nThese options use DB configuraion in RunConfig.properties file.");
		System.exit(1);
	}

	public static void deleteFolder(File folder) {
		File[] files = folder.listFiles();
		if (files != null) { // some JVMs return null for empty dirs
			for (File f : files) {
				if (f.isDirectory()) {
					deleteFolder(f);
				} else {
					f.delete();
				}
			}
		}
		folder.delete();
	}
}
