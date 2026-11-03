package com.res.java.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import com.res.cobol.Main;
import com.res.common.RESConfig;
import com.res.java.translation.symbol.SymbolConstants;
import com.res.java.translation.symbol.SymbolProperties;


import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.springframework.beans.factory.annotation.Autowired;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


public class ProgramClassFileWriter extends ClassFileWriter {
	
	private ArrayList<String> headerLst =new ArrayList<>();
	private ArrayList<String> classLst =new ArrayList<>();

	public ProgramClassFileWriter(SymbolProperties props) {
		super(Files.openClassFile(props, false));
		super.setClassName(NameUtil.getClassName(props.getDataName(), "", false));
		setData(false);
		if (RESConfig.getInstance().isGenerateBeanInfo()) {
			setBeanInfoPrintStream(Files.openBeanInfoFile(props, false));
		}
		printHeader(props, false);
	}

	//QuyTV Create job template
	public ProgramClassFileWriter(SymbolProperties props, boolean isJobTemplate) {
		super(
				Files.openClassFile(props,false)
			);
		super.setClassName(NameUtil.getClassName(props.getDataName(),"",false));
		setData(false);
		if(RESConfig.getInstance().isGenerateBeanInfo()) {
			setBeanInfoPrintStream(Files.openBeanInfoFile(props,false));
		}
		printJobTemplate(props);
	}
	
	// QuyTV print Job Template
	public void printJobTemplate(SymbolProperties props) {
		String s;
		if ((s = NameUtil.getPackageName(props, true)) != null && s.length() > 0) {
			// s+=NameUtil.getJavaName(props,false);
			s=s.replace("src.main.java.", "");
			s+="."+ props.getJavaName1().replace("src.main.", "");
			println("package " + s + ";");
			println("package " + s + ";", true);
		}
		println("");
		println("import org.springframework.batch.core.Job;");
		println("import org.springframework.batch.core.Step;");
		println("import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;");
		println("import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;");
		println("import org.springframework.batch.core.launch.support.RunIdIncrementer;");
		println("import org.springframework.batch.core.step.tasklet.Tasklet;");		
		println("import org.springframework.beans.factory.annotation.Autowired;");
		println("import org.springframework.context.annotation.Bean;");
		println("import org.springframework.context.annotation.Configuration;");
		println("");
		println("import jp.co.alsm.config.BaseJobExecutionListener;");
		println("");		
		println("@Configuration ");
		println("public class "+ props.getJavaName2() +"Job {");
		println("");			
		println("	@Autowired");
		println("	private JobBuilderFactory jobs;");
		println("");
		println("	@Autowired");
		println("	private StepBuilderFactory steps;");
		println("");
		println("	@Autowired");
		println("	private BaseJobExecutionListener jobListener;");
		println("");
		println("	@Bean(name =\""+ props.getJavaName2()+"\" )");
		println("	public Job "+ props.getJavaName1()+"("+ props.getJavaName2()+"Tasklet "+ props.getJavaName1()+"Tasklet) {");
		println("		return jobs.get(\""+props.getJavaName2()+"\").incrementer(new RunIdIncrementer()).start("+ props.getJavaName1()+"Step("+
				                props.getJavaName1()+"Tasklet))");
		println("				.listener(jobListener).build();");
		println("	}");
		println("");
		println("	@Bean");
		println("	public Step "+props.getJavaName1()+"Step(Tasklet tasklet) {");
		println("		return steps.get(\""+props.getJavaName2()+"Step\").tasklet(tasklet).build();");
		println("	}");
	}
	
	public void printHeader(SymbolProperties props, boolean isNested) {
		if (!isNested) {
			String s;
			if ((s = NameUtil.getPackageName(props, true)) != null && s.length() > 0) {
				// s+=NameUtil.getJavaName(props,false);
				s=s.replace("src.main.java.", "");
				s+="."+ props.getJavaName1().replace("src.main.", "");
				/*
				println("package " + s + ";");
				println("package " + s + ";", true);
				*/
				headerLst.add("package " + s + ";");
			}
			/*
			println("/************************************************************************");
			println(" ** RES generated this class file from Cobol program " + props.getDataName().toUpperCase()
					+ " in source file " + Main.getContext().getSourceFileName().toUpperCase());
			println(" ** Generated at time " + RESUtil.getTime() + " on " + RESUtil.getDayOfWeek() + ", "
					+ RESUtil.getDate());
			if (Main.getContext().isProcessVerbose()) {
				println(" ** " + Main.getContext().getOptionsVerbose().toString());
			}
			println(" ************************************************************************");

			*/
			
			//printImports(props);
			/*
			println("");
			
			println("import java.io.IOException;");			
			println("import java.util.List;");
			println("import java.util.Map;");

			println("");
			*/
			headerLst.add("");
			headerLst.add("import java.io.IOException;");			
			headerLst.add("import java.util.List;");
			headerLst.add("import java.util.Map;");
			headerLst.add("");

			/* QuyTV add import lib */
			/*
			println("import org.springframework.batch.core.StepContribution;");
			println("import org.springframework.batch.core.configuration.annotation.StepScope;");
			println("import org.springframework.batch.core.scope.context.ChunkContext;");
			println("import org.springframework.batch.core.step.tasklet.Tasklet;");
			println("import org.springframework.batch.repeat.RepeatStatus;");
			println("import org.springframework.beans.factory.annotation.Autowired;");
			println("import org.springframework.beans.factory.annotation.Value;");
			println("import org.springframework.stereotype.Component;");
			println("");
*/
			headerLst.add("import org.springframework.batch.core.StepContribution;");
			headerLst.add("import org.springframework.batch.core.configuration.annotation.StepScope;");
			headerLst.add("import org.springframework.batch.core.scope.context.ChunkContext;");
			headerLst.add("import org.springframework.batch.core.step.tasklet.Tasklet;");
			headerLst.add("import org.springframework.batch.repeat.RepeatStatus;");
			headerLst.add("import org.springframework.beans.factory.annotation.Autowired;");
			headerLst.add("import org.springframework.beans.factory.annotation.Value;");
			headerLst.add("import org.springframework.stereotype.Component;");
			headerLst.add("");
			headerLst.add("import jp.co.alsm.common.Record;");
			headerLst.add("import jp.co.alsm.common.Console;");
			headerLst.add("import jp.co.alsm.common.FileRecord;");
			headerLst.add("import jp.co.alsm.common.InvalidKeyException;");
			headerLst.add("import jp.co.alsm.helper.CobolFunction;");
			headerLst.add("import jp.co.alsm.helper.Compare;");
			headerLst.add("import jp.co.alsm.helper.FieldFormat;");
			headerLst.add("import jp.co.alsm.helper._Math;");
			headerLst.add("import jp.co.alsm.logging.Logger;");
			headerLst.add("");
			headerLst.add("import java.math.BigDecimal;");
			headerLst.add("");			
			headerLst.add("import lombok.Getter;");			
			headerLst.add("import lombok.Setter;");
			headerLst.add("");
			
			if (Main.getContext().isSqlTranslated())
				headerLst.add("import java.sql.SQLException;");
			headerLst.add("");
			/*
				println("import java.sql.SQLException;");
			println("");
			*/
			// QuyTV
			/*
			println("@StepScope");
			println("@Component");
			println("public class "+props.getJavaName2()+"Tasklet"+" extends Tasklet {");
			println(" ");
			println("	protected Logger logger;");
// DaoNH ADD Start: Auto wired Model class
			println("");
			println("	@Autowired");
			// println("	private "+props.getJavaName2()+"Model " + props.getJavaName1() + "Model;");
			println("	private "+props.getJavaName2()+"Model " + "model;");
// DaoNH ADD End: Auto wired Model class
			println("");
			println("	public "+props.getJavaName2()+"Tasklet() {");
			println("		this.logger = new Logger(getClass());");
			println("	}");
			println("");
			println("public class " + props.getJavaName2() + "BeanInfo extends RESBeanInfo {", true);
			*/
			classLst.add("@Component");
			classLst.add("@StepScope");
			classLst.add("public class "+props.getJavaName2()+"Tasklet implements Tasklet {");
			classLst.add("");
			classLst.add("tab()");
			classLst.add("protected Logger logger;");
			classLst.add("");			
			classLst.add("@Autowired");
			classLst.add("private "+props.getJavaName2()+"Model " + " model;");
			classLst.add("");
			classLst.add("@Value(\"#{jobParameters}\")");
			classLst.add("public Map<String, Object> jobParameters;");
			classLst.add("");
			//classLst.add("@Getter");
			//classLst.add("@Setter");
			classLst.add("private List<?> csr1;");
			classLst.add("");
			classLst.add("public "+props.getJavaName2()+"Tasklet() {");
			classLst.add("tab()");
			classLst.add("this.logger = new Logger(getClass());");
			classLst.add("backTab()");
			classLst.add("}");
			classLst.add("");
			
		} else {
			//println("/************************************************************************");
			//println(" ** RES generated this class from Nested Cobol program "+
			//		props.getDataName().toUpperCase()+" in source file "+Main.getContext().getSourceFileName().toUpperCase());
			//println(" ************************************************************************/");
		/*
			println("public class "+props.getJavaName2()+"Tasklet"+" extends Tasklet {");
			println(" ");
			println("	protected Logger logger;");
			*/
			classLst.add("public class "+props.getJavaName2()+"Tasklet implements Tasklet {");
			classLst.add(" ");
			classLst.add("	protected Logger logger;");
		}
		Main.setHeaderList(headerLst);
		Main.setClassList(classLst);
		// Replace content of pom.xml file
		// Replace(props.getJavaName2());
	}

	private void printImports(SymbolProperties props) {
		ArrayList<SymbolProperties> a = props.getChildren();
		if (a == null)
			return;
		for (SymbolProperties child : a) {
			if (child.getType() == SymbolConstants.FILE || child.getType() == SymbolConstants.PROGRAM) {
				printImports(child);
			} else
				printSingleImport(child);

		}
	}

	private void printSingleImport(SymbolProperties o2) {
		if (!o2.is01Group() || !(o2.getRef() || o2.getMod()) || o2.isFromRESLibrary())
			return;
		String name2 = o2.getJavaName2();
		println("import " + NameUtil.getPackageName(o2, false) + "." + name2 + ";");
	}

	private static void Replace(String strReplace) {

		try {
			String filepath = new File("").getAbsolutePath();
			filepath = filepath + "\\pom.xml";
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(filepath);

			// Get the root element
			Node project = doc.getFirstChild();

			NodeList list = project.getChildNodes();

			for (int i = 0; i < list.getLength(); i++) {

				Node node = list.item(i);

				if ("artifactId".equals(node.getNodeName())) {
					node.setTextContent(strReplace);
				}

				if ("name".equals(node.getNodeName())) {
					node.setTextContent(strReplace);
				}
			}

			// write the content into xml file
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			DOMSource source = new DOMSource(doc);
			StreamResult result = new StreamResult(new File(filepath));
			transformer.transform(source, result);

			System.out.println("Done");

		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		} catch (TransformerException tfe) {
			tfe.printStackTrace();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		} catch (SAXException sae) {
			sae.printStackTrace();
		}
	}
}
