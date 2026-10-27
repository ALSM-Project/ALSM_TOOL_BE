package com.alsm.runtime.batch.model05;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.alsm.runtime.common.Record;
import com.alsm.runtime.common.Console;
import com.alsm.runtime.helper.Compare;
import com.alsm.runtime.logging.Logger;

import lombok.Getter;
import lombok.Setter;


@Component
@StepScope
public class Model05Tasklet implements Tasklet {

	protected Logger logger;
	
	@Autowired
	private Model05Model  model;
	
	@Value("#{jobParameters}")
	public Map<String, Object> jobParameters;
	
	private List<?> csr1;
	
	public Model05Tasklet() {
		this.logger = new Logger(getClass());
	}
	
	
	@Override
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
		try {
			//TODO Need to check called fuction
			mainControl();
			} catch (Exception e) {
				this.logger.error(e.getMessage());
				return RepeatStatus.FINISHED;
			}
			
		return RepeatStatus.FINISHED;
	}
	public void mainControl() {
		Console.println(model.getModelName()+"STARTED");
		case_0001();
		case_0002();
		case_0003();
		case_0004();
		case_0005();
		case_0006();
		Console.println(model.getModelName()+"ENDED");
	}
	public void case_0001() {
		model.setCnt(0);
		model.getTimeArea_1().setTime_01(Console.readTime());
		while(model.getCnt() <= model.getCntMax()) {
			model.setB(model.getA() + model.getB());
			model.setCnt(1 + model.getCnt());
		}
		model.getTimeArea_1().setTime_02(Console.readTime());
		model.getTimeArea_1().setTime_03(model.getTimeArea_1().getTime_02() - model.getTimeArea_1().getTime_01());
		Console.println("ADD      TIME = "+model.getTimeArea_1().getTime_03());
	}
	public void case_0002() {
		model.setCnt(0);
		model.getTimeArea_1().setTime_04(Console.readTime());
		while(model.getCnt() <= model.getCntMax()) {
			model.setC(model.getB() - model.getA());
			model.setCnt(1 + model.getCnt());
		}
		model.getTimeArea_1().setTime_05(Console.readTime());
		model.getTimeArea_1().setTime_06(model.getTimeArea_1().getTime_05() - model.getTimeArea_1().getTime_04());
		Console.println("SUBTRACT TIME = "+model.getTimeArea_1().getTime_06());
	}
	public void case_0003() {
		model.setCnt(0);
		model.getTimeArea_1().setTime_07(Console.readTime());
		while(model.getCnt() <= model.getCntMax()) {
			model.setC(model.getA() * model.getB());
			model.setCnt(1 + model.getCnt());
		}
		model.getTimeArea_1().setTime_08(Console.readTime());
		model.getTimeArea_1().setTime_09(model.getTimeArea_1().getTime_08() - model.getTimeArea_1().getTime_07());
		Console.println("MULTIPLY TIME = "+model.getTimeArea_1().getTime_09());
	}
	public void case_0004() {
		model.setCnt(0);
		model.getTimeArea_1().setTime_10(Console.readTime());
		while(model.getCnt() <= model.getCntMax()) {
			model.setC(model.getA() / model.getB());
			model.setCnt(1 + model.getCnt());
		}
		model.getTimeArea_1().setTime_11(Console.readTime());
		model.getTimeArea_1().setTime_12(model.getTimeArea_1().getTime_11() - model.getTimeArea_1().getTime_10());
		Console.println("DIVIDE   TIME = "+model.getTimeArea_1().getTime_12());
	}
	public void case_0005() {
		model.setCnt(0);
		model.getTimeArea_1().setTime_13(Console.readTime());
		while(model.getCnt() <= model.getCntMax()) {
			model.setC(model.getA() + model.getB());
			model.setCnt(1 + model.getCnt());
		}
		model.getTimeArea_1().setTime_14(Console.readTime());
		model.getTimeArea_1().setTime_16(model.getTimeArea_1().getTime_14() - model.getTimeArea_1().getTime_13());
		Console.println("COMPUTE  TIME = "+model.getTimeArea_1().getTime_16());
	}
	public void case_0006() {
		model.setCnt(0);
		model.getTimeArea_1().setTime_17(Console.readTime());
		while(model.getCnt() <= model.getCntMax()) {
			model.setB(model.getA());
			model.setCnt(1 + model.getCnt());
		}
		model.getTimeArea_1().setTime_18(Console.readTime());
		model.getTimeArea_1().setTime_19(model.getTimeArea_1().getTime_18() - model.getTimeArea_1().getTime_17());
		Console.println("MOVE     TIME = "+model.getTimeArea_1().getTime_19());
	}
}
