package jp.co.fpt.batch.model04;

import org.springframework.stereotype.Component;

import jp.co.fpt.common.Constant;
import jp.co.fpt.common.Record;
import jp.co.fpt.helper.StringHelper;
import lombok.Getter;
import lombok.Setter;

@Component
@Getter
@Setter
public class Model04Model {

	private String modelName;
	private int cnt;
	private int cntMax;
	private Record a;
	private Record recordA;
	private int b;
	private int c;
	private TimeArea_1 timeArea_1;
	
	public Model04Model(){
		setModelName("MODEL04");
		setCnt(0);
		setCntMax(10000000);
		recordA =  new Record(new byte[5]);
		a = recordA.subSequence(0,4).withFormat("9(005)").withOption("COMP-3");
		setA(5);
		setB(3);
		setC(0);
		timeArea_1 = new TimeArea_1();
	}
	
	public int getA() {
		return recordA.toInt();
	}
	
	public void setA(int value) {
		recordA.set(value, recordA.getOffset(), recordA.getEnd());
	}
	public class TimeArea_1{
		
		private Record time_01;
		private Record time_02;
		private Record time_03;
		private Record time_04;
		private Record time_05;
		private Record time_06;
		private Record time_07;
		private Record time_08;
		private Record time_09;
		private Record time_10;
		private Record time_11;
		private Record time_12;
		private Record time_13;
		private Record time_14;
		private Record time_15;
		private Record time_16;
		private Record time_17;
		private Record time_18;
		private Record time_19;
		private Record record = new Record(new byte[Length]);
		public byte[] getDatas() {
			return record.getDatas();
		}
		
		public TimeArea_1() {
			time_01 = record.subSequence(0,8).withFormat("9(08)");
			setTime_01(0);
			time_02 = record.subSequence(8,16).withFormat("9(08)");
			setTime_02(0);
			time_03 = record.subSequence(16,24).withFormat("9(08)");
			setTime_03(0);
			time_04 = record.subSequence(24,32).withFormat("9(08)");
			setTime_04(0);
			time_05 = record.subSequence(32,40).withFormat("9(08)");
			setTime_05(0);
			time_06 = record.subSequence(40,48).withFormat("9(08)");
			setTime_06(0);
			time_07 = record.subSequence(48,56).withFormat("9(08)");
			setTime_07(0);
			time_08 = record.subSequence(56,64).withFormat("9(08)");
			setTime_08(0);
			time_09 = record.subSequence(64,72).withFormat("9(08)");
			setTime_09(0);
			time_10 = record.subSequence(72,80).withFormat("9(08)");
			setTime_10(0);
			time_11 = record.subSequence(80,88).withFormat("9(08)");
			setTime_11(0);
			time_12 = record.subSequence(88,96).withFormat("9(08)");
			setTime_12(0);
			time_13 = record.subSequence(96,104).withFormat("9(08)");
			setTime_13(0);
			time_14 = record.subSequence(104,112).withFormat("9(08)");
			setTime_14(0);
			time_15 = record.subSequence(112,120).withFormat("9(08)");
			setTime_15(0);
			time_16 = record.subSequence(120,128).withFormat("9(08)");
			setTime_16(0);
			time_17 = record.subSequence(128,136).withFormat("9(08)");
			setTime_17(0);
			time_18 = record.subSequence(136,144).withFormat("9(08)");
			setTime_18(0);
			time_19 = record.subSequence(144,152).withFormat("9(08)");
			setTime_19(0);
		}
		
		public int getTime_01() {
			return time_01.toInt();
		}
		
		public void setTime_01(int value) {
			time_01.set(value, time_01.getOffset(), time_01.getEnd());
		}
		
		public int getTime_02() {
			return time_02.toInt();
		}
		
		public void setTime_02(int value) {
			time_02.set(value, time_02.getOffset(), time_02.getEnd());
		}
		
		public int getTime_03() {
			return time_03.toInt();
		}
		
		public void setTime_03(int value) {
			time_03.set(value, time_03.getOffset(), time_03.getEnd());
		}
		
		public int getTime_04() {
			return time_04.toInt();
		}
		
		public void setTime_04(int value) {
			time_04.set(value, time_04.getOffset(), time_04.getEnd());
		}
		
		public int getTime_05() {
			return time_05.toInt();
		}
		
		public void setTime_05(int value) {
			time_05.set(value, time_05.getOffset(), time_05.getEnd());
		}
		
		public int getTime_06() {
			return time_06.toInt();
		}
		
		public void setTime_06(int value) {
			time_06.set(value, time_06.getOffset(), time_06.getEnd());
		}
		
		public int getTime_07() {
			return time_07.toInt();
		}
		
		public void setTime_07(int value) {
			time_07.set(value, time_07.getOffset(), time_07.getEnd());
		}
		
		public int getTime_08() {
			return time_08.toInt();
		}
		
		public void setTime_08(int value) {
			time_08.set(value, time_08.getOffset(), time_08.getEnd());
		}
		
		public int getTime_09() {
			return time_09.toInt();
		}
		
		public void setTime_09(int value) {
			time_09.set(value, time_09.getOffset(), time_09.getEnd());
		}
		
		public int getTime_10() {
			return time_10.toInt();
		}
		
		public void setTime_10(int value) {
			time_10.set(value, time_10.getOffset(), time_10.getEnd());
		}
		
		public int getTime_11() {
			return time_11.toInt();
		}
		
		public void setTime_11(int value) {
			time_11.set(value, time_11.getOffset(), time_11.getEnd());
		}
		
		public int getTime_12() {
			return time_12.toInt();
		}
		
		public void setTime_12(int value) {
			time_12.set(value, time_12.getOffset(), time_12.getEnd());
		}
		
		public int getTime_13() {
			return time_13.toInt();
		}
		
		public void setTime_13(int value) {
			time_13.set(value, time_13.getOffset(), time_13.getEnd());
		}
		
		public int getTime_14() {
			return time_14.toInt();
		}
		
		public void setTime_14(int value) {
			time_14.set(value, time_14.getOffset(), time_14.getEnd());
		}
		
		public int getTime_15() {
			return time_15.toInt();
		}
		
		public void setTime_15(int value) {
			time_15.set(value, time_15.getOffset(), time_15.getEnd());
		}
		
		public int getTime_16() {
			return time_16.toInt();
		}
		
		public void setTime_16(int value) {
			time_16.set(value, time_16.getOffset(), time_16.getEnd());
		}
		
		public int getTime_17() {
			return time_17.toInt();
		}
		
		public void setTime_17(int value) {
			time_17.set(value, time_17.getOffset(), time_17.getEnd());
		}
		
		public int getTime_18() {
			return time_18.toInt();
		}
		
		public void setTime_18(int value) {
			time_18.set(value, time_18.getOffset(), time_18.getEnd());
		}
		
		public int getTime_19() {
			return time_19.toInt();
		}
		
		public void setTime_19(int value) {
			time_19.set(value, time_19.getOffset(), time_19.getEnd());
		}
		public static final int Length = 152;
		
		
		public void write(String value) {
			record.set(value, 0, Length);
		}
		
		public String toString() {
			return record.toString();
		}
		
	}
}
}
