package jp.co.alsm.helper;

import java.math.BigDecimal;
import java.math.RoundingMode;

public class _Math {

	
	public static int add(int arg1,int arg2) {
		return arg1+arg2;
	}
	public static long add(long arg1,long arg2) {
		return arg1+arg2;
	}
	public static double add(double arg1,double arg2) {
		return arg1+arg2;
	}
	public static BigDecimal add(BigDecimal arg1,double arg2) {
		return arg1.add(getBigDecimal(arg2)).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal add(double arg1,BigDecimal arg2) {
		return arg2.add(getBigDecimal(arg1)).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal add(BigDecimal arg1,BigDecimal arg2) {
		return arg2.add(arg1).setScale(18,RoundingMode.HALF_UP);
	}
	
	
	public static double subtract(double arg1,double arg2) {
		return arg1-arg2;
	}
	public static BigDecimal subtract(BigDecimal arg1,double arg2) {
		return arg1.subtract(getBigDecimal(arg2)).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal subtract(double arg1,BigDecimal arg2) {
		return getBigDecimal(arg1).subtract(arg2).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal subtract(BigDecimal arg1,BigDecimal arg2) {
		return arg1.subtract(arg2).setScale(18,RoundingMode.HALF_UP);
	}
	public static int subtract(int arg1,int arg2) {
		return arg1-arg2;
	}
	public static long subtract(long arg1,long arg2) {
		return arg1-arg2;
	}

	public static int multiply(int arg1,int arg2) {
		return arg1*arg2;
	}
	public static long multiply(long arg1,long arg2) {
		return arg1*arg2;
	}
	public static double multiply(double arg1,double arg2) {
		return arg1*arg2;
	}
	public static BigDecimal multiply(BigDecimal arg1,double arg2) {
		return arg1.multiply(getBigDecimal(arg2)).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal multiply(double arg1,BigDecimal arg2) {
		return arg2.multiply(getBigDecimal(arg1)).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal multiply(BigDecimal arg1,BigDecimal arg2) {
		return arg2.multiply(arg1).setScale(18,RoundingMode.HALF_UP);
	}

	public static double divide(double arg1,double arg2) {
		return arg1/arg2;
	}
	public static BigDecimal divide(BigDecimal arg1,double arg2) {
		return arg1.divide(getBigDecimal(arg2), RoundingMode.HALF_UP);
	}
	public static BigDecimal divide(double arg1,BigDecimal arg2) {
		return getBigDecimal(arg1/arg2.doubleValue()).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal divide(BigDecimal arg1,BigDecimal arg2) {
		return arg1.setScale(18,RoundingMode.HALF_UP).divide(arg2.setScale(18,RoundingMode.HALF_UP), RoundingMode.HALF_UP);
	}
	public static int divide(int arg1,int arg2) {
		return arg1/arg2;
	}
	public static long divide(long arg1,long arg2) {
		return arg1/arg2;
	}

	public static double remainder(double arg1,double arg2) {
		return arg1%arg2;
	}
	public static BigDecimal remainder(BigDecimal arg1,double arg2) {
		return arg1.remainder(getBigDecimal(arg2)).setScale(18,RoundingMode.HALF_UP);
	}
	public static BigDecimal remainder(double arg1,BigDecimal arg2) {
		return arg2.remainder(getBigDecimal(arg1)).setScale(0,RoundingMode.HALF_UP);
	}
	public static BigDecimal remainder(BigDecimal arg1,BigDecimal arg2) {
		return arg2.remainder(arg1).setScale(18,RoundingMode.HALF_UP);
	}
	public static int remainder(int arg1,int arg2) {
		return arg1%arg2;
	}
	public static long remainder(long arg1,long arg2) {
		return arg1%arg2;
	}

	public static int pow(int arg1,int arg2) {
		return (int)java.lang.Math.pow(arg1,arg2);
	}
	public static long pow(long arg1,long arg2) {
		return (long)java.lang.Math.pow(arg1,arg2);
	}
	public static double pow(double arg1,double arg2) {
		return java.lang.Math.pow(arg1,arg2);
	}
	public static BigDecimal pow(BigDecimal arg1,BigDecimal arg2) {
		return arg1.pow(arg2.intValue()).setScale(18,RoundingMode.HALF_UP);
	}
	private static BigDecimal getBigDecimal(double arg2) {
		return new BigDecimal(arg2).setScale(18,RoundingMode.HALF_UP);
	}

	/** COMPUTE ... ROUNDED: round half away from zero to the decimals of the receiving item. */
	public static BigDecimal round(BigDecimal value, int scale) {
		return value.setScale(scale, RoundingMode.HALF_UP);
	}

	public static BigDecimal round(double value, int scale) {
		return BigDecimal.valueOf(value).setScale(scale, RoundingMode.HALF_UP);
	}

	public static long round(long value, int scale) {
		return value;
	}

	public static int round(int value, int scale) {
		return value;
	}
}
