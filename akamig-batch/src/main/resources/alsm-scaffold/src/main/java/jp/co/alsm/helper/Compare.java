package jp.co.alsm.helper;

import java.math.BigDecimal;

import static jp.co.alsm.helper.StringHelper.isNul;

public class Compare {

	public static final boolean equals(String o1, String o2) {
		return o1.compareTo(o2) == 0;
	}

	public static final boolean notEquals(String o1, String o2) {
		return o1.compareTo(o2) != 0;
	}

	public static final boolean lessThan(String o1, String o2) {
		return !isNul(o1) && (o1.compareTo(o2) < 0 || isNul(o2));
	}

	public static final boolean lessThanOrEqual(String o1, String o2) {
		return o1.compareTo(o2) <= 0 || isNul(o2);
	}

	public static final boolean greaterThan(String o1, String o2) {
		return (o1.compareTo(o2) > 0 || isNul(o1)) && !isNul(o2);
	}

	public static final boolean greaterThanOrEqual(String o1, String o2) {
		return o1.compareTo(o2) >= 0 || isNul(o1);
	}

	public static final boolean equals(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) == 0;
		} else {
			return Long.parseLong(o1) == Long.parseLong(o2);
		}
	}

	public static final boolean notEquals(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) != 0;
		} else {
			return Long.parseLong(o1) != Long.parseLong(o2);
		}
	}

	public static final boolean lessThan(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) < 0;
		} else {
			return Long.parseLong(o1) < Long.parseLong(o2);
		}
	}

	public static final boolean lessThanOrEqual(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) <= 0;
		} else {
			return Long.parseLong(o1) <= Long.parseLong(o2);
		}
	}

	public static final boolean greaterThan(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) > 0;
		} else {
			return Long.parseLong(o1) > Long.parseLong(o2);
		}
	}

	public static final boolean greaterThanOrEqual(String o1, String o2, boolean isDecimal) {
		if (isDecimal) {
			return FieldFormat.parseNumber(o1).compareTo(FieldFormat.parseNumber(o2)) >= 0;
		} else {
			return Long.parseLong(o1) >= Long.parseLong(o2);
		}
	}


	// Class tests (IF x IS NUMERIC / ALPHABETIC ...) on the text of an item.
	/*
	 * Any pair the typed overloads below do not cover (a String accessor against an int, a char
	 * against a long, ...). Two numbers, or a number and a numeric text, compare by value;
	 * anything else compares as text, the shorter operand padded with spaces as COBOL does.
	 */
	public static final boolean eq(Object o1, Object o2) { return compareAny(o1, o2) == 0; }
	public static final boolean ne(Object o1, Object o2) { return compareAny(o1, o2) != 0; }
	public static final boolean lt(Object o1, Object o2) { return compareAny(o1, o2) < 0; }
	public static final boolean le(Object o1, Object o2) { return compareAny(o1, o2) <= 0; }
	public static final boolean gt(Object o1, Object o2) { return compareAny(o1, o2) > 0; }
	public static final boolean ge(Object o1, Object o2) { return compareAny(o1, o2) >= 0; }

	public static final int compareAny(Object o1, Object o2) {
		if (o1 instanceof byte[]) o1 = new String((byte[]) o1);
		if (o2 instanceof byte[]) o2 = new String((byte[]) o2);
		if (o1 instanceof Character) o1 = String.valueOf(o1);
		if (o2 instanceof Character) o2 = String.valueOf(o2);
		if (o1 instanceof Number || o2 instanceof Number) {
			BigDecimal b1 = numberOf(o1), b2 = numberOf(o2);
			if (b1 != null && b2 != null)
				return b1.compareTo(b2);
		}
		return compareText(o1 == null ? "" : o1.toString(), o2 == null ? "" : o2.toString());
	}

	/*
	 * COBOL comparison of two texts: byte by byte in the program's encoding (so HIGH-VALUES is
	 * above every character, half-width katakana included), the shorter one padded with spaces.
	 * HIGH-VALUES / LOW-VALUES from CobolFunction (256 times one character) count at the length
	 * of the other operand, as a figurative constant does.
	 */
	public static final int compareText(String o1, String o2) {
		o1 = figurative(o1 == null ? "" : o1, o2 == null ? 0 : o2.length());
		o2 = figurative(o2 == null ? "" : o2, o1.length());
		byte[] b1, b2;
		try {
			b1 = jp.co.alsm.common.Record.bytes(o1);
			b2 = jp.co.alsm.common.Record.bytes(o2);
		} catch (java.io.UnsupportedEncodingException e) {
			b1 = o1.getBytes();
			b2 = o2.getBytes();
		}
		int n = Math.max(b1.length, b2.length);
		for (int i = 0; i < n; i++) {
			int c1 = i < b1.length ? b1[i] & 0xFF : ' ';
			int c2 = i < b2.length ? b2[i] & 0xFF : ' ';
			if (c1 != c2)
				return c1 < c2 ? -1 : 1;
		}
		return 0;
	}

	private static String figurative(String s, int length) {
		if (s.length() == 256 && length < 256 && (s.charAt(0) == '\u00FF' || s.charAt(0) == '\u0000')
				&& s.charAt(255) == s.charAt(0))
			return s.substring(0, length);
		return s;
	}

	private static BigDecimal numberOf(Object o) {
		if (o instanceof BigDecimal) return (BigDecimal) o;
		if (o instanceof Number) return new BigDecimal(o.toString());
		String text = o == null ? "" : o.toString().trim();
		if (text.isEmpty()) return null;
		try {
			return new BigDecimal(text);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	public static final boolean isNumeric(String value) {
		String text = value.trim();
		if (text.isEmpty()) {
			return false;
		}
		for (int i = 0; i < text.length(); i++) {
			char c = text.charAt(i);
			if (c < '0' || c > '9') {
				return false;
			}
		}
		return true;
	}

	public static final boolean isAlphabetic(String value) {
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (c != ' ' && !Character.isLetter(c)) {
				return false;
			}
		}
		return true;
	}

	public static final boolean isAlphabeticLower(String value) {
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (c != ' ' && !(Character.isLetter(c) && Character.isLowerCase(c))) {
				return false;
			}
		}
		return true;
	}

	public static final boolean isAlphabeticUpper(String value) {
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (c != ' ' && !(Character.isLetter(c) && Character.isUpperCase(c))) {
				return false;
			}
		}
		return true;
	}

	// Comparison and sign helpers the AKAMIG converter calls.


	public static final boolean eq(int o1,int o2) {
		return o1==o2;
	}
	public static final boolean eq(long o1,long o2) {
		return o1==o2;
	}
	public static final boolean eq(double o1,double o2) {
		return Math.abs(o1-o2) <.000005;
	}
	public static final boolean eq(double o1,BigDecimal o2) {
		return Math.abs(o2.doubleValue()-o1) <.000005;
	}
	public static final boolean eq(BigDecimal o1,double o2) {
		return Math.abs(o1.doubleValue()-o2) <.000005;
	}
	public static final boolean eq(BigDecimal o1,BigDecimal o2) {
		return o1.compareTo(o2)==0;
	}
	public static final boolean eq(BigDecimal o1,String o2) {
		return eq(FieldFormat.parseNumber(o2),o1);
	}
	public static final boolean eq(String o1,BigDecimal o2) {
		return eq(FieldFormat.parseNumber(o1),o2);
	}
   public static final boolean eq(byte[] o1,BigDecimal o2) {
        return eq(FieldFormat.parseNumber(new String(o1)),o2);
    }

    public static final boolean eq(BigDecimal o1,byte[] o2) {
        return eq(o1,FieldFormat.parseNumber(new String(o2)));
    }

    public static final boolean eq(char o1,String o2) {
		return o2.charAt(0)==o1&&o2.trim().length()<=1;
	}
	
    public static final boolean eq(String o1,char o2) {
		return o1.charAt(0)==o2&&o1.trim().length()<=1;
	}
	  
   public static final boolean eq(String o1,String o2) {
		return compareText(o1, o2)==0;
	}
	
	public static final boolean eq(String o1,byte[] o2) {
        return new String(o2).compareTo(o1) == 0;
    }

    public static final boolean eq(byte[] o1,String o2) {
         return eq(new String(o1),o2);
    }
 
    public static final boolean eq(String o1,long o2) {
		return compareAny(o1, o2) == 0;
	}
    public static final boolean eq(long o1,String o2) {
		return compareAny(o1, o2) == 0;
	}
 
    public static final boolean eq(long o1,byte[] o2) {
        return eq(new String(o2),String.valueOf(o1));
    }
    public static final boolean eq(byte[] o1,long o2) {
        return eq(new String(o1),String.valueOf(o2));
    }
    public static final boolean eq(byte[] o1,byte[] o2) {
    	return eq(new String(o1),new String(o2));
    }

		public static final boolean ne(int o1,int o2) {
			return o1!=o2;
		}
		public static final boolean ne(long o1,long o2) {
			return o1!=o2;
		}
		public static final boolean ne(double o1,double o2) {
			return Math.abs(o1-o2)>.000005;
		}
		public static final boolean ne(double o1,BigDecimal o2) {
			return Math.abs(o2.doubleValue()-o1) >.000005;
		}
		public static final boolean ne(BigDecimal o1,double o2) {
			return Math.abs(o1.doubleValue()-o2) >.000005;
		}
		public static final boolean ne(BigDecimal o1,BigDecimal o2) {
			return o1.compareTo(o2)!=0;
		}
		public static final boolean ne(BigDecimal o1,String o2) {
			return ne(FieldFormat.parseNumber(o2),o1);
		}
		public static final boolean ne(String o1,BigDecimal o2) {
			return ne(FieldFormat.parseNumber(o1),o2);
		}
		
		public static final boolean ne(byte[] o1,BigDecimal o2) {
	        return ne(FieldFormat.parseNumber(new String(o1)),o2);
	    }
		
		public static final boolean ne(byte[] o1,long o2) {
	        // DaoNH MOD Start: Replace deprecated code
			//return ne(new Long(new String(o1)),o2);
			return ne(Long.parseLong(String.valueOf(o1)),o2);
			// DaoNH MOD End: Replace deprecated code
	    }

	    public static final boolean ne(BigDecimal o1,byte[] o2) {
	        return ne(o1,FieldFormat.parseNumber(new String(o2)));
	    }
	    public static final boolean ne(char o1,String o2) {
			return o2.charAt(0)!=o1||o2.trim().length()!=1;
		}
		
	    public static final boolean ne(String o1,char o2) {
			return o1.charAt(0)!=o2||o1.length()!=1;
		}
		  
	   public static final boolean ne(String o1,String o2) {
			return compareText(o1, o2)!=0;
		}
		
		public static final boolean ne(String o1,byte[] o2) {
	        return new String(o2).compareTo(o1) != 0;
	    }

	    public static final boolean ne(byte[] o1,String o2) {
	         return ne(new String(o1),o2);
	    }
	    
	    public static final boolean ne(String o1,long o2) {
		return compareAny(o1, o2) != 0;
	}
	    
	    public static final boolean ne(long o1,String o2) {
		return compareAny(o1, o2) != 0;
	}
	 
	    public static final boolean ne(long o1,byte[] o2) {
	    	return ne(String.valueOf(o1),new String(o2).trim());
	    	    }
	    public static final boolean ne(byte[] o1,byte[] o2) {
	    	return new String(o1).trim().compareTo(new String(o2).trim()) != 0;
	    }

		public static final boolean lt(int o1,int o2) {
			return o1<o2;
		}
		public static final boolean lt(long o1,long o2) {
			return o1<o2;
		}
		public static final boolean lt(double o1,double o2) {
			return o2-o1 > .000005;
		}
		public static final boolean lt(double o1,BigDecimal o2) {
			return o2.doubleValue()-o1 >.000005;
		}
		public static final boolean lt(BigDecimal o1,double o2) {
			return o2-o1.doubleValue() >.000005;
		}
		public static final boolean lt(BigDecimal o1,BigDecimal o2) {
			return o1.compareTo(o2)<0;
		}
		public static final boolean lt(BigDecimal o1,String o2) {
			return lt(FieldFormat.parseNumber(o2),o1);
		}
		public static final boolean lt(String o1,BigDecimal o2) {
			return lt(FieldFormat.parseNumber(o1),o2);
		}
		
	   public static final boolean lt(byte[] o1,BigDecimal o2) {
	        return lt(FieldFormat.parseNumber(new String(o1)),o2);
	    }

	    public static final boolean lt(BigDecimal o1,byte[] o2) {
	        return lt(o1,FieldFormat.parseNumber(new String(o2)));
	    }
	    public static final boolean lt(char o1,String o2) {
			return o1<o2.charAt(0);
		}
		
	    public static final boolean lt(String o1,char o2) {
			return o1.charAt(0)<o2||o1.length()<=1;
		}		
	    public static final boolean lt(byte[] o1,long o2) {
	        // DaoNH MOD Start: Replace deprecated code
			//return lt(new Long(new String(o1)),o2);
			return lt(Long.parseLong(String.valueOf(o1)),o2);
			// DaoNH MOD End: Replace deprecated code
	    }
		  
	   public static final boolean lt(String o1,String o2) {
			return compareText(o1, o2)<0;
		}
		
		public static final boolean lt(String o1,byte[] o2) {
	        return new String(o2).compareTo(o1) > 0;
	    }

	    public static final boolean lt(byte[] o1,String o2) {
	         return lt(new String(o1),o2);
	    }
	    
	    public static final boolean lt(String o1,long o2) {
		return compareAny(o1, o2) < 0;
	}
	    
	    public static final boolean lt(long o1,String o2) {
		return compareAny(o1, o2) < 0;
	}
	 
	    public static final boolean lt(long o1,byte[] o2) {
	    	return lt(String.valueOf(o1),new String(o2).trim());
	    }
	    public static final boolean lt(byte[] o1,byte[] o2) {
	    	return new String(o1).compareTo(new String(o2))<0;
	    }
	    
		public static final boolean le(int o1,int o2) {
			return o1<=o2;
		}
		public static final boolean le(long o1,long o2) {
			return o1<=o2;
		}
		public static final boolean le(double o1,double o2) {
			return o2-o1>=.000005;
		}
		public static final boolean le(double o1,BigDecimal o2) {
			return o2.doubleValue()-o1 >=.000005;
		}
		public static final boolean le(BigDecimal o1,double o2) {
			return o2-o1.doubleValue() >=.000005;
		}
		public static final boolean le(BigDecimal o1,BigDecimal o2) {
			return o1.compareTo(o2)<=0;
		}
		public static final boolean le(BigDecimal o1,String o2) {
			return le(FieldFormat.parseNumber(o2),o1);
		}
		public static final boolean le(String o1,BigDecimal o2) {
			return le(FieldFormat.parseNumber(o1),o2);
		}
		
	   public static final boolean le(byte[] o1,BigDecimal o2) {
	        return le(FieldFormat.parseNumber(new String(o1)),o2);
	    }

	    public static final boolean le(BigDecimal o1,byte[] o2) {
	        return le(o1,FieldFormat.parseNumber(new String(o2)));
	    }
	    public static final boolean le(char o1,String o2) {
			return o1<=o2.charAt(0);
		}
		
	    public static final boolean le(String o1,char o2) {
			return o1.charAt(0)<=o2||o1.length()<=1;
		}		
		public static final boolean le(byte[] o1,long o2) {
	        // DaoNH MOD Start: Replace deprecated code
			//return le(new Long(new String(o1)),o2);
			return le(Long.parseLong(String.valueOf(o1)),o2);
			// DaoNH MOD End: Replace deprecated code
	    }

	   public static final boolean le(String o1,String o2) {
			return compareText(o1, o2)<=0;
		}
		
		public static final boolean le(String o1,byte[] o2) {
	        return new String(o2).compareTo(o1) >= 0;
	    }

	    public static final boolean le(byte[] o1,String o2) {
	         return le(new String(o1),o2);
	    }
	    
	    public static final boolean le(String o1,long o2) {
		return compareAny(o1, o2) <= 0;
	}
	    
	    public static final boolean le(long o1,String o2) {
		return compareAny(o1, o2) <= 0;
	}
	 
	    public static final boolean le(long o1,byte[] o2) {
	    	return le(String.valueOf(o1),new String(o2).trim());
	    }
	    public static final boolean le(byte[] o1,byte[] o2) {
	    	return new String(o1).trim().compareTo(new String(o2).trim()) <= 0;
	    }

		public static final boolean gt(char o1,long o2) {
			return o1-'0'==o2;
		}
		public static final boolean gt(long o1,char o2) {
			return o1==o2-'0';
		}
		public static final boolean gt(char o1,double o2) {
			return o1-'0'==o2;
		}
		public static final boolean gt(double o1,char o2) {
			return o1>o2-'0';
		}
		
	    
		public static final boolean gt(int o1,int o2) {
			return o1>o2;
		}
		public static final boolean gt(long o1,long o2) {
			return o1>o2;
		}
		public static final boolean gt(double o1,double o2) {
			return o1-o2 > .000005;
		}
		public static final boolean gt(double o1,BigDecimal o2) {
			return o1-o2.doubleValue() >.000005;
		}
		public static final boolean gt(BigDecimal o1,double o2) {
			return o1.doubleValue()-o2 >.000005;
		}
		public static final boolean gt(BigDecimal o1,BigDecimal o2) {
			return o1.compareTo(o2)>0;
		}
		public static final boolean gt(BigDecimal o1,String o2) {
			return gt(FieldFormat.parseNumber(o2),o1);
		}
		public static final boolean gt(String o1,BigDecimal o2) {
			return gt(FieldFormat.parseNumber(o1),o2);
		}
		
	   public static final boolean gt(byte[] o1,BigDecimal o2) {
	        return gt(FieldFormat.parseNumber(new String(o1)),o2);
	    }

	    public static final boolean gt(BigDecimal o1,byte[] o2) {
	        return gt(o1,FieldFormat.parseNumber(new String(o2)));
	    }
	    public static final boolean gt(char o1,String o2) {
			return o1>o2.charAt(0);
		}
		
	    public static final boolean gt(String o1,char o2) {
			return o1.charAt(0)>o2;
		}		
		public static final boolean gt(byte[] o1,long o2) {
	        // DaoNH MOD Start: Replace deprecated code
			//return gt(new Long(new String(o1)),o2);
			return gt(Long.parseLong(String.valueOf(o1)),o2);
			// DaoNH MOD End: Replace deprecated code
	    }

	   public static final boolean gt(String o1,String o2) {
			return compareText(o1, o2)>0;
		}
		
		public static final boolean gt(String o1,byte[] o2) {
	        return new String(o2).compareTo(o1) < 0;
	    }

	    public static final boolean gt(byte[] o1,String o2) {
	         return gt(new String(o1),o2);
	    }
	    
	    public static final boolean gt(String o1,long o2) {
		return compareAny(o1, o2) > 0;
	}
	    
	    public static final boolean gt(long o1,String o2) {
		return compareAny(o1, o2) > 0;
	}
	 
	    public static final boolean gt(long o1,byte[] o2) {
	    	return gt(String.valueOf(o1),new String(o2).trim());
	    }
	    public static final boolean gt(byte[] o1,byte[] o2) {
	    	return new String(o1).trim().compareTo(new String(o2).trim()) > 0;
	    }
	    
		public static final boolean ge(char o1,char o2) {
			return o1==o2;
		}
		public static final boolean ge(char o1,long o2) {
			return o1-'0'==o2;
		}
		public static final boolean ge(long o1,char o2) {
			return o1==o2-'0';
		}
		public static final boolean ge(char o1,double o2) {
			return o1-'0'==o2;
		}
		public static final boolean ge(double o1,char o2) {
			return o1>=o2-'0';
		}
		
	    
		public static final boolean ge(int o1,int o2) {
			return o1>=o2;
		}
		public static final boolean ge(long o1,long o2) {
			return o1>=o2;
		}
		public static final boolean ge(double o1,double o2) {
			return o1-o2>=.000005;
		}
		public static final boolean ge(double o1,BigDecimal o2) {
			return o1-o2.doubleValue() >=.000005;
		}
		public static final boolean ge(BigDecimal o1,double o2) {
			return o1.doubleValue()-o2 >=.000005;
		}
		public static final boolean ge(BigDecimal o1,BigDecimal o2) {
			return o1.compareTo(o2)>=0;
		}
		public static final boolean ge(BigDecimal o1,String o2) {
			return ge(FieldFormat.parseNumber(o2),o1);
		}
		public static final boolean ge(String o1,BigDecimal o2) {
			return ge(FieldFormat.parseNumber(o1),o2);
		}
		public static final boolean ge(byte[] o1,BigDecimal o2) {
	        return ge(FieldFormat.parseNumber(new String(o1)),o2);
	    }
	    public static final boolean ge(BigDecimal o1,byte[] o2) {
	        return ge(o1,FieldFormat.parseNumber(new String(o2)));
	    }
	    public static final boolean ge(char o1,String o2) {
			return o1>=o2.charAt(0);
		}
		
	    public static final boolean ge(String o1,char o2) {
			return o1.charAt(0)>=o2&&o1.length()>=1;
		}		
	    
		public static final boolean ge(byte[] o1,long o2) {
	        // DaoNH MOD Start: Replace deprecated code
			//return ge(new Long(new String(o1)),o2);
			return ge(Long.parseLong(String.valueOf(o1)),o2);
			// DaoNH MOD End: Replace deprecated code
	    }

	    public static final boolean ge(String o1,String o2) {
			return compareText(o1, o2)>=0;
		}
		public static final boolean ge(String o1,byte[] o2) {
	        return new String(o2).compareTo(o1) <= 0;
	    }

	    public static final boolean ge(byte[] o1,String o2) {
	         return ge(new String(o1),o2);
	    }
	    public static final boolean ge(String o1,long o2) {
		return compareAny(o1, o2) >= 0;
	}
	    public static final boolean ge(long o1,String o2) {
		return compareAny(o1, o2) >= 0;
	}
	    public static final boolean ge(long o1,byte[] o2) {
	    	return ge(String.valueOf(o1),new String(o2).trim());
	    }
	    public static final boolean ge(byte[] o1,byte[] o2) {
	    	return new String(o1).compareTo(new String(o2)) >= 0;
	    }

	    public static final boolean isPositive(int arg) {
	    	return arg>0;
	    }
	    public static final boolean isPositive(long arg) {
	    	return arg>0;
	    }
	    public static final boolean isPositive(double arg) {
	    	return arg>0;
	    }
	    public static final boolean isPositive(BigDecimal arg) {
	    	return arg.compareTo(BigDecimal.ZERO)>0;
	    }
	    
	    public static final boolean isNegative(int arg) {
	    	return arg<0;
	    }
	    public static final boolean isNegative(long arg) {
	    	return arg<0;
	    }
	    public static final boolean isNegative(double arg) {
	    	return arg<0;
	    }
	    public static final boolean isNegative(BigDecimal arg) {
	    	return arg.compareTo(BigDecimal.ZERO)<0;
	    }

	    public static final boolean isZero(int arg) {
	    	return arg==0;
	    }
	    public static final boolean isZero(long arg) {
	    	return arg==0;
	    }
	    public static final boolean isZero(double arg) {
	    	return arg==0;
	    }
	    public static final boolean isZero(BigDecimal arg) {
	    	return arg.compareTo(BigDecimal.ZERO)==0;
	    }
	    
	    public static final int spaces(char arg) {
    		if(arg<' ')
    			return -1;
    		else 
    			if(arg>' ')
    				return 1;
	    	return 0;
	    }
	    

	    public static final int spaces(double arg) {
    			return -1;
	    }
	    
	    public static final int spaces(BigDecimal arg) {
			return -1;
	    }
    
	    public static final int spaces(byte[] arg) {
	    	for(int i=0;i<arg.length;++i)
	    		if(arg[i]<' ')
	    			return -1;
	    		else 
	    			if(arg[i]>' ')
	    				return 1;
	    	return 0;
	    }
	    


	    public static final int spaces(String arg) {
	    	for(int i=0;i<arg.length();++i)
	    		if(arg.charAt(i)<' ')
	    			return -1;
	    		else 
	    			if(arg.charAt(i)>' ')
	    				return 1;
	    	return 0;
	    }
	    
	    
	    
}
