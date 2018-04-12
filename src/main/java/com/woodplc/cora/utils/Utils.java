package com.woodplc.cora.utils;

public final class Utils {
	
	private Utils() {}
	
	public static enum RegEx{
		COMMA(","),
		WHITESPACE("\\s+");
		
		private final String regex;
		
		RegEx(String regex) {
			this.regex = regex;
		}
		
		public String regex() {return regex;}
	}
}
