package com.woodplc.cora.parser;

public final class Parsers {

	private static final String FORTRAN_FILE_EXTENSIONS = "*.{for,f90}";
	
	private Parsers() {}
	
	public static Parser fortranParser() {
		return new ANTLRFortranParser();
	}
	
	public static String fortranFileExtensions() {
		return FORTRAN_FILE_EXTENSIONS;
	}
	
}
