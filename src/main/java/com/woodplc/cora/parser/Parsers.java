package com.woodplc.cora.parser;

import java.util.Objects;

import com.woodplc.cora.ir.IREngine;

public final class Parsers {

	private static final String FORTRAN_FILE_EXTENSIONS = "*.{for,f90}";
	
	private Parsers() {}
	
	public static Parser indexableFortranParser(IREngine engine) {
		return new ANTLRFortranParser(Objects.requireNonNull(engine));
	}
	
	public static String fortranFileExtensions() {
		return FORTRAN_FILE_EXTENSIONS;
	}
	
}
