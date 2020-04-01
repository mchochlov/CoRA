package com.woodplc.cora.parser;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import com.google.common.io.MoreFiles;
import com.woodplc.cora.ir.IREngine;

public final class Parsers {

	private static final Set<String> FORTRAN_FILE_EXTENSIONS = new HashSet<>(
				Arrays.asList("for", "f90")
			);
	
	private Parsers() {}
	
	public static Parser indexableFortranParser(IREngine engine) {
		return new ANTLRFortranParser(Objects.requireNonNull(engine));
	}
		
	public static boolean isFortranFile(Path path) {
		return FORTRAN_FILE_EXTENSIONS.contains(MoreFiles.getFileExtension(path).toLowerCase());
	}

	public static Parser nonIndexableFortranParser() {
		return new ANTLRFortranParser();
	}
	
}
