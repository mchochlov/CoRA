package com.woodplc.cora.data;

import java.nio.file.Path;

public final class Program extends SubProgram {

	public Program(String module, String subname, int startLine, int endLine, Path path) {
		super(module, subname, startLine, endLine, path);
	}

}
