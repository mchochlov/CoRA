package com.woodplc.cora.parser;

import java.nio.file.Path;

import com.woodplc.cora.data.FortranFileModel;

public interface Parser {
	
	FortranFileModel parse(Path path);

}
