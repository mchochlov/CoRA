package com.woodplc.cora.parser;

import java.nio.file.Path;

import com.woodplc.cora.data.FortranFileModel;

interface Parser {
	
	FortranFileModel parse(Path path);

}
