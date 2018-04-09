package com.woodplc.cora.parser;

import java.nio.file.Path;

import com.woodplc.cora.data.SDGraph;

public interface Parser {
	
	SDGraph parse(Path path);

}
