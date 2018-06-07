package com.woodplc.cora.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.stream.Stream;

import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;

public final class TestUtils {

	public static enum SoftwareSystem {
		TEST(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\ParserTest\\Lero\\all")),
		FLEX3(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\FlexcomAnalysis\\Flex3")),
		DPRFLEX3(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\DeepRiserAnalysis\\Dprflex3")),
		MAM(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\PipeLayAnalysis\\mam"));
		
		private final Path path;
		
		private SoftwareSystem(Path path) {
			this.path = path;
		}
		
		public Path path() {return path;}
	}
	
	public static SDGraph parseFiles(Parser parser, Path path) throws IOException {
		Objects.requireNonNull(parser);
		Objects.requireNonNull(path);
		SDGraph graph = Graphs.getSDGraphInstance();
		Objects.requireNonNull(graph);
		try (Stream<Path> stream = Files.walk(path)) 
		{
			stream
				.filter(Parsers::isFortranFile)
				.forEach(p -> graph.merge(parser.parse(p)));
			return graph;
	    }
	}
}
