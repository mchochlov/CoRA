package com.woodplc.cora.utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.stream.Stream;

import com.woodplc.cora.data.ApplicationState;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.ModuleContainer;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.gui.model.EntityView;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

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
		SDGraph graph = Graphs.newInstance();
		Objects.requireNonNull(graph);
		try (Stream<Path> stream = Files.walk(path)) 
		{
			stream
				.filter(Parsers::isFortranFile)
				.forEach(p -> graph.merge(parser.parse(p)));
			return graph;
	    }
	}

	public static ApplicationState emptyApplicationState() {
		return new ApplicationState(null, null, FXCollections.observableArrayList(), 
				ModuleContainer.empty(), ModuleContainer.empty(), ModuleContainer.empty(), 
				new Feature());
	}

	public static ApplicationState fullyInitializedApplicationState() {
		File lastKnownDir = new File(SoftwareSystem.TEST.path.toString());
		String searchQuery = "searchQuery";
		ObservableList<EntityView> searchResults = FXCollections.observableArrayList(
				new EntityView(1, "subprogram_1"),
				new EntityView(2, "subprogram_2"),
				new EntityView(3, "subprogram_3")
				);
		ModuleContainer mc1 = ModuleContainer.fromValues(SoftwareSystem.FLEX3.path.toString(), "checksum_1");
		ModuleContainer mc2 = ModuleContainer.fromValues(SoftwareSystem.DPRFLEX3.path.toString(), "checksum_2");
		ModuleContainer mc3 = ModuleContainer.fromValues(SoftwareSystem.MAM.path.toString(), "checksum_3");
		Feature feature = new Feature();
		feature.systemASubprograms().addAll("subprogram_a", "subprogram_b", "subprogram_c");
		feature.systemBSubprograms().addAll("subprogram_d", "subprogram_e");
		feature.systemCSubprograms().addAll("subprogram_f");
		return new ApplicationState(lastKnownDir, searchQuery, searchResults, 
				mc1, mc2, mc3, feature);
	}
}
