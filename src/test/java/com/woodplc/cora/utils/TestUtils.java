package com.woodplc.cora.utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

import com.woodplc.cora.data.ApplicationState;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.FeatureView;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.ModuleContainer;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.gui.model.SearchEntryView;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public final class TestUtils {

	public static enum SoftwareSystem {
		TEST_MOD_FL(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\ParserTest\\Lero\\modules_test\\fl")),
		TEST_MOD_DR(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\ParserTest\\Lero\\modules_test\\dr")),
		TEST_MOD_PL(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\ParserTest\\Lero\\modules_test\\pl")),
		TEST(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\ParserTest\\Lero\\all")),
		FLEX3(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\FlexcomAnalysis\\Flex3")),
		DPRFLEX3(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\DeepRiserAnalysis\\Dprflex3")),
		MAM(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\PipeLayAnalysis\\mam")), 
		CAF(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\CAF\\Preprocessing")),
		FLEXCOM(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\FlexcomAnalysis")),
		DEEPRISER(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\DeepRiserAnalysis")),
		PIPELAY(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\PipeLayAnalysis")), 
		CAF_ROOT(Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\CAF"));
		
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
		ObservableList<SearchEntryView> searchResults = FXCollections.observableArrayList(
				new SearchEntryView(1, 0.86f, "subprogram_1"),
				new SearchEntryView(2, 0.5f, "subprogram_2"),
				new SearchEntryView(3, 0.25f, "subprogram_3")
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

	public static FeatureView emptyFeatureView() {
		return new FeatureView(new HashSet<>(), new HashSet<>(), new HashSet<>());
	}

	public static FeatureView fullyInitializedFeatureView() {
		Set<SubProgram> systemA = new HashSet<>(Arrays.asList(
				SubProgram.ofType("subroutine", "module1", "subprogram_1", 1, 5, Paths.get("path_1")),
				SubProgram.ofType("program", "", "subprogram_2", 10, 100, Paths.get("path_2"))
			));
		Set<SubProgram> systemB = new HashSet<>(Arrays.asList(
				SubProgram.ofType("function", "module2", "subprogram_3", 6, 7, Paths.get("path_3"))
			));
		Set<SubProgram> systemC = new HashSet<>(Arrays.asList(
				SubProgram.ofType("subroutine", "module2", "subprogram_4", 10, 15, Paths.get("path_4")),
				SubProgram.ofType("function", "", "subprogram_5", 25, 30, Paths.get("path_5")),
				SubProgram.ofType("subroutine", "module_5", "subprogram_6", 25, 30, Paths.get("path_6"))
			));
		return new FeatureView(systemA, systemB, systemC);
	}
}
