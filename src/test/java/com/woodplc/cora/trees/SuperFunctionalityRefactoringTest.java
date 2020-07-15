package com.woodplc.cora.trees;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.OrderedLinkedMap;
import com.woodplc.cora.trees.algorithms.CloneMergeAlgorithm;
import com.woodplc.cora.trees.algorithms.MergeAlgorithms;
import com.woodplc.cora.utils.Utils;

class SuperFunctionalityRefactoringTest {
	
	private static final String SUB_A = "set_reynolds_flex3.f90";
	private static final String SUB_B = "set_reynolds_dprflex3.f90";
	private static final String SUB_C = "set_reynolds_mam.for";
	private static final String SUB_MERGED_SF = "set_reynolds_merged_sf.txt";
	
	private static final String SUB_A1 = "calculate_reynolds_flex3.f90";
	private static final String SUB_B1 = "calculate_reynolds_dprflex3.f90";
	private static final String SUB_C1 = "calculate_reynolds_mam.for";
	private static final String SUB_MERGED_SF1 = "calculate_reynolds_merged_sf.txt";

	@Test
	void testMergeWithSFRefactoringReynolds() throws URISyntaxException, IOException {
		Path subA = Paths.get(getClass().getResource(SUB_A).toURI());
		Path subB = Paths.get(getClass().getResource(SUB_B).toURI());
		Path subC = Paths.get(getClass().getResource(SUB_C).toURI());
		Stream<String> originalSubprogramA = Utils.readFullLinesFromFile(subA).stream();
		Stream<String> originalSubprogramB = Utils.readFullLinesFromFile(subB).stream();
		Stream<String> originalSubprogramC = Utils.readFullLinesFromFile(subC).stream();
		OrderedLinkedMap<Path, Stream<String>> subs = new OrderedLinkedMap<>();
		subs.put(subA, originalSubprogramA);
		subs.put(subB, originalSubprogramB);
		subs.put(subC, originalSubprogramC);
		
		CloneMergeAlgorithm cma = MergeAlgorithms.configure()
				.withSuperFunctionalityRefactoring()
				.tokenSequenceMerge(subs);
		List<String> mergedSubprogram = cma.merge();
		//mergedSubprogram.forEach(System.out::println);
		List<String> correctMerged = Files.readAllLines(Paths.get(getClass().getResource(SUB_MERGED_SF).toURI()));
		assertTrue(correctMerged.equals(mergedSubprogram));
	}
	
	@Test
	void testMergeWithSFRefactoringCalcReyn() throws URISyntaxException, IOException {
		Path subA = Paths.get(getClass().getResource(SUB_A1).toURI());
		Path subB = Paths.get(getClass().getResource(SUB_B1).toURI());
		Path subC = Paths.get(getClass().getResource(SUB_C1).toURI());
		Stream<String> originalSubprogramA = Utils.readFullLinesFromFile(subA).stream();
		Stream<String> originalSubprogramB = Utils.readFullLinesFromFile(subB).stream();
		Stream<String> originalSubprogramC = Utils.readFullLinesFromFile(subC).stream();
		OrderedLinkedMap<Path, Stream<String>> subs = new OrderedLinkedMap<>();
		subs.put(subA, originalSubprogramA);
		subs.put(subB, originalSubprogramB);
		subs.put(subC, originalSubprogramC);
		
		//cma = MergeAlgorithms.defaultTokenSequenceMergeAlgorithm(subs);
		CloneMergeAlgorithm cma = MergeAlgorithms.configure()
				.withSuperFunctionalityRefactoring()
				.tokenSequenceMerge(subs);
		List<String> mergedSubprogram = cma.merge();
		//mergedSubprogram.forEach(System.out::println);
		List<String> correctMerged = Files.readAllLines(Paths.get(getClass().getResource(SUB_MERGED_SF1).toURI()));
		assertTrue(correctMerged.equals(mergedSubprogram));
	}
}
