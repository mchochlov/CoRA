package com.woodplc.cora.trees;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import com.github.gumtreediff.matchers.MappingStore;
import com.github.gumtreediff.matchers.Matcher;
import com.github.gumtreediff.matchers.Matchers;
import com.github.gumtreediff.matchers.heuristic.gt.GreedyBottomUpMatcher;
import com.github.gumtreediff.matchers.heuristic.gt.GreedySubtreeMatcher;
import com.github.gumtreediff.tree.ITree;
import com.woodplc.cora.utils.Utils;

class TreeComparisonTest {
	
	private static final String SUB_A = "set_reynolds_flex3.f90";
	private static final String SUB_B = "set_reynolds_dprflex3.f90";
	private static final String SUB_C = "set_reynolds_mam.for";
	private static final String AB_MAPPING = "ab_mapping.txt";
	private static final String AC_MAPPING = "ac_mapping.txt";
	private static final String BC_MAPPING = "bc_mapping.txt";

	@Test
	void testFortranTreeGenerator() throws IOException, URISyntaxException {
		Path subA = Paths.get(getClass().getResource(SUB_A).toURI());
		Path subB = Paths.get(getClass().getResource(SUB_B).toURI());
		Path subC = Paths.get(getClass().getResource(SUB_C).toURI());
		String originalA = Utils.readFullLinesFromFile(subA).stream().map(String::toLowerCase).collect(Collectors.joining());
		String originalB = Utils.readFullLinesFromFile(subB).stream().map(String::toLowerCase).collect(Collectors.joining());
		String originalC = Utils.readFullLinesFromFile(subC).stream().map(String::toLowerCase).collect(Collectors.joining());
		ITree treeA = new FortranTreeGenerator(subA).generateFrom().string(originalA).getRoot();
		ITree treeB = new FortranTreeGenerator(subB).generateFrom().string(originalB).getRoot();
		ITree treeC = new FortranTreeGenerator(subC).generateFrom().string(originalC).getRoot();
		
		Matcher m = Matchers.getInstance().getMatcher(); // retrieve the default matcher
		MappingStore ms = m.match(treeA, treeB); // return the mapping store
		
		List<String> mappingAB = Files.readAllLines(Paths.get(getClass().getResource(AB_MAPPING).toURI()));
		assertTrue(mappingAB.equals(computeDiff(ms, treeA, treeB)));

		ms = m.match(treeA, treeC);
		List<String> mappingAC = Files.readAllLines(Paths.get(getClass().getResource(AC_MAPPING).toURI()));
		assertTrue(mappingAC.equals(computeDiff(ms, treeA, treeC)));
		
		ms = m.match(treeB, treeC);
		List<String> mappingBC = Files.readAllLines(Paths.get(getClass().getResource(BC_MAPPING).toURI()));
		assertTrue(mappingBC.equals(computeDiff(ms, treeB, treeC)));
	}
	
	private List<String> computeDiff(MappingStore ms, ITree src, ITree dst) {
		int allMapped = 0;
		int allUnMappedSrc = 0;
		int allUnMappedDst = 0;
		List<String> diff = new ArrayList<>();
		for (ITree tree : src.preOrder()) {
			if (!ms.isSrcMapped(tree)) {// && tree.getType().name.equals("REAL")) {
				if (tree.isLeaf()) {
					diff.add("UNMAPPED " + tree.getType().name + "<<" + tree.getLabel().replaceAll("(\\r|\\n|\\r\\n)+", "\\\\n") + ">>" + tree.getPos() + " " + tree.getEndPos());
				} else {
					diff.add("UNMAPPED " + tree.getType().name + " " + tree.getPos() + " " + tree.getEndPos());					
				}
				diff.add("==========================");
				allUnMappedSrc++;
			} else if (ms.isSrcMapped(tree)) {// && tree.getType().name.equals("REAL")) {
				ITree mapped = ms.getDstForSrc(tree);
				if (tree.isLeaf() && !tree.hasSameTypeAndLabel(mapped)) {
					diff.add("MAPPED " + tree.getType().name + "<<" + tree.getLabel().replaceAll("(\\r|\\n|\\r\\n)+", "\\\\n") + ">>" + tree.getPos() + " " + tree.getEndPos());
					diff.add("MAPPED " + mapped.getType().name + "<<" + mapped.getLabel().replaceAll("(\\r|\\n|\\r\\n)+", "\\\\n") + ">>" + mapped.getPos() + " " + mapped.getEndPos());
					diff.add("==========================");					
				}
				allMapped++;
			} else {
				throw new IllegalStateException();
			}
		}
		
		for (ITree tree : dst.preOrder()) {
			if (!ms.isDstMapped(tree)) {// && tree.getType().name.equals("REAL")) {
				if (tree.isLeaf()) {
					diff.add("UNMAPPED " + tree.getType().name + "<<" + tree.getLabel().replaceAll("(\\r|\\n|\\r\\n)+", "\\\\n") + ">>" + tree.getPos() + " " + tree.getEndPos());
				} else {
					diff.add("UNMAPPED " + tree.getType().name + " " + tree.getPos() + " " + tree.getEndPos());					
				}
				diff.add("==========================");
				allUnMappedDst++;
			} else if (ms.isDstMapped(tree)) {
			
			} else {
				throw new IllegalStateException();
			}
		}
		
		diff.add("Total mapped : " + allMapped);
		diff.add("Total unmapped src : " + allUnMappedSrc);
		diff.add("Total unmapped dst : " + allUnMappedDst);
		
		return diff;
	}
	
}
