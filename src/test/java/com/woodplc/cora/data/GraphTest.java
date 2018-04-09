package com.woodplc.cora.data;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;
import com.woodplc.cora.utils.Utils.RegEx;

class GraphTest {
	
	private static final String SUBPROGRAM_DEPENDENCIES_FILE = "subprogram_dependencies.txt";
	private static final int NUM_ENTRIES = 19;
	private static final int NUM_SUBPROGRAM_MEMBERS = 3;
	private static final String VARIABLE_CALLEES_FILE = "variable_callees.txt";
	private static final int NUM_VARIABLE_MEMBERS = 2;

	@Test
	void testSubProgramCalleesAndCallers() throws IOException, URISyntaxException {
		List<String> fileEntries = Files.readAllLines(Paths.get(getClass().getResource(SUBPROGRAM_DEPENDENCIES_FILE).toURI()));
		assertNotNull(fileEntries);
		assertEquals(NUM_ENTRIES, fileEntries.size());
		
		Parser parser = Parsers.fortranParser();
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST.path());
		assertNotNull(graph);
		
		for (String entry : fileEntries) {
			String[] entryMembers = entry.toLowerCase().split(RegEx.COMMA.regex(), NUM_SUBPROGRAM_MEMBERS);
			assertEquals(NUM_SUBPROGRAM_MEMBERS, entryMembers.length);
			String subName = entryMembers[0];
			assertTrue(!subName.isEmpty());
			Set<String> correctCallees = entryMembers[1].isEmpty() ? Collections.emptySet() 
					: new HashSet<>(Arrays.asList(entryMembers[1].split(RegEx.WHITESPACE.regex())));
			Set<String> correctCallers = entryMembers[2].isEmpty() ? Collections.emptySet()
					: new HashSet<>(Arrays.asList(entryMembers[2].split(RegEx.WHITESPACE.regex())));
			
			assertTrue(graph.containsSubprogram(subName));
			Set<String> testCallees = graph.getSubprogramCallees(subName);
			Set<String> testCallers = graph.getSubprogramCallers(subName);
			
			assertTrue(correctCallees.equals(testCallees));
			assertTrue(correctCallers.equals(testCallers));
		}
	}

	@Test
	void testVariableCallees() throws IOException, URISyntaxException {
		List<String> fileEntries = Files.readAllLines(Paths.get(getClass().getResource(VARIABLE_CALLEES_FILE).toURI()));
		assertNotNull(fileEntries);
		assertEquals(NUM_ENTRIES, fileEntries.size());
		
		Parser parser = Parsers.fortranParser();
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST.path());
		assertNotNull(graph);
		
		for (String entry : fileEntries) {
			String[] entryMembers = entry.toLowerCase().split(RegEx.COMMA.regex(), NUM_VARIABLE_MEMBERS);
			assertEquals(NUM_VARIABLE_MEMBERS, entryMembers.length);
			String varName = entryMembers[0];
			assertTrue(varName.length() > 0);
			Set<String> correctCallees = entryMembers[1].isEmpty() ? Collections.emptySet()
					: new HashSet<>(Arrays.asList(entryMembers[1].split(RegEx.WHITESPACE.regex())));
			
			if (correctCallees.size() < 1) {
				assertFalse(graph.containsVariable(varName));
			} else {
				assertTrue(graph.containsVariable(varName));
				Set<String> testCallees = graph.getVariableCallees(varName);
				assertTrue(correctCallees.equals(testCallees));
			}	
		}
	}
}
