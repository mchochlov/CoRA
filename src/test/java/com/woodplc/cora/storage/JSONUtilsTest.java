package com.woodplc.cora.storage;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.ApplicationState;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;
import com.woodplc.cora.utils.TestUtils;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class JSONUtilsTest {

	private final IREngine engine = mock(IREngine.class);
	
	@Test
	void testSDGraphLoadRestoreIntegrity() throws IOException {
		SDGraph emptyGraph = Graphs.newInstance();
		assertNotNull(emptyGraph);
		JSONUtils.graphToJson(SoftwareSystem.TEST.path(), emptyGraph);
		
		SDGraph restoredEmptyGraph = JSONUtils.graphFromJson(SoftwareSystem.TEST.path());
		assertNotNull(restoredEmptyGraph);
		assertTrue(emptyGraph != restoredEmptyGraph);
		assertTrue(emptyGraph.equals(restoredEmptyGraph));

		Parser parser = Parsers.indexableFortranParser(engine);
		assertNotNull(parser);
		SDGraph savedGraph = parseFiles(parser, SoftwareSystem.TEST.path());
		assertNotNull(savedGraph);
		JSONUtils.graphToJson(SoftwareSystem.TEST.path(), savedGraph);
		
		SDGraph restoredGraph = JSONUtils.graphFromJson(SoftwareSystem.TEST.path());
		assertNotNull(restoredGraph);
		assertTrue(savedGraph != restoredGraph);
		assertTrue(savedGraph.equals(restoredGraph));
	}
	
	@Test
	void testStateLoadRestoreIntegrity() throws IOException {
		ApplicationState emptyState = TestUtils.emptyApplicationState();
		assertNotNull(emptyState);
		JSONUtils.stateToJson(emptyState);
		
		ApplicationState restoredEmptyState = JSONUtils.stateFromJson();
		assertNotNull(restoredEmptyState);
		assertTrue(emptyState != restoredEmptyState);
		assertTrue(emptyState.equals(restoredEmptyState));
		
		ApplicationState nonEmptyState = TestUtils.fullyInitializedApplicationState();
		assertNotNull(nonEmptyState);
		JSONUtils.stateToJson(nonEmptyState);
		
		ApplicationState restoredNonEmptyState = JSONUtils.stateFromJson();
		assertNotNull(restoredNonEmptyState);
		assertTrue(nonEmptyState != restoredNonEmptyState);
		assertTrue(nonEmptyState.equals(restoredNonEmptyState));
	}

}
