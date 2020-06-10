package com.woodplc.cora.storage;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.FeatureView;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;
import com.woodplc.cora.utils.TestUtils;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class JSONUtilsTest {

	private final static Path exportPath = Paths.get(SoftwareSystem.TEST.path().toString(), "feature_export.json");
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
	void testFeatureClonesExport() throws IOException {
		FeatureView emptyView = TestUtils.emptyFeatureView();
		assertNotNull(emptyView);
		JSONUtils.exportFeatureToJson(exportPath, emptyView);
		
		FeatureView restoredEmptyView = JSONUtils.loadFeatureFromJson(exportPath);
		assertNotNull(restoredEmptyView);
		assertTrue(emptyView != restoredEmptyView);
		assertTrue(emptyView.equals(restoredEmptyView));
		
		FeatureView nonEmptyView = TestUtils.fullyInitializedFeatureView();
		assertNotNull(nonEmptyView);
		JSONUtils.exportFeatureToJson(exportPath, nonEmptyView);
		
		FeatureView restoredNonEmptyView = JSONUtils.loadFeatureFromJson(exportPath);
		assertNotNull(restoredNonEmptyView);
		assertTrue(nonEmptyView != restoredNonEmptyView);
		assertTrue(nonEmptyView.equals(restoredNonEmptyView));
	}

}
