package com.woodplc.cora.ir;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class IREngineTest {

	private static final Map<String, String> queryAndResult = new HashMap<>();
	
	static {
		queryAndResult.put("spectrum wave", "spectrum_or_wave_results.txt");
		queryAndResult.put("+spectrum +wave", "spectrum_and_wave_results.txt");
		queryAndResult.put("-spectrum wave", "not_spectrum_results.txt");
		queryAndResult.put("spectrum -wave", "not_wave_results.txt");
	}

	@Test
	void testIRResultsForConsistencyWriteable() throws IOException, URISyntaxException {
		IREngine engine = IREngines.newWriteableInstance(SoftwareSystem.TEST.path());
		assertNotNull(engine);
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST.path());
		assertNotNull(graph);
		
		engine.save();
		
		for (Map.Entry<String, String> entry : queryAndResult.entrySet()) {
			List<String> previousSearchResults = Files.readAllLines(Paths.get(getClass().getResource(entry.getValue()).toURI()));
			assertNotNull(previousSearchResults);
			assertTrue(previousSearchResults.equals(engine.search(entry.getKey())));
		}
		
		engine.close();
	}
	
	@Test
	void testIRResultsForConsistencyReadOnly() throws IOException, URISyntaxException {
		IREngine engine = IREngines.newReadOnlyInstance(SoftwareSystem.TEST.path());
		assertNotNull(engine);
		
		assertThrows(UnsupportedOperationException.class, () -> {
			Parser parser = Parsers.indexableFortranParser(engine);
			parseFiles(parser, SoftwareSystem.TEST.path());
        });
		
		assertThrows(UnsupportedOperationException.class, () -> {
			engine.save();
		});
		
		for (Map.Entry<String, String> entry : queryAndResult.entrySet()) {
			List<String> previousSearchResults = Files.readAllLines(Paths.get(getClass().getResource(entry.getValue()).toURI()));
			assertNotNull(previousSearchResults);
			assertTrue(previousSearchResults.equals(engine.search(entry.getKey())));
		}
		
		engine.close();
	}

}
