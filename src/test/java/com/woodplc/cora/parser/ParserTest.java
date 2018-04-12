package com.woodplc.cora.parser;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static java.time.Duration.ofSeconds;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class ParserTest {

	private static final String SUBPROGRAM_METADATA_FILE = "subprogram_metadata.txt";
	private static final int NUM_CORRECT_SUBPROGRAMS = 19;
	private static final long ACCEPTABLE_RESPONSE_TIME_SEC = 30;	

	@Test
	void testParserForCorrectness() throws IOException, URISyntaxException {
		Set<SubProgram> correctSubsetOfSubPrograms = Files.readAllLines(Paths.get(getClass().getResource(SUBPROGRAM_METADATA_FILE).toURI()))
				.stream()
				.map(x -> SubProgram.fromString(x.toLowerCase()))
				.collect(Collectors.toSet());
		assertEquals(NUM_CORRECT_SUBPROGRAMS, correctSubsetOfSubPrograms.size());
		
		Parser parser = Parsers.fortranParser();
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST.path());
		assertNotNull(graph);
		Set<SubProgram> testSetOfSubprograms = graph.getSubprograms();
		
		assertTrue(testSetOfSubprograms.containsAll(correctSubsetOfSubPrograms));
	}
	
	@Test
	void testParserProductionCodeResponseTime() {
		Parser flex3Parser = Parsers.fortranParser();
		assertTimeout(ofSeconds(ACCEPTABLE_RESPONSE_TIME_SEC), () -> {
			parseFiles(flex3Parser, SoftwareSystem.FLEX3.path());
		});
		
		Parser dprflex3Parser = Parsers.fortranParser();
		assertTimeout(ofSeconds(ACCEPTABLE_RESPONSE_TIME_SEC), () -> {
			parseFiles(dprflex3Parser, SoftwareSystem.DPRFLEX3.path());
		});
		
		Parser mamParser = Parsers.fortranParser();
		assertTimeout(ofSeconds(ACCEPTABLE_RESPONSE_TIME_SEC), () -> {
			parseFiles(mamParser, SoftwareSystem.MAM.path());
		});
	}
	
}
