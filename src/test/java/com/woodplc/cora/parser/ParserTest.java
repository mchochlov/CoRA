package com.woodplc.cora.parser;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static java.time.Duration.ofSeconds;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.woodplc.cora.data.ModuleVariable;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class ParserTest {

	private final IREngine engine = mock(IREngine.class);
	
	private static final String SUBPROGRAM_METADATA_FILE = "subprogram_metadata.txt";
	private static final int NUM_CORRECT_SUBPROGRAMS = 19;
	private static final long ACCEPTABLE_RESPONSE_TIME_SEC = 45;

	private static final String FL_MODULE_METADATA_FILE = "fl_module_metadata.txt";	
	private static final int NUM_CORRECT_MODULES_FL = 152;
	private static final int FL_BODY_CORRECT_ENTRIES = 127;

	private static final String DR_MODULE_METADATA_FILE = "dr_module_metadata.txt";	
	private static final int NUM_CORRECT_MODULES_DR = 1;
	private static final int DR_BODY_CORRECT_ENTRIES = 312;
	
	private static final String PL_MODULE_METADATA_FILE = "pl_module_metadata.txt";	
	private static final int NUM_CORRECT_MODULES_PL = 41;
	private static final int PL_BODY_CORRECT_ENTRIES = 24;

	@Test
	void testParserForCorrectness() throws IOException, URISyntaxException {
		Set<SubProgram> correctSubsetOfSubPrograms = Files.readAllLines(Paths.get(getClass().getResource(SUBPROGRAM_METADATA_FILE).toURI()))
				.stream()
				.map(SubProgram::fromString)
				.collect(Collectors.toSet());
		assertEquals(NUM_CORRECT_SUBPROGRAMS, correctSubsetOfSubPrograms.size());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST.path());
		assertNotNull(graph);
		Set<SubProgram> testSetOfSubprograms = graph.subprograms();
		
		verify(engine, times(testSetOfSubprograms.size())).index(Mockito.anyString(), Mockito.anyString());
		assertTrue(testSetOfSubprograms.containsAll(correctSubsetOfSubPrograms));		
	}
	
	
	@Test
	void testSampleModulesFromFlex3() throws IOException, URISyntaxException {
		Set<String> correctSubsetOfModules = Files.readAllLines(Paths.get(getClass().getResource(FL_MODULE_METADATA_FILE).toURI()))
				.stream()
				.map(x -> x.toLowerCase())
				.collect(Collectors.toSet());
		assertEquals(NUM_CORRECT_MODULES_FL, correctSubsetOfModules.size());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST_MOD_FL.path());
		assertNotNull(graph);
		
		Set<String> testSetOfModules = graph.modules();
		assertTrue(testSetOfModules.equals(correctSubsetOfModules));		
		
		// Test private fields are not included
		assertTrue(testSetOfModules.contains("warnings"));
		assertNull(graph.getModuleVariable("warnings", "num_flex3_warnings"));
		
		// Test public by default is included
		assertTrue(testSetOfModules.contains("interrupt"));
		assertNotNull(graph.getModuleVariable("interrupt", "ctrl_c"));
		
		//Test parameters are included
		assertTrue(testSetOfModules.contains("load_timetrace_data"));
		assertNotNull(graph.getModuleVariable("load_timetrace_data", "load_file_line_len"));
		
		//Test various data types
		assertTrue(testSetOfModules.contains("floating_body_data"));
		assertEquals(FL_BODY_CORRECT_ENTRIES, graph.getAllModuleVariables("floating_body_data").size());
		//subprogram
		ModuleVariable mv = graph.getModuleVariable("floating_body_data", "allocate_fb_data");
		assertNotNull(mv);
		assertEquals(mv.getType(), "external");
		//scalar integer
		mv = graph.getModuleVariable("floating_body_data", "fb_mxwinhd");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "integer");
		//array integer
		mv = graph.getModuleVariable("floating_body_data", "num_timesteps");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "integer");
		assertEquals(mv.getAllocation(), "num_fbset");
		//scalar real
		mv = graph.getModuleVariable("floating_body_data", "timeold");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "real(8)");
		//array real
		mv = graph.getModuleVariable("floating_body_data", "y_sum");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "real(8)");
		assertEquals(mv.getAllocation(), "num_fbset");
		//scalar logical
		mv = graph.getModuleVariable("floating_body_data", "perform_convolution");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "logical");
		//array logical
		mv = graph.getModuleVariable("floating_body_data", "floating_body_mass_added");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "logical");
		assertEquals(mv.getAllocation(), "num_fbset");
		//array character
		mv = graph.getModuleVariable("floating_body_data", "fb_set");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "character(len=32)");
		assertEquals(mv.getAllocation(), "num_fbset");
		//multidimensional array
		mv = graph.getModuleVariable("floating_body_data", "store_displacements");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "real(8)");
		assertEquals(mv.getAllocation(), "num_fbset,max_timesteps,3");
		
		//type
		mv = graph.getModuleVariable("db_output", "block_address_db");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "type(t_block_addresses)");
	}
	
	@Test
	void testSampleModulesFromDprFlex3() throws IOException, URISyntaxException {
		Set<String> correctSubsetOfModules = Files.readAllLines(Paths.get(getClass().getResource(DR_MODULE_METADATA_FILE).toURI()))
				.stream()
				.map(x -> x.toLowerCase())
				.collect(Collectors.toSet());
		assertEquals(NUM_CORRECT_MODULES_DR, correctSubsetOfModules.size());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST_MOD_DR.path());
		assertNotNull(graph);
		
		Set<String> testSetOfModules = graph.modules();
		assertTrue(testSetOfModules.equals(correctSubsetOfModules));		
		
		//Test assignments are included
		assertTrue(testSetOfModules.contains("maina_and_mainb_arrays"));
		assertNotNull(graph.getModuleVariable("maina_and_mainb_arrays", "non_condcas"));
		
		//Test various data types
		assertTrue(testSetOfModules.contains("maina_and_mainb_arrays"));
		assertEquals(DR_BODY_CORRECT_ENTRIES, graph.getAllModuleVariables("maina_and_mainb_arrays").size());
		//subprogram
		ModuleVariable mv = graph.getModuleVariable("maina_and_mainb_arrays", "maina_allocate_1");
		assertNotNull(mv);
		assertEquals(mv.getType(), "external");
		//scalar integer
		mv = graph.getModuleVariable("maina_and_mainb_arrays", "stand_condcas");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "integer");
		//array integer
		mv = graph.getModuleVariable("maina_and_mainb_arrays", "iaelem");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "integer");
		assertEquals(mv.getAllocation(), "naelem,4");
		//array real
		mv = graph.getModuleVariable("maina_and_mainb_arrays", "tglsb");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "real(8)");
		assertEquals(mv.getAllocation(), "3,3,nonp");
		//array character
		mv = graph.getModuleVariable("maina_and_mainb_arrays", "bcnffl");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "character(len=255)");
		assertEquals(mv.getAllocation(), "nbffil");
	}
	
	@Test
	void testSampleModulesFromMam() throws IOException, URISyntaxException {
		Set<String> correctSubsetOfModules = Files.readAllLines(Paths.get(getClass().getResource(PL_MODULE_METADATA_FILE).toURI()))
				.stream()
				.map(x -> x.toLowerCase())
				.collect(Collectors.toSet());
		assertEquals(NUM_CORRECT_MODULES_PL, correctSubsetOfModules.size());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph graph = parseFiles(parser, SoftwareSystem.TEST_MOD_PL.path());
		assertNotNull(graph);
		
		Set<String> testSetOfModules = graph.modules();
		assertTrue(testSetOfModules.equals(correctSubsetOfModules));		
		
		//Test assignments are included
		assertTrue(testSetOfModules.contains("wave_data"));
		assertNotNull(graph.getModuleVariable("wave_data", "No_Wave"));
		
		//Test various data types
		assertTrue(testSetOfModules.contains("tensioner_data"));
		assertEquals(PL_BODY_CORRECT_ENTRIES, graph.getAllModuleVariables("tensioner_data").size());
		//subprogram
		ModuleVariable mv = graph.getModuleVariable("tensioner_data", "allocate_tensioner_data");
		assertNotNull(mv);
		assertEquals(mv.getType(), "external");
		//scalar integer
		mv = graph.getModuleVariable("tensioner_data", "max_tensioners");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "integer");
		//array integer
		mv = graph.getModuleVariable("tensioner_data", "tensioner_type");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "integer");
		assertEquals(mv.getAllocation(), "max_tensioners");
		//scalar real
		mv = graph.getModuleVariable("tensioner_data", "static_max_tension");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "real(8)");
		//array real
		mv = graph.getModuleVariable("tensioner_data", "act_tensioner_data");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "real(8)");
		assertEquals(mv.getAllocation(), "5");
		//scalar logical
		mv = graph.getModuleVariable("tensioner_data", "active_tensioners");
		assertNotNull(mv);
		assertTrue(mv.isScalar());
		assertEquals(mv.getType(), "logical");
		//array character
		mv = graph.getModuleVariable("tensioner_data", "tensioner_sets");
		assertNotNull(mv);
		assertTrue(!mv.isScalar());
		assertEquals(mv.getType(), "character(len=set_length)");
		assertEquals(mv.getAllocation(), "max_tensioners");
	}
	
	@Test
	void testParserProductionCodeResponseTime() {
		Parser flex3Parser = Parsers.indexableFortranParser(engine);
		assertTimeout(ofSeconds(ACCEPTABLE_RESPONSE_TIME_SEC), () -> {
			parseFiles(flex3Parser, SoftwareSystem.FLEX3.path());
		});
		
		Parser dprflex3Parser = Parsers.indexableFortranParser(engine);
		assertTimeout(ofSeconds(ACCEPTABLE_RESPONSE_TIME_SEC), () -> {
			parseFiles(dprflex3Parser, SoftwareSystem.DPRFLEX3.path());
		});
		
		Parser mamParser = Parsers.indexableFortranParser(engine);
		assertTimeout(ofSeconds(ACCEPTABLE_RESPONSE_TIME_SEC), () -> {
			parseFiles(mamParser, SoftwareSystem.MAM.path());
		});
	}
	
}
