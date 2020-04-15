package com.woodplc.cora.refactoring;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class RefactoringTest {
	
	private final IREngine engine = mock(IREngine.class);
	
	// check_pip_contact a3d3a47
	private static final String ORIGINAL_SUBPROGRAM = "original_subprogram.f90";
	// check_pip_contact_01 35ff5c6
	private static final String MANUAL_REF_SUBPROGRAM = "manual_ref_subprogram.f90";
	
	// 0c4a8fad
	private static final String ORIGINAL_SUBPROGRAM_COMMON = "original_subprogram_common.f90";
	private static final String MANUAL_REF_SUBPROGRAM_COMMON = "manual_ref_subprogram_common.f90";
	
	private static final String ORIGINAL_SUBPROGRAM_COMMON_A = "original_sub_common_array.f90";
	private static final String MANUAL_REF_SUBPROGRAM_COMMON_A = "manual_ref_sub_common_array.f90";

	private static final String ORIGINAL_EMPTY_ARG = "original_empty_argument.f90";
	private static final String MANUAL_REF_EMPTY_ARG = "manual_ref_empty_argument.f90";
	
	private static final String ORIGINAL_FUNC_F77 = "original_function_f77.for";
	private static final String MANUAL_REF_FUNC_F77 = "manual_ref_function_f77.for";
	
	@Test
	void testRWARefactoring() throws IOException, URISyntaxException {
		Path originalPath = Paths.get(getClass().getResource(ORIGINAL_SUBPROGRAM).toURI());
		Stream<String> originalSubprogram = Files.lines(originalPath);
		List<String> manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_SUBPROGRAM).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph systemGraph = parseFiles(parser, SoftwareSystem.FLEX3.path());
		assertNotNull(systemGraph);
		
		parser = Parsers.nonIndexableFortranParser();
		SDGraph cafGraph = parseFiles(parser, SoftwareSystem.CAF.path());
		assertNotNull(systemGraph);
		
		Refactoring refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		List<String> autoRefactoredSubprogram = refactoring.refactor();
		//autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
	}

	@Test
	void testRWARefactoringWithCommonBlock() throws IOException, URISyntaxException {
		Path originalPath = Paths.get(getClass().getResource(ORIGINAL_SUBPROGRAM_COMMON).toURI());
		Stream<String> originalSubprogram = Files.lines(originalPath);
		List<String> manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_SUBPROGRAM_COMMON).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph systemGraph = parseFiles(parser, SoftwareSystem.DPRFLEX3.path());
		assertNotNull(systemGraph);
		
		parser = Parsers.nonIndexableFortranParser();
		SDGraph cafGraph = parseFiles(parser, SoftwareSystem.CAF.path());
		assertNotNull(systemGraph);
		
		Refactoring refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		List<String> autoRefactoredSubprogram = refactoring.refactor();
		//autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		//with common array
		originalPath = Paths.get(getClass().getResource(ORIGINAL_SUBPROGRAM_COMMON_A).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_SUBPROGRAM_COMMON_A).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		autoRefactoredSubprogram = refactoring.refactor();
		//autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		//with empty arg list
		originalPath = Paths.get(getClass().getResource(ORIGINAL_EMPTY_ARG).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_EMPTY_ARG).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		autoRefactoredSubprogram = refactoring.refactor();
		//autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));

	}

	@Test
	void testRWARefactoringFunctionF77() throws IOException, URISyntaxException {
		Path originalPath = Paths.get(getClass().getResource(ORIGINAL_FUNC_F77).toURI());
		Stream<String> originalSubprogram = Files.lines(originalPath);
		List<String> manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_FUNC_F77).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph systemGraph = parseFiles(parser, SoftwareSystem.MAM.path());
		assertNotNull(systemGraph);
		
		parser = Parsers.nonIndexableFortranParser();
		SDGraph cafGraph = parseFiles(parser, SoftwareSystem.CAF.path());
		assertNotNull(systemGraph);
		
		Refactoring refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		List<String> autoRefactoredSubprogram = refactoring.refactor();
		//autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
	}
}
