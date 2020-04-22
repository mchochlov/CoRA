package com.woodplc.cora.refactoring;

import static com.woodplc.cora.utils.TestUtils.parseFiles;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
import java.util.Set;
import java.util.stream.Stream;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.CheckoutConflictException;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRefNameException;
import org.eclipse.jgit.api.errors.RefAlreadyExistsException;
import org.eclipse.jgit.api.errors.RefNotFoundException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;
import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class RefactoringTest {
	
	private static Git flexcomRepo;
	private static Git deepRiserRepo;
	private static Git pipeLayRepo;
	private static Git cafRepo;
	
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
	
	private static final String P1_ORIGINAL_EMPTY_ARG = "p1_original_empty_argument.f90";
	private static final String P1_REF_EMPTY_ARG = "p1_ref_empty_argument.f90";
	
	private static final String ORIGINAL_SUB_F77 = "original_sub_f77.for";
	private static final String MANUAL_REF_SUB_F77 = "manual_ref_sub_f77.for";
	
	private static final String P1_ORIGINAL_SUB_F77 = "p1_original_sub_f77.for";
	private static final String P1_MANUAL_REF_SUB_F77 = "p1_ref_sub_f77.for";
	
	private static final String ADD_PIP_ORIG = "update_inner_pip_nodes_orig.f90";
	private static final String ADD_PIP_REF = "update_inner_pip_nodes_ref.f90";
	
	private static final String P1_NODES_ORIG = "p1_nodes_orig.f90";
	private static final String P1_NODES_REF = "p1_nodes_ref.f90";
	
	private static final String FESOLVER_BRANCH = "12498-FESolver";
	private static final String PIP_BRANCH = "12824_Pipe-in-Pipe";
	private static final String P1_ORIG_NDFORC = "p1_orig_ndforc.f90";
	private static final String P1_REF_NDFORC = "p1_ref_ndforc.f90";
	
	@BeforeAll
	public static void initGitRepos() throws IOException, RefAlreadyExistsException, RefNotFoundException, InvalidRefNameException, CheckoutConflictException, GitAPIException {
		flexcomRepo = Git.open(SoftwareSystem.FLEXCOM.path().toFile());
		flexcomRepo.checkout().setName(FESOLVER_BRANCH).call();
		deepRiserRepo = Git.open(SoftwareSystem.DEEPRISER.path().toFile());
		deepRiserRepo.checkout().setName(FESOLVER_BRANCH).call();
		pipeLayRepo = Git.open(SoftwareSystem.PIPELAY.path().toFile());
		pipeLayRepo.checkout().setName(FESOLVER_BRANCH).call();
		cafRepo = Git.open(SoftwareSystem.CAF_ROOT.path().toFile());
		cafRepo.checkout().setName(PIP_BRANCH).call();
	}
	
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
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		originalPath = Paths.get(getClass().getResource(ADD_PIP_ORIG).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(ADD_PIP_REF).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		//test caller
		Set<String> callers = systemGraph.getSubprogramCallers("update_inner_pip_outer_nodes");
		assertNotNull(callers);
		assertEquals(callers.size(), 1);
		originalPath = Paths.get(getClass().getResource(P1_NODES_ORIG).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(P1_NODES_REF).toURI()));

		refactoring = Refactorings.createCallerRefactoring(originalPath, originalSubprogram, refactoring);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
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
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		//with common array
		originalPath = Paths.get(getClass().getResource(ORIGINAL_SUBPROGRAM_COMMON_A).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_SUBPROGRAM_COMMON_A).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		//test caller
		Set<String> callers = systemGraph.getSubprogramCallers("TRANSF");
		assertNotNull(callers);
		assertEquals(callers.size(), 3);
		originalPath = Paths.get(getClass().getResource(P1_ORIG_NDFORC).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(P1_REF_NDFORC).toURI()));

		refactoring = Refactorings.createCallerRefactoring(originalPath, originalSubprogram, refactoring);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));		
		
		//with empty arg list
		originalPath = Paths.get(getClass().getResource(ORIGINAL_EMPTY_ARG).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_EMPTY_ARG).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		//test caller
		callers = systemGraph.getSubprogramCallers("OPEN_PARAMETER_LOG_FILE");
		assertNotNull(callers);
		assertEquals(callers.size(), 1);
		originalPath = Paths.get(getClass().getResource(P1_ORIGINAL_EMPTY_ARG).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(P1_REF_EMPTY_ARG).toURI()));

		refactoring = Refactorings.createCallerRefactoring(originalPath, originalSubprogram, refactoring);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
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
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		originalPath = Paths.get(getClass().getResource(ORIGINAL_SUB_F77).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(MANUAL_REF_SUB_F77).toURI()));
		
		assertNotNull(originalSubprogram);
		assertFalse(manuallyRefactoredSubprogram.isEmpty());
		refactoring = Refactorings.createRWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
		//test caller
		Set<String> callers = systemGraph.getSubprogramCallers("PARTS");
		assertNotNull(callers);
		assertEquals(callers.size(), 1);
		originalPath = Paths.get(getClass().getResource(P1_ORIGINAL_SUB_F77).toURI());
		originalSubprogram = Files.lines(originalPath);
		manuallyRefactoredSubprogram = Files.readAllLines(Paths.get(getClass().getResource(P1_MANUAL_REF_SUB_F77).toURI()));

		refactoring = Refactorings.createCallerRefactoring(originalPath, originalSubprogram, refactoring);
		autoRefactoredSubprogram = refactoring.refactor();
		autoRefactoredSubprogram.forEach(System.out::println);
		assertTrue(manuallyRefactoredSubprogram.equals(autoRefactoredSubprogram));
		
	}
	
	@AfterAll
	public static void closeGitRepos() {
		flexcomRepo.close();
		deepRiserRepo.close();
		pipeLayRepo.close();
		cafRepo.close();
	}
}
