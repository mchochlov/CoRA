package com.woodplc.cora.refactoring;

import java.nio.file.Path;
import java.util.Objects;
import java.util.stream.Stream;

import com.woodplc.cora.data.SDGraph;

public final class Refactorings {

	public static Refactoring createRWARefactoring(Path originalPath, Stream<String> originalSubprogram, SDGraph systemGraph,
			SDGraph cafGraph) {
		Objects.requireNonNull(originalPath);
		Objects.requireNonNull(originalSubprogram);
		Objects.requireNonNull(systemGraph);
		Objects.requireNonNull(cafGraph);
		return new RWARefactoring(originalPath, originalSubprogram, systemGraph, cafGraph);
	}

}
