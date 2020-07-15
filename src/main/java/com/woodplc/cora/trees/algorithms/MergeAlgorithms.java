package com.woodplc.cora.trees.algorithms;

import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import com.woodplc.cora.data.OrderedLinkedMap;

public final class MergeAlgorithms {

	public static CloneMergeAlgorithm defaultTokenSequenceMergeAlgorithm(OrderedLinkedMap<Path, Stream<String>> originalSubprograms) {
		for (Map.Entry<Path, Stream<String>> entry : originalSubprograms.entrySet()) {
			Objects.requireNonNull(entry.getKey());
			Objects.requireNonNull(entry.getValue());
		}
		
		return new TokenSequenceMergeAlgorithm(originalSubprograms, false);
	}

	public static AlgorithmBuilder configure() {
		return new AlgorithmBuilder();
	}

	public static class AlgorithmBuilder {
		
		private boolean useSuperFunctionality;
		
		private AlgorithmBuilder() {}

		public AlgorithmBuilder withSuperFunctionalityRefactoring() {
			this.useSuperFunctionality = true;
			return this;
		}

		public CloneMergeAlgorithm tokenSequenceMerge(OrderedLinkedMap<Path, Stream<String>> originalSubprograms) {
			for (Map.Entry<Path, Stream<String>> entry : originalSubprograms.entrySet()) {
				Objects.requireNonNull(entry.getKey());
				Objects.requireNonNull(entry.getValue());
			}
			
			return new TokenSequenceMergeAlgorithm(originalSubprograms, useSuperFunctionality);
		}
		
	}
}
