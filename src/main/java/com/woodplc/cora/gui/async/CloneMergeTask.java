package com.woodplc.cora.gui.async;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.woodplc.cora.data.OrderedLinkedMap;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.trees.algorithms.CloneMergeAlgorithm;
import com.woodplc.cora.trees.algorithms.MergeAlgorithms.AlgorithmBuilder;
import com.woodplc.cora.utils.Utils;

import javafx.concurrent.Task;

public final class CloneMergeTask extends Task<List<String>> {
	
	final Logger logger = LoggerFactory.getLogger(CloneMergeTask.class);
	private final AlgorithmBuilder aBuilder;
	private final List<SubProgram> subprograms;
	
	public CloneMergeTask(AlgorithmBuilder aBuilder, List<SubProgram> subprograms) {
		Objects.requireNonNull(aBuilder);
		this.aBuilder = aBuilder;
		this.subprograms = subprograms;
	}

	@Override
	protected List<String> call() throws Exception {
		logger.info("Starting clone class refactoring and merge");
		updateMessage("Starting clone class refactoring and merge");
		
		OrderedLinkedMap<Path, Stream<String>> subs = new OrderedLinkedMap<>();
		for (SubProgram subprogram : subprograms) {
			logger.info("Reading subprogram: " + subprogram.name());
			updateMessage("Reading subprogram: " + subprogram.name());
			Stream<String> originalSubprogram = Utils.readFullLinesFromFile(subprogram.path())
					.stream()
					.limit(subprogram.endLine()).skip(subprogram.startLine() - 1);
			subs.put(subprogram.path(), originalSubprogram);
		}
		
		CloneMergeAlgorithm cma = aBuilder.tokenSequenceMerge(subs);
		logger.info("Merging subprograms");
		updateMessage("Merging subprograms");
		
		return cma.merge();
	}

}
