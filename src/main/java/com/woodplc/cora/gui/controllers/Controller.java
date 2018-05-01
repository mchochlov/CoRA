package com.woodplc.cora.gui.controllers;

import java.util.Objects;

import com.woodplc.cora.data.SDGraph;

import javafx.collections.ObservableList;

abstract class Controller {
	
	protected final String subname;
	protected final SDGraph graph;
	protected final ObservableList<String> systemASubprograms;
	
	Controller(String subname, SDGraph graph, ObservableList<String> systemASubprograms) {
		this.subname = Objects.requireNonNull(subname);
		this.graph = Objects.requireNonNull(graph);
		this.systemASubprograms = Objects.requireNonNull(systemASubprograms);
		
		if (!graph.containsSubprogram(subname)) {
			throw new IllegalStateException();
		}
	}
}
