package com.woodplc.cora.data;

import java.nio.file.Path;

public final class ImmutableModule {
	private Path path;
	private SDGraph graph;

	public ImmutableModule() {}

	public Path getPath() {
		return path;
	}

	public void setPath(Path path) {
		this.path = path;
	}

	public SDGraph getGraph() {
		return graph;
	}

	public void setGraph(SDGraph graph) {
		this.graph = graph;
	}
}