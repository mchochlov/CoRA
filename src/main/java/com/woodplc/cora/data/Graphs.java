package com.woodplc.cora.data;

public final class Graphs {
	
	private Graphs() {}

	public static SDGraph stubGraph() {
		return new StubGraph();
	}
	
}
