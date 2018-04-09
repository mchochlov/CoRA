package com.woodplc.cora.data;

public final class Graphs {
	
	private Graphs() {}

	public static SDGraph getSDGraphInstance() {
		return new GuavaBasedSDGraph();
	}
	
}
