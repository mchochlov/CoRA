package com.woodplc.cora.data;

public final class Graphs {
	
	private Graphs() {}

	public static SDGraph newInstance() {
		return new GuavaBasedSDGraph();
	}
	
}
