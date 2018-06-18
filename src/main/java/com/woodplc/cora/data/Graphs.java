package com.woodplc.cora.data;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public final class Graphs {
	
	private Graphs() {}

	public static SDGraph newInstance() {
		return new GuavaBasedSDGraph();
	}

	public static SDGraph newInstanceFromValues(Set<SubProgram> subprograms,
			Set<CallEdge> edges, 
			Map<String, Collection<String>> variables) {
		return new GuavaBasedSDGraph(subprograms, edges, variables);
	}
	
}
