package com.woodplc.cora.data;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.SetMultimap;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;

class GuavaBasedSDGraph implements SDGraph {
	
	private final SetMultimap<String, SubProgram> subprograms;
	private final MutableGraph<String> subprogramCallGraph;
	private final SetMultimap<String, String> variableCallees;
	
	GuavaBasedSDGraph() {
		subprograms = SetMultimapBuilder
				.hashKeys()
				.hashSetValues()
				.build();
		subprogramCallGraph = GraphBuilder
				.directed()
				.allowsSelfLoops(true)
				.build();
		variableCallees = SetMultimapBuilder
				.hashKeys()
				.hashSetValues()
				.build();
	}
	
	@Override
	public void addVariablesAndCallees(Set<String> variables, Set<String> callees) {
		Objects.requireNonNull(variables);
		Objects.requireNonNull(callees);
		variables.forEach(x -> {
			this.variableCallees.putAll(x, callees);
		});
	}

	@Override
	public void addSubprogramAndCallees(SubProgram subprogram, Set<String> callees) {
		Objects.requireNonNull(subprogram);
		Objects.requireNonNull(callees);
		this.subprograms.put(subprogram.name(), subprogram);
		
		if (callees.isEmpty()) {
			this.subprogramCallGraph.addNode(subprogram.name());
		} else {
			callees.forEach(x -> {
				this.subprogramCallGraph.putEdge(subprogram.name(), x);
			});
		}
	}
	
	@Override
	public boolean containsSubprogram(String subName) {
		return subprograms.containsKey(subName);
	}
	
	@Override
	public boolean containsVariable(String varName) {
		return variableCallees.containsKey(varName);
	}

	@Override
	public Set<SubProgram> getSubprograms() {
		return new HashSet<>(subprograms.values());
	}

	@Override
	public int getFanIn(String subName) {
		return subprogramCallGraph.inDegree(subName);
	}

	@Override
	public int getFanOut(String subName) {
		return subprogramCallGraph.outDegree(subName);
	}

	@Override
	public Set<String> getSubprogramCallees(String subName) {
		return new HashSet<>(subprogramCallGraph.successors(subName));
	}

	@Override
	public Set<String> getSubprogramCallers(String subName) {
		return new HashSet<>(subprogramCallGraph.predecessors(subName));
	}

	@Override
	public Set<String> getVariableCallees(String varName) {
		return new HashSet<>(variableCallees.get(varName));
	}

	@Override
	public void merge(SDGraph graph) {
		Objects.requireNonNull(graph);
		if (!(graph instanceof GuavaBasedSDGraph)) {
			throw new IllegalArgumentException();
		}
		
		GuavaBasedSDGraph g = (GuavaBasedSDGraph) graph;
		
		g.subprogramCallGraph.nodes().forEach(x -> {
			this.subprogramCallGraph.addNode(x);
		});
		g.subprogramCallGraph.edges().forEach(x -> {
			this.subprogramCallGraph.putEdge(x.source(), x.target());
		});
		this.subprograms.putAll(g.subprograms);
		this.variableCallees.putAll(g.variableCallees);
		g = null;
	}

}
