package com.woodplc.cora.data;

import static java.util.stream.Collectors.toSet;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Multimaps;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;
import com.google.common.graph.Traverser;

final class GuavaBasedSDGraph implements SDGraph {
		
	private final Set<SubProgram> subprograms = new HashSet<>();

	private final MutableGraph<String> subprogramCallGraph = GraphBuilder
			.directed()
			.allowsSelfLoops(true)
			.build();
	private final SetMultimap<String, String> variableCallees = SetMultimapBuilder
			.hashKeys()
			.hashSetValues()
			.build();
	
	private ImmutableSet<String> unreferencedSubprograms = null;
	
	GuavaBasedSDGraph() {}
	
	GuavaBasedSDGraph(Set<SubProgram> subprograms, 
			Set<CallEdge> edges,
			Map<String, Collection<String>> variables) {
		Objects.requireNonNull(subprograms);
		Objects.requireNonNull(edges);
		Objects.requireNonNull(variables);
		
		this.subprograms.addAll(subprograms);
		this.subprograms.stream()
				.map(SubProgram::name)
				.forEach(subprogramCallGraph::addNode);
		
		edges.forEach(edge -> subprogramCallGraph.putEdge(edge.source(), edge.target()));
		variables.forEach(variableCallees::putAll);
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
		
		if (!subprograms.add(subprogram)) {
			throw new IllegalStateException(subprogram.toString());
		}
		subprogramCallGraph.addNode(subprogram.name());
		if (!callees.isEmpty()) {
			for (String callee : callees) {
				if (!subprogramCallGraph.putEdge(subprogram.name(), callee)) {
					throw new IllegalStateException();
				}
			}
		}
	}
	
	@Override
	public boolean containsSubprogram(String subName) {
		return subprogramCallGraph.nodes().contains(subName);
	}
	
	@Override
	public boolean containsVariable(String varName) {
		return variableCallees.containsKey(varName);
	}

	@Override
	public Set<SubProgram> subprograms() {
		return new HashSet<>(subprograms);
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
		
		if (g.isEmpty() || g.equals(this)) return;
		
		g.subprogramCallGraph.nodes().forEach(x -> {
			this.subprogramCallGraph.addNode(x);
		});
		g.subprogramCallGraph.edges().forEach(x -> {
			this.subprogramCallGraph.putEdge(x.source(), x.target());
		});
		this.subprograms.addAll(g.subprograms);
		this.variableCallees.putAll(g.variableCallees);
		g = null;
	}

	@Override
	public Map<String, Set<String>> getVariablesAndCallees(String subname) {
		return Multimaps.asMap(variableCallees)
				.entrySet()
				.stream()
				.filter(x -> x.getValue().size() > 1 && x.getValue().contains(subname))
				.collect(Collectors.toMap(Map.Entry::getKey, e -> new HashSet<>(e.getValue())));
	}

	@Override
	public boolean isEmpty() {
		return subprograms.isEmpty() 
				&& subprogramCallGraph.nodes().isEmpty() 
				&& variableCallees.isEmpty();
	}

	@Override
	public Set<CallEdge> edges() {
		return subprogramCallGraph.edges()
				.stream()
				.map(pair -> new CallEdge(pair.source(), pair.target()))
				.collect(toSet());
	}

	@Override
	public Set<String> nodes() {
		return new HashSet<>(subprogramCallGraph.nodes());
	}

	@Override
	public Map<String, Collection<String>> variables() {
		return new HashMap<>(variableCallees.asMap());
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof GuavaBasedSDGraph)) return false;
		
		GuavaBasedSDGraph g = (GuavaBasedSDGraph) o;
		
		return this.subprograms.equals(g.subprograms)
				&& this.subprogramCallGraph.equals(g.subprogramCallGraph)
				&& this.variableCallees.equals(g.variableCallees);
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.subprograms, this.subprogramCallGraph, this.variableCallees);
	}

	@Override
	public Set<String> getUnreferencedSubprograms() throws ProgramEntryNotFoundException {

		if (unreferencedSubprograms == null) {
			Optional<SubProgram> startNode = subprograms
					.stream()
					.filter(s -> s instanceof Program)
					.findFirst();
			if (!startNode.isPresent()) throw new ProgramEntryNotFoundException();
			Set<String> functions = subprograms.stream()
					.filter(f -> f instanceof Function)
					.map(SubProgram::name)
					.collect(Collectors.toSet());
			Traverser<String> traverser = Traverser.forGraph(subprogramCallGraph);
			Set<String> referencedSubprograms = Sets.newHashSet(traverser.depthFirstPostOrder(startNode.get().name()));
			unreferencedSubprograms = Sets.difference(subprogramCallGraph.nodes(), 
					Sets.union(functions, referencedSubprograms))
					.immutableCopy();
		}
		return unreferencedSubprograms;
	}
}
