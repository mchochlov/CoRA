package com.woodplc.cora.data;

import static java.util.stream.Collectors.toSet;

import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.Multimaps;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;
import com.google.common.graph.Traverser;

public final class GuavaBasedSDGraph implements SDGraph {
		
	private final Table<String, String, ModuleVariable> moduleVariableMap;
	private final Set<SubProgram> subprograms;
	private final MutableGraph<String> subprogramCallGraph;
	private final SetMultimap<String, String> variableCallees;
	private final Set<String> allModules;
	
	private transient ImmutableSet<String> unreferencedSubprograms = null;
	private transient ImmutableSet<String> externalSubprograms =  null;
	
	GuavaBasedSDGraph() {
		moduleVariableMap = TreeBasedTable.create(String.CASE_INSENSITIVE_ORDER, String.CASE_INSENSITIVE_ORDER);
		subprograms = new HashSet<>();
		subprogramCallGraph = GraphBuilder
				.directed()
				.allowsSelfLoops(true)
				.build();
		variableCallees = SetMultimapBuilder
				.hashKeys()
				.hashSetValues()
				.build();
		allModules = new HashSet<>();
	}
	
	public GuavaBasedSDGraph(SDGraph graph) {
		Objects.requireNonNull(graph);
		if (!(graph instanceof GuavaBasedSDGraph)) throw new IllegalArgumentException();
		GuavaBasedSDGraph g = (GuavaBasedSDGraph) graph;
		moduleVariableMap = TreeBasedTable.create((TreeBasedTable<String, String, ModuleVariable>) g.moduleVariableMap);
		subprograms = new HashSet<>(g.subprograms);
		subprogramCallGraph = com.google.common.graph.Graphs.copyOf(g.subprogramCallGraph);
		variableCallees = SetMultimapBuilder.hashKeys().hashSetValues().build(g.variableCallees);
		allModules = new HashSet<>(g.allModules);
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
		this.moduleVariableMap.putAll(g.moduleVariableMap);
		this.variableCallees.putAll(g.variableCallees);
		this.allModules.addAll(g.allModules);
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
				&& variableCallees.isEmpty()
				&& moduleVariableMap.isEmpty()
				&& allModules.isEmpty();
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
				&& this.moduleVariableMap.equals(g.moduleVariableMap)
				&& this.subprogramCallGraph.equals(g.subprogramCallGraph)
				&& this.variableCallees.equals(g.variableCallees)
				&& this.allModules.equals(g.allModules);
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.subprograms, this.moduleVariableMap, this.subprogramCallGraph, this.variableCallees, this.allModules);
	}

	@Override
	public Set<String> getUnreferencedSubprograms() throws ProgramEntryNotFoundException {

		if (unreferencedSubprograms == null) {
			Optional<SubProgram> startNode = subprograms
					.stream()
					.filter(s -> s instanceof Program)
					.findFirst();
			if (!startNode.isPresent()) {
				unreferencedSubprograms = ImmutableSet.of();
				throw new ProgramEntryNotFoundException();
			}

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

	@Override
	public Set<String> getExternalSubprograms() throws ProgramEntryNotFoundException {
		if (externalSubprograms == null) {
			Set<String> internalSubprograms = subprograms.stream()
					.map(s -> s.name())
					.collect(Collectors.toSet());
			externalSubprograms = Sets.difference(subprogramCallGraph.nodes(), 
					internalSubprograms)
					.immutableCopy();
		}
		return externalSubprograms;
	}

	@Override
	public void printSubgraph(Set<String> subGraphNodes) {
		MutableGraph<String> mGraph = com.google.common.graph.Graphs.inducedSubgraph(subprogramCallGraph, subGraphNodes);
		for (String node : mGraph.nodes()) {
			if (mGraph.predecessors(node).isEmpty()) {
				Deque<String> queue = new LinkedList<>();
				queue.add(node);
				int level = 0;
				while (!queue.isEmpty()) {
					System.out.print(++level + " ");
					int qLen = queue.size();
					for (int i = 0; i < qLen; i++) {
						String n = queue.pollLast();
						if(!subprogramCallGraph.predecessors(n).equals(mGraph.predecessors(n))) {
							System.out.print("Public ");
						}
						System.out.print(n + " ");
						for (String s : mGraph.successors(n)) {
							queue.addFirst(s);
						}
					}
					System.out.println("\n");
				}
			}
		}
		
	}

	@Override
	public Set<String> modules() {
		Set<String> ts = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
		ts.addAll(allModules);
		return ts;
	}

	@Override
	public void addModuleVariable(String moduleName, String variableName, ModuleVariable moduleVariable) {
		Objects.requireNonNull(moduleName);
		Objects.requireNonNull(variableName);
		
		if (moduleName.isEmpty() || moduleVariableMap.put(moduleName, variableName, moduleVariable) != null) {
			throw new IllegalStateException(moduleName + " " + variableName);
		}
	}

	@Override
	public ModuleVariable getModuleVariable(String moduleName, String variableName) {
		Objects.requireNonNull(moduleName);
		Objects.requireNonNull(variableName);
		
		return moduleVariableMap.get(moduleName, variableName);
	}

	@Override
	public Set<String> getAllModuleVariables(String moduleName) {
		return moduleVariableMap.row(moduleName).keySet();
	}
	
	

	@Override
	public void addGraphNodes(Set<SubProgram> subprograms) {
		Objects.requireNonNull(subprograms);
		for (SubProgram subProgram : subprograms) {
			subprogramCallGraph.addNode(subProgram.name());
		}
		
	}

	@Override
	public Map<String, ModuleVariable> getAllVariableModules(String variableName) {
		return moduleVariableMap.column(variableName);
	}

	@Override
	public void addModule(String currentModule) {
		this.allModules.add(currentModule);
	}

	@Override
	public void updateSubprograms(Set<SubProgram> subprograms) {
		this.subprograms.removeAll(subprograms);
		this.subprograms.addAll(subprograms);
	}
}
