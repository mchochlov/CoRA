package com.woodplc.cora.data;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public interface SDGraph {

	void addVariablesAndCallees(Set<String> variables, Set<String> callees);

	void addSubprogramAndCallees(SubProgram subprogram, Set<String> callees);

	boolean containsSubprogram(String subName);

	boolean containsVariable(String varName);
	
	Set<CallEdge> edges();
	
	Set<SubProgram> getSubprograms();
	
	int getFanIn(String subName);
	
	int getFanOut(String subName);
	
	Set<String> getSubprogramCallees(String subName);

	Set<String> getSubprogramCallers(String subName);

	Set<String> getVariableCallees(String varName);
	
	boolean isEmpty();

	void merge(SDGraph graph);
	
	Set<String> nodes();

	Map<String, Collection<String>> variables();
	
	Map<String, Collection<SubProgram>> subprograms();
	
	Map<String, Set<String>> getVariablesAndCallees(String subname);
}
