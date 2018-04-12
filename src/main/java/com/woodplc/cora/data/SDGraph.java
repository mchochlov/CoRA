package com.woodplc.cora.data;

import java.util.Set;

public interface SDGraph {

	void addVariablesAndCallees(Set<String> variables, Set<String> callees);

	void addSubprogramAndCallees(SubProgram subprogram, Set<String> callees);

	boolean containsSubprogram(String subName);

	boolean containsVariable(String varName);

	Set<SubProgram> getSubprograms();

	Set<String> getSubprogramCallees(String subName);

	Set<String> getSubprogramCallers(String subName);

	Set<String> getVariableCallees(String varName);

	void merge(SDGraph graph);
}
