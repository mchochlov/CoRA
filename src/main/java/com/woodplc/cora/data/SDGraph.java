package com.woodplc.cora.data;

import java.util.Set;

public interface SDGraph {

	Set<SubProgram> getSubprograms();

	void add(FortranFileModel model);

	boolean containsSubprogram(String subName);

	Set<String> getSubprogramCallees(String subName);

	Set<String> getSubprogramCallers(String subName);

	boolean containsVariable(String varName);

	Set<String> getVariableCallees(String varName);

}
