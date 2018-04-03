package com.woodplc.cora.data;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

class StubGraph implements SDGraph {
	
	private Set<SubProgram> subprograms = new HashSet<>();

	@Override
	public Set<SubProgram> getSubprograms() {
		return Collections.unmodifiableSet(subprograms);
	}

	@Override
	public void add(FortranFileModel model) {
		Objects.requireNonNull(model);
		subprograms.addAll(model.getSubprograms());
	}

	@Override
	public boolean containsSubprogram(String subName) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Set<String> getSubprogramCallees(String subName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Set<String> getSubprogramCallers(String subName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean containsVariable(String varName) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Set<String> getVariableCallees(String varName) {
		// TODO Auto-generated method stub
		return null;
	}

}
