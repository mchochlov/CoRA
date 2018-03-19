package com.woodplc.cora.data;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

class StubGraph implements SDGraph {
	
	private Set<SubProgram> subprograms = new HashSet<>();

	@Override
	public Set<SubProgram> getSubprograms() {
		return subprograms;
	}

	@Override
	public void add(FortranFileModel model) {
		Objects.requireNonNull(model);
		subprograms.addAll(model.getSubprograms());
	}

}
