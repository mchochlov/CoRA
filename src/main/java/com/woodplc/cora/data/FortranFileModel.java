package com.woodplc.cora.data;

import java.util.HashSet;
import java.util.Set;

public class FortranFileModel {

	private Set<SubProgram> subprograms = new HashSet<>();
	
	public Set<SubProgram> getSubprograms() {
		return subprograms;
	}
	
	public void add(SubProgram subprogram) {
		subprograms.add(subprogram);
	}

}
