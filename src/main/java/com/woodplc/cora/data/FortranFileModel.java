package com.woodplc.cora.data;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class FortranFileModel {

	private final Set<SubProgram> subprograms = new HashSet<>();
	private final Map<String, ControlVariable> controlVariables = new HashMap<>();
	
	public Set<SubProgram> getSubprograms() {
		return Collections.unmodifiableSet(subprograms);
	}
	
	public void addSubprogram(SubProgram subprogram) {
		Objects.requireNonNull(subprogram);
		subprograms.add(subprogram);
	}

	public void addControlVariables(Set<String> identifiers, Set<String> callees) {
		Objects.requireNonNull(identifiers);
		Objects.requireNonNull(callees);
		
		for (String identifier : identifiers) {
			if (!controlVariables.containsKey(identifier)) {
				controlVariables.put(identifier, ControlVariable.fromValues(identifier, callees));
			} else {
				ControlVariable cv = controlVariables.get(identifier);
				cv.addCallees(callees);
			}
		}
	}
}
