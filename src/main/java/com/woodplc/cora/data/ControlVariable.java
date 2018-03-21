package com.woodplc.cora.data;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class ControlVariable {

	private final String varname;
	private int count;
	private final Set<String> callees;
	
	private ControlVariable(String identifier, Set<String> callees) {
		this.varname = identifier;
		this.callees = new HashSet<>(callees);
		this.count = 1;
	}

	public static ControlVariable fromValues(String varname, Set<String> callees) {
		Objects.requireNonNull(varname, "Variable name cannot be null!");
		Objects.requireNonNull(callees);
		
		if (varname.isEmpty() || callees.isEmpty()) {
			throw new IllegalArgumentException();
		}
		
		return new ControlVariable(varname, callees);
	}
	
	public void addCallees(Set<String> callees) {
		count++;
		this.callees.addAll(callees);
	}

}
