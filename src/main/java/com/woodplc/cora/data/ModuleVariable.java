package com.woodplc.cora.data;

import java.util.ArrayList;
import java.util.List;

public class ModuleVariable {

	private final String type;
	private String allocation;
	private final List<String> allocationParameters;

	
	public ModuleVariable(String text) {
		this.type = text;
		this.allocation = "";
		allocationParameters = new ArrayList<>();
	}
	
	public String getAllocation() {
		return allocation;
	}

	public void setAllocation(String allocation) {
		this.allocation = allocation;
	}

	public String getType() {
		return type;
	}

	public boolean isScalar() {
		return allocation.isEmpty();
	}
	
	

	public List<String> getAllocationParameters() {
		return allocationParameters;
	}

	public void setAllocationParameters(List<String> allocationParameters) {
		this.allocationParameters.addAll(allocationParameters);
	}

	@Override
	public String toString() {
		return "ModuleVariable [type=" + type + ", allocation=" + allocation + "]";
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((allocation == null) ? 0 : allocation.hashCode());
		result = prime * result + ((allocationParameters == null) ? 0 : allocationParameters.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ModuleVariable other = (ModuleVariable) obj;
		if (allocation == null) {
			if (other.allocation != null)
				return false;
		} else if (!allocation.equals(other.allocation))
			return false;
		if (allocationParameters == null) {
			if (other.allocationParameters != null)
				return false;
		} else if (!allocationParameters.equals(other.allocationParameters))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

}
