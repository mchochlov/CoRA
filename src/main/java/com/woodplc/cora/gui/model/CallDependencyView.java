package com.woodplc.cora.gui.model;

import java.util.Objects;

import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;

public class CallDependencyView {
	private final SimpleIntegerProperty numCalls;
	private final SimpleStringProperty name;
	
	public CallDependencyView(int param, String name) {
		if (param < 0 || name == null || name.isEmpty()) {
			throw new IllegalArgumentException();
		}
		this.numCalls = new SimpleIntegerProperty(param);
		this.name = new SimpleStringProperty(name);
	}

	public int getNumCalls() {return numCalls.get();}

	public String getName() {return name.get();}
	
	public void setNumCalls(int param) {this.numCalls.set(param);}
	public void setName(String name) {this.name.set(name);}
	
	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof CallDependencyView)) return false;
		CallDependencyView cdv = (CallDependencyView) o;
		return this.numCalls.intValue() == cdv.numCalls.intValue()
				&& this.name.getValue().equals(cdv.name.getValue());
	}

	@Override
	public int hashCode() {
		return Objects.hash(numCalls.intValue(), name.getValue());
	}
}
