package com.woodplc.cora.data;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public final class Feature {
	
	private final ObservableList<String> systemASubprograms = FXCollections.observableArrayList();
	private final ObservableList<String> systemBSubprograms = FXCollections.observableArrayList();
	private final ObservableList<String> systemCSubprograms = FXCollections.observableArrayList();
	
	public Feature() {}

	public Feature(List<String> subprogramsA, 
			List<String> subprogramsB, 
			List<String> subprogramsC) {
		Objects.requireNonNull(subprogramsA);
		Objects.requireNonNull(subprogramsB);
		Objects.requireNonNull(subprogramsC);
		if (!subprogramsA.isEmpty()) this.systemASubprograms.addAll(subprogramsA);
		if (!subprogramsB.isEmpty()) this.systemBSubprograms.addAll(subprogramsB);
		if (!subprogramsC.isEmpty()) this.systemCSubprograms.addAll(subprogramsC);
	}
	
	public boolean isEmpty() {
		return this.systemASubprograms.isEmpty()
				&& this.systemBSubprograms.isEmpty()
				&& this.systemCSubprograms.isEmpty();
	}
	
	public ObservableList<String> systemASubprograms() {return systemASubprograms;}

	public ObservableList<String> systemBSubprograms() {return systemBSubprograms;}
	
	public ObservableList<String> systemCSubprograms() {return systemCSubprograms;}
	
	public List<String> readOnlySystemASubprograms() {return Collections.unmodifiableList(systemASubprograms);}
	
	public List<String> readOnlySystemBSubprograms() {return Collections.unmodifiableList(systemBSubprograms);}
	
	public List<String> readOnlySystemCSubprograms() {return Collections.unmodifiableList(systemCSubprograms);}
}