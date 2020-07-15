package com.woodplc.cora.data;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.google.gson.annotations.Expose;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public final class Feature {

	@Expose
	private final ObservableList<String> systemASubprograms = FXCollections.observableArrayList();
	@Expose
	private final ObservableList<String> systemBSubprograms = FXCollections.observableArrayList();
	@Expose
	private final ObservableList<String> systemCSubprograms = FXCollections.observableArrayList();
	
	@Override
	public int hashCode() {
		return Objects.hash(systemASubprograms, systemBSubprograms, systemCSubprograms);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			 return true;
		}
		if (!(obj instanceof Feature)) {
			return false;
		}
		Feature other = (Feature) obj;
		return this.systemASubprograms.equals(other.systemASubprograms) &&
				this.systemBSubprograms.equals(other.systemBSubprograms) &&
				this.systemCSubprograms.equals(other.systemCSubprograms);
	}
	
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