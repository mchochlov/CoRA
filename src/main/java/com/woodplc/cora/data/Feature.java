package com.woodplc.cora.data;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public final class Feature {
	
	private final ObservableList<String> systemASubprograms;
	private final ObservableList<String> systemBSubprograms;
	private final ObservableList<String> systemCSubprograms;
	
	private Feature() {
		systemASubprograms = FXCollections.observableArrayList();
		systemBSubprograms = FXCollections.observableArrayList();
		systemCSubprograms = FXCollections.observableArrayList();
	}
	
	public static Feature newBlankFeature() {
		return new Feature();
	}
	
	public ObservableList<String> systemASubprograms(){return systemASubprograms;}

	public ObservableList<String> systemBSubprograms() {return systemBSubprograms;}
	
	public ObservableList<String> systemCSubprograms() {return systemCSubprograms;}
}