package com.woodplc.cora.gui.controllers;

import java.util.Objects;

import javafx.collections.ObservableList;

abstract class Controller {
	
	protected final String subname;
	protected final ObservableList<String> systemSubprograms;
	
	Controller(String subname, ObservableList<String> systemASubprograms) {
		this.subname = Objects.requireNonNull(subname);
		this.systemSubprograms = Objects.requireNonNull(systemASubprograms);
	}
}
