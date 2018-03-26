package com.woodplc.cora.gui.model;

import java.util.Objects;

import com.woodplc.cora.data.SDGraph;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class DataModel {
	

	private static DataModel instance = null;
	
	private SDGraph systemAGraph;
	
	public void setSystemAGraph(SDGraph systemAGraph) {
		this.systemAGraph = Objects.requireNonNull(systemAGraph);
	}

	private ObservableList<String> flex3Subprograms = FXCollections.observableArrayList();
	private ObservableList<String> flex3Callers = FXCollections.observableArrayList("cable_solution", "distributed_load");
	private ObservableList<String> flex3Callees = FXCollections.observableArrayList("cable_solution", "distributed_load");
	private ObservableList<Subprogram> data =
	        FXCollections.observableArrayList(
	            new Subprogram("1", "calc drag factor"),
	            new Subprogram("2", "calculate drag and inertia factors"),
	            new Subprogram("3", "read input file main"),
	            new Subprogram("4", "default buoyancy"),
	            new Subprogram("5", "distributed buoyancy")
	        );
	
	private DataModel() {}
	
	public static DataModel instance() {
		if (instance == null) {
			instance = new DataModel();
		}
		return instance;
	}
	
	public ObservableList<String> getFlex3Subprograms() {
		return flex3Subprograms;
	}

	public ObservableList<String> getFlex3Callers() {
		return flex3Callers;
	}

	public ObservableList<String> getFlex3Callees() {
		return flex3Callees;
	}

	public ObservableList<Subprogram> getData() {
		return data;
	}
	
	public static class Subprogram {
		private SimpleStringProperty id;
		private SimpleStringProperty name;

		private Subprogram(String id, String name) {
			this.id = new SimpleStringProperty(id);
			this.name = new SimpleStringProperty(name);
		}

		public String getId() {
			return id.get();
		}

		public void setId(String id) {
			this.id.set(id);
		}

		public String getName() {
			return name.get();
		}

		public void setName(String name) {
			this.name.set(name);
		}
	}

	
	
}
