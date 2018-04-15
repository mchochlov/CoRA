package com.woodplc.cora.gui.controllers;

import java.util.Objects;
import java.util.Set;

import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.gui.model.EntityView;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

class AdjacentSubprogramsController {
	
	private final String subname;
	private final SDGraph graph;
	private final ObservableList<String> systemASubprograms;
	private final ObservableList<EntityView> callers = FXCollections.observableArrayList();
	private final ObservableList<EntityView> callees = FXCollections.observableArrayList();
	
	@FXML
    private Label callersLbl;

    @FXML
    private TableView<EntityView> callersTbl;
    @FXML
    private TableColumn<EntityView, Integer> fanInClmn;
    @FXML
    private TableColumn<EntityView, String> callerClmn;

    @FXML
    private Label calleesLbl;

    @FXML
    private TableView<EntityView> calleesTbl;
    @FXML
    private TableColumn<EntityView, Integer> fanOutClmn;
    @FXML
    private TableColumn<EntityView, String> calleeClmn;
	
	AdjacentSubprogramsController(String subname, SDGraph graph, ObservableList<String> systemASubprograms) {
		this.subname = Objects.requireNonNull(subname);
		this.graph = Objects.requireNonNull(graph);
		this.systemASubprograms = Objects.requireNonNull(systemASubprograms);
	}
	
	@FXML 
	void initialize() {
		callersLbl.setText(callersLbl.getText() + " " + subname);
		calleesLbl.setText(calleesLbl.getText() + " " + subname);
		
		callersTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		calleesTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		
		if (graph.containsSubprogram(subname)) {
			Set<String> callers = graph.getSubprogramCallers(subname);
			callers.removeAll(systemASubprograms);
			for (String caller : callers) {
				this.callers.add(new EntityView(graph.getFanOut(caller), caller));
			}
			Set<String> callees = graph.getSubprogramCallees(subname);
			callees.removeAll(systemASubprograms);
			for (String callee : callees) {
				this.callees.add(new EntityView(graph.getFanIn(callee), callee));
			}
	
		}
		
		fanInClmn.setCellValueFactory(new PropertyValueFactory<EntityView, Integer>("param"));
		callerClmn.setCellValueFactory(new PropertyValueFactory<EntityView, String>("name"));
		fanOutClmn.setCellValueFactory(new PropertyValueFactory<EntityView, Integer>("param"));
		calleeClmn.setCellValueFactory(new PropertyValueFactory<EntityView, String>("name"));
		
		callersTbl.setItems(callers);
		callersTbl.getSortOrder().add(fanInClmn);
		calleesTbl.setItems(callees);
		calleesTbl.getSortOrder().add(fanOutClmn);
	}

	@FXML
    void selectAdjacentSubprograms(ActionEvent event) {
		ObservableList<EntityView> selectedCallers = callersTbl.getSelectionModel().getSelectedItems();
		ObservableList<EntityView> selectedCallees = calleesTbl.getSelectionModel().getSelectedItems();
		if (selectedCallers.isEmpty() && selectedCallees.isEmpty()) {return;}
		
		if (!selectedCallers.isEmpty()) {
			selectedCallers.forEach(x -> {
				systemASubprograms.add(x.getName());
			});
			callers.removeAll(selectedCallers);
		}
		
		if (!selectedCallees.isEmpty()) {
			selectedCallees.forEach(x -> {
				systemASubprograms.add(x.getName());
			});
			callees.removeAll(selectedCallees);
		}
    }
}
