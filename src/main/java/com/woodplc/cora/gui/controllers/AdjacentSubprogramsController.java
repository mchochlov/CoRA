package com.woodplc.cora.gui.controllers;

import java.util.stream.Collectors;

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

class AdjacentSubprogramsController extends Controller {
	
	private final ObservableList<EntityView> callers;
	private final ObservableList<EntityView> callees;// = FXCollections.observableArrayList();
	
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
		super(subname, graph, systemASubprograms);
				
		this.callers = this.graph.getSubprogramCallers(subname)
				.stream()
				.filter(s -> !this.systemASubprograms.contains(s))
				.map(s -> new EntityView(this.graph.getFanOut(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));

		this.callees = this.graph.getSubprogramCallees(subname)
				.stream()
				.filter(s -> !this.systemASubprograms.contains(s))
				.map(s -> new EntityView(this.graph.getFanIn(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));
	}
	
	@FXML 
	void initialize() {
		callersLbl.setText(callersLbl.getText() + " " + subname);
		calleesLbl.setText(calleesLbl.getText() + " " + subname);
		
		callersTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		calleesTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		
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
			systemASubprograms.addAll(selectedCallers
					.stream()
					.map(EntityView::getName)
					.collect(Collectors.toSet()));
			callers.removeAll(selectedCallers);
		}
		
		if (!selectedCallees.isEmpty()) {
			systemASubprograms.addAll(selectedCallees
					.stream()
					.map(EntityView::getName)
					.collect(Collectors.toSet()));
			callees.removeAll(selectedCallees);
		}
    }
}
