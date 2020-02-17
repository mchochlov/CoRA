package com.woodplc.cora.gui.controllers;

import java.nio.file.Path;
import java.util.Objects;
import java.util.stream.Collectors;

import com.woodplc.cora.data.Feature;
import com.woodplc.cora.gui.model.CallDependencyView;

import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

class AdjacentSubprogramsController extends Controller {
	
	private final ObservableList<CallDependencyView> callers;
	private final ObservableList<CallDependencyView> callees;
	private final String path;
	private final Feature feature;
	
	@FXML
    private Label callersLbl;

    @FXML
    private TableView<CallDependencyView> callersTbl;
    @FXML
    private TableColumn<CallDependencyView, Integer> fanInClmn;
    @FXML
    private TableColumn<CallDependencyView, String> callerClmn;

    @FXML
    private Label calleesLbl;

    @FXML
    private TableView<CallDependencyView> calleesTbl;
    @FXML
    private TableColumn<CallDependencyView, Integer> fanOutClmn;
    @FXML
    private TableColumn<CallDependencyView, String> calleeClmn;
	
	AdjacentSubprogramsController(String subname, ObservableList<String> systemSubprograms,
			ObservableList<CallDependencyView> callers, ObservableList<CallDependencyView> callees, String path, Feature feature) {
		super(subname, systemSubprograms);
		this.callers = Objects.requireNonNull(callers);
		this.callees = Objects.requireNonNull(callees);
		this.path = Objects.requireNonNull(path);
		this.feature = Objects.requireNonNull(feature);
	}
	
	@FXML 
	void initialize() {
		callersLbl.setText(callersLbl.getText() + " " + subname);
		calleesLbl.setText(calleesLbl.getText() + " " + subname);
		
		callersTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		calleesTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		
		fanInClmn.setCellValueFactory(new PropertyValueFactory<CallDependencyView, Integer>("numCalls"));
		callerClmn.setCellValueFactory(new PropertyValueFactory<CallDependencyView, String>("name"));
		fanOutClmn.setCellValueFactory(new PropertyValueFactory<CallDependencyView, Integer>("numCalls"));
		calleeClmn.setCellValueFactory(new PropertyValueFactory<CallDependencyView, String>("name"));
		
		callersTbl.setItems(callers);
		callersTbl.getSortOrder().add(fanInClmn);
		calleesTbl.setItems(callees);
		calleesTbl.getSortOrder().add(fanOutClmn);
	}

	@FXML
    void selectAdjacentSubprograms(ActionEvent event) {
		ObservableList<CallDependencyView> selectedCallers = callersTbl.getSelectionModel().getSelectedItems();
		ObservableList<CallDependencyView> selectedCallees = calleesTbl.getSelectionModel().getSelectedItems();
		if (selectedCallers.isEmpty() && selectedCallees.isEmpty()) {return;}
		
		if (!selectedCallers.isEmpty()) {
			systemSubprograms.addAll(selectedCallers
					.stream()
					.map(CallDependencyView::getName)
					.collect(Collectors.toSet()));
			feature.addRefactoringCasesFromAdj(this.path, selectedCallers);
			callers.removeAll(selectedCallers);
		}
		
		if (!selectedCallees.isEmpty()) {
			systemSubprograms.addAll(selectedCallees
					.stream()
					.map(CallDependencyView::getName)
					.collect(Collectors.toSet()));
			feature.addRefactoringCasesFromAdj(this.path, selectedCallees);
			callees.removeAll(selectedCallees);
		}
    }
}
