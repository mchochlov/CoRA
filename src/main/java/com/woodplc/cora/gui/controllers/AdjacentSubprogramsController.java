package com.woodplc.cora.gui.controllers;

import java.util.Objects;
import java.util.stream.Collectors;

import com.woodplc.cora.gui.model.EntityView;

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
	private final ObservableList<EntityView> callees;
	
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
	
	AdjacentSubprogramsController(String subname, ObservableList<String> systemSubprograms,
			ObservableList<EntityView> callers, ObservableList<EntityView> callees) {
		super(subname, systemSubprograms);
		this.callers = Objects.requireNonNull(callers);
		this.callees = Objects.requireNonNull(callees);
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
			systemSubprograms.addAll(selectedCallers
					.stream()
					.map(EntityView::getName)
					.collect(Collectors.toSet()));
			callers.removeAll(selectedCallers);
		}
		
		if (!selectedCallees.isEmpty()) {
			systemSubprograms.addAll(selectedCallees
					.stream()
					.map(EntityView::getName)
					.collect(Collectors.toSet()));
			callees.removeAll(selectedCallees);
		}
    }
}
