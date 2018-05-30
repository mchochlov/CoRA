package com.woodplc.cora.gui.controllers;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

class VariableControlledController extends Controller {

	private final Map<String, Set<String>> variables;

	@FXML
    private Label varLbl;
	
	@FXML
    private TreeView<String> varTreeView;
	
	VariableControlledController(String subname, ObservableList<String> systemSubprograms, Map<String, Set<String>> variables) {
		super(subname, systemSubprograms);
			
		this.variables = Objects.requireNonNull(variables);
	}

	@FXML 
	void initialize() {
		varLbl.setText(subname + " " + varLbl.getText());
				
		TreeItem<String> root = new TreeItem<>();
		root.setExpanded(true);
		for (Entry<String, Set<String>> entry : variables.entrySet()) {
			TreeItem<String> treeEntry = new TreeItem<>(entry.getKey());
			entry.getValue().forEach(x -> treeEntry.getChildren().add(new TreeItem<>(x)));
			root.getChildren().add(treeEntry);
		}
		 
		varTreeView.setRoot(root);
		varTreeView.setShowRoot(false);
		varTreeView.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
	}
	
	@FXML
    void selectSubprograms(ActionEvent event) {
		ObservableList<TreeItem<String>> selectedItems = FXCollections.observableArrayList(varTreeView.getSelectionModel().getSelectedItems());
		
		for (TreeItem<String> item : selectedItems) {
			TreeItem<String> parent = item.getParent();
			TreeItem<String> root = parent.getParent();
			if (root != null) {
				parent.getChildren().remove(item);
				if (parent.getChildren().isEmpty()) {
					root.getChildren().remove(parent);
				}
				this.systemSubprograms.add(item.getValue());
			}
		}
    }
}
