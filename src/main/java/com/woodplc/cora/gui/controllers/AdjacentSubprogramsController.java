package com.woodplc.cora.gui.controllers;

import com.woodplc.cora.gui.model.DataModel;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.ListView;
import javafx.scene.control.SelectionMode;

public class AdjacentSubprogramsController {
	
	private DataModel model;
	
	@FXML
	private ListView<String> flex3CallersListView;// = new ListView<>(flex3Callers);


	@FXML
	void initialize() {
		this.model = DataModel.instance();
		flex3CallersListView.setItems(model.getFlex3Callers());
		flex3CallersListView.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
	}

	@FXML
    void selectAdjacentSubprograms(ActionEvent event) {
		model.getFlex3Subprograms().addAll(flex3CallersListView
				.getSelectionModel().getSelectedItems());
    }
}
