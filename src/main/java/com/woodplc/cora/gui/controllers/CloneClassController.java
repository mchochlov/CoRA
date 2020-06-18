package com.woodplc.cora.gui.controllers;

import java.util.List;
import java.util.Set;

import org.fxmisc.richtext.StyleClassedTextArea;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.util.Pair;

public class CloneClassController {

	@FXML
    private ListView<?> ccListview;

    @FXML
    private CheckBox useSFcheckBox;

    @FXML
    private StyleClassedTextArea codeMergeArea;

    @FXML
    private Button mergeBtn;

    @FXML
    private Label mergeProgressLbl;

    public CloneClassController(List<Pair<String,String>> value) {
		// TODO Auto-generated constructor stub
	}

	@FXML
    void mergeClones(ActionEvent event) {

    }
}
