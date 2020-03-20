package com.woodplc.cora.gui.controllers;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.woodplc.cora.app.Main;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.ProgramEntryNotFoundException;
import com.woodplc.cora.gui.model.SearchEntryView;
import com.woodplc.cora.ir.IREngine;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;

class ClonesController extends Controller {

	private final ObservableList<SearchEntryView> clones = FXCollections.observableArrayList();
	private final IREngine engineA;
	private final IREngine engineOther;
	private final Alert illegalStateAlert = new Alert(AlertType.ERROR, Main.getResources().getString("subprogram_not_found"));
	private final String pathA;
	private final String path;
	private final Feature feature;
	
	ClonesController(String subname, ObservableList<String> systemSubprograms, IREngine engineA, IREngine engineOther, 
			String pathA, String path, Feature feature) {
		super(subname, systemSubprograms);
		
		this.engineA = Objects.requireNonNull(engineA);
		this.engineOther = Objects.requireNonNull(engineOther);
		this.pathA = Objects.requireNonNull(pathA);
		this.path = Objects.requireNonNull(path);
		this.feature = Objects.requireNonNull(feature);
	}

	@FXML
	private Label clonesLbl;
	@FXML
    private TextField queryTxtFld;
    @FXML
    private Button searchBtn;

	@FXML
	private TableView<SearchEntryView> clonesTbl;
	@FXML
	private TableColumn<SearchEntryView, Integer> ranlClmn;
	@FXML
	private TableColumn<SearchEntryView, Integer> ranlClmnScore;
	@FXML
	private TableColumn<SearchEntryView, String> cloneClmn;

	@FXML
	void initialize() {
		clonesLbl.setText(clonesLbl.getText() + " " + subname);
		clonesTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

		ranlClmn.setCellValueFactory(new PropertyValueFactory<SearchEntryView, Integer>("param"));
		ranlClmnScore.setCellValueFactory(new PropertyValueFactory<SearchEntryView, Integer>("score"));
		cloneClmn.setCellValueFactory(new PropertyValueFactory<SearchEntryView, String>("name"));

		clonesTbl.setItems(clones);
	}

	@FXML
	void findClones(ActionEvent event) {
		String query = queryTxtFld.getText();
		
		searchBtn.setDisable(true);
		clones.clear();

		FindClonesTask sTask = new FindClonesTask(subname, query, engineA, engineOther);
		sTask.setOnSucceeded((e) -> {
			sTask.getValue().removeIf(x -> systemSubprograms.contains(x.getName()));
			clones.addAll(sTask.getValue());
			searchBtn.setDisable(false);
		});
		
		sTask.setOnFailed((e) -> {
			sTask.getException().printStackTrace();
			if (sTask.getException() instanceof ProgramEntryNotFoundException) {
				illegalStateAlert.showAndWait();
			}
			searchBtn.setDisable(false);
		});
		
		new Thread(sTask).start();
	}

	@FXML
	void selectSubprograms(ActionEvent event) {
		ObservableList<SearchEntryView> selectedClones = clonesTbl.getSelectionModel().getSelectedItems();
		if (selectedClones.isEmpty()) {return;}
		Set<String> clones = selectedClones
				.stream()
				.map(SearchEntryView::getName)
				.collect(Collectors.toSet());
		systemSubprograms.addAll(clones);
		this.clones.removeAll(selectedClones);
		feature.addRefactoringCasesFromClones(pathA, path, subname, clones);
	}

	static class FindClonesTask extends Task<List<SearchEntryView>> {
		
		private final String subname;
		private final String query;
		private final IREngine engineA;
		private final IREngine engineOther;

		FindClonesTask(String subname, String query, IREngine engineA, IREngine engineOther){
			this.subname = Objects.requireNonNull(subname);
			this.query = Objects.requireNonNull(query);
			this.engineA = Objects.requireNonNull(engineA);
			this.engineOther = Objects.requireNonNull(engineOther);
		}
		
		@Override
		protected List<SearchEntryView> call() throws ProgramEntryNotFoundException {
			List<String> termVector = this.engineA.getDocumentTermVector(subname);
			final AtomicInteger counter = new AtomicInteger(0);
			return this.engineOther.moreLikeThis(termVector, query).stream()
					.map(res -> new SearchEntryView(counter.incrementAndGet(), res))
					.collect(Collectors.toList());
		}
	}
}
