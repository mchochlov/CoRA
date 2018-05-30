package com.woodplc.cora.gui.controllers;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.woodplc.cora.gui.model.EntityView;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.ir.IREngines;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;

class ClonesController extends Controller {

	private final ObservableList<EntityView> clones = FXCollections.observableArrayList();
	private final Path moduleAPath;
	private final Path moduleOtherPath;
	
	ClonesController(String subname, ObservableList<String> systemSubprograms, Path moduleAPath, Path moduleOtherPath) {
		super(subname, systemSubprograms);
		
		this.moduleAPath = Objects.requireNonNull(moduleAPath);
		this.moduleOtherPath = Objects.requireNonNull(moduleOtherPath);
	}

	@FXML
	private Label clonesLbl;
	@FXML
    private TextField queryTxtFld;
    @FXML
    private Button searchBtn;

	@FXML
	private TableView<EntityView> clonesTbl;
	@FXML
	private TableColumn<EntityView, Integer> ranlClmn;
	@FXML
	private TableColumn<EntityView, String> cloneClmn;

	@FXML
	void initialize() {
		clonesLbl.setText(clonesLbl.getText() + " " + subname);
		clonesTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

		ranlClmn.setCellValueFactory(new PropertyValueFactory<EntityView, Integer>("param"));
		cloneClmn.setCellValueFactory(new PropertyValueFactory<EntityView, String>("name"));

		clonesTbl.setItems(clones);
	}

	@FXML
	void findClones(ActionEvent event) {
		String query = queryTxtFld.getText();
		
		searchBtn.setDisable(true);
		clones.clear();

		FindClonesTask sTask = new FindClonesTask(subname, query, moduleAPath, moduleOtherPath);
		sTask.setOnSucceeded((e) -> {
			sTask.getValue().removeIf(x -> systemSubprograms.contains(x.getName()));
			clones.addAll(sTask.getValue());
			searchBtn.setDisable(false);
		});
		
		sTask.setOnFailed((e) -> {
			sTask.getException().printStackTrace();
			searchBtn.setDisable(false);
		});
		
		new Thread(sTask).start();
	}

	@FXML
	void selectSubprograms(ActionEvent event) {
		ObservableList<EntityView> selectedClones = clonesTbl.getSelectionModel().getSelectedItems();
		if (selectedClones.isEmpty()) {return;}
		
		systemSubprograms.addAll(selectedClones
				.stream()
				.map(EntityView::getName)
				.collect(Collectors.toSet()));
		clones.removeAll(selectedClones);
	}

	static class FindClonesTask extends Task<List<EntityView>> {
		
		private final String subname;
		private final String query;
		private final Path pathToA;
		private final Path pathToOtherSystem;

		FindClonesTask(String subname, String query, Path pathToA, Path pathToOtherSystem){
			this.subname = Objects.requireNonNull(subname);
			this.query = Objects.requireNonNull(query);
			this.pathToA = Objects.requireNonNull(pathToA);
			this.pathToOtherSystem = Objects.requireNonNull(pathToOtherSystem);
		}
		
		@Override
		protected List<EntityView> call() throws Exception {
			IREngine engineA = IREngines.getLuceneEngineInstance(pathToA);
			IREngine engineOther = IREngines.getLuceneEngineInstance(pathToOtherSystem);
			
			List<String> termVector = engineA.getDocumentTermVector(subname);
			final AtomicInteger counter = new AtomicInteger(0);
			return engineOther.moreLikeThis(termVector, query).stream()
					.map(res -> new EntityView(counter.incrementAndGet(), res))
					.collect(Collectors.toList());
		}
	}
}
