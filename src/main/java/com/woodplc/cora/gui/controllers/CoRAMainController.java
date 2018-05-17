package com.woodplc.cora.gui.controllers;

import static java.util.stream.Collectors.toSet;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.google.common.hash.Hasher;
import com.google.common.hash.Hashing;
import com.woodplc.cora.app.Main;
import com.woodplc.cora.app.Main.Resource;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.MutableModule;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.gui.model.EntityView;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.ir.IREngines;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;

import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class CoRAMainController {
		
	private final DirectoryChooser dirChooser = new DirectoryChooser();
	private final Alert graphNotFoundAlert = new Alert(AlertType.ERROR, Main.getResources().getString("graph_not_found"));
	
	private final ObservableList<EntityView> searchResults = FXCollections.observableArrayList();
	private final FilteredList<EntityView> filteredSearchResults = new FilteredList<>(searchResults);
	
	private final MutableModule moduleA = new MutableModule();
	private final MutableModule moduleB = new MutableModule();
	private final MutableModule moduleC = new MutableModule();
	
	private final Feature feature = Feature.newBlankFeature();
	
	@FXML
	private Label systemALbl;
	@FXML
	private TextField flex3DirFld;
	@FXML
	private Button flex3BrowseBtn;
	@FXML
	private Button flex3ParseBtn;
	@FXML
	private ProgressBar flex3ProgressBar;
	
	@FXML
    private Label systemBLbl;
    @FXML
    private TextField systemBDirFld;
    @FXML
    private Button systemBBrowseBtn;
    @FXML
    private Button systemBParseBtn;
    @FXML
    private ProgressBar systemBProgressBar;

    @FXML
    private Label systemCLbl;
    @FXML
    private TextField systemCDirFld;
    @FXML
    private Button systemCBrowseBtn;
    @FXML
    private Button systemCParseBtn;
    @FXML
    private ProgressBar systemCProgressBar;
	
	
	@FXML
	private ListView<String> flex3List;
	
	@FXML
    private TextField searchTxtFld;
	
	@FXML
    private Button flex3SearchBtn;
	
	@FXML
	private TableColumn<EntityView, Integer> flex3ClmnId;

	@FXML
	private TableColumn<EntityView, String> flex3ClmnName;

	@FXML
	private TableView<EntityView> flex3Tbl;
	
	private enum ProgressBarColor{
		BLUE("-fx-accent: blue"),
		RED("-fx-accent: red"),
		GREEN("-fx-accent: green");
		
		
		private final String style;
		
		private ProgressBarColor(String style) {this.style = style;}
	}

	@FXML
	void openFlex3BrowseDlg(ActionEvent event) {
		open(moduleA, Main.getResources().getString("select_system_a"), flex3DirFld, systemALbl);
	}
	
	@FXML
    void openSystemBBrowseDlg(ActionEvent event) {
		open(moduleB, Main.getResources().getString("select_system_b"), systemBDirFld, systemBLbl);
    }

    @FXML
    void openSystemCBrowseDlg(ActionEvent event) {
		open(moduleC, Main.getResources().getString("select_system_c"), systemCDirFld, systemCLbl);
    }


	private void open(MutableModule module, String title, TextField txtField, Label label) {
		dirChooser.setTitle(title);
		File selectedDir = dirChooser.showDialog(txtField.getScene().getWindow());
		if (selectedDir != null) {
			txtField.setText(selectedDir.getAbsolutePath());
			module.setPath(selectedDir.toPath());
			label.setText(module.getPath().getFileName().toString());
		}
	}
	
	@FXML
	void parseFlex3(ActionEvent event) {
		parse(moduleA, flex3ParseBtn, flex3ProgressBar);
	}
	
	@FXML
    void parseSystemB(ActionEvent event) {
		parse(moduleB, systemBParseBtn, systemBProgressBar);
    }

    @FXML
    void parseSystemC(ActionEvent event) {
		parse(moduleC, systemCParseBtn, systemCProgressBar);
    }
	
	private void parse(MutableModule module, Button parseBtn, ProgressBar progressBar) {
		if (module.getPath() != null) {
			parseBtn.setDisable(true);
			progressBar.setStyle(ProgressBarColor.BLUE.style);
			ParseTask pTask = new ParseTask(module.getPath());
			pTask.setOnSucceeded((event) -> {
				module.setGraph(pTask.getValue());
				parseBtn.setDisable(false);
				progressBar.setStyle(ProgressBarColor.GREEN.style);
			});
			
			pTask.setOnFailed((event) -> {
				pTask.getException().printStackTrace();
				module.setGraph(Graphs.getSDGraphInstance());
				parseBtn.setDisable(false);
				progressBar.progressProperty().unbind();
				progressBar.progressProperty().set(Double.MAX_VALUE);
				progressBar.setStyle(ProgressBarColor.RED.style);
			});
			
			progressBar.progressProperty().bind(pTask.progressProperty());
			new Thread(pTask).start();
		}
	}

	@FXML
	void flex3Search(ActionEvent event) {
		String query = searchTxtFld.getText();
		if (query == null || query.isEmpty()) { return;}
		
		if (moduleA.getGraph() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		flex3SearchBtn.setDisable(true);
		searchResults.clear();

		SearchTask sTask = new SearchTask(query, moduleA.getPath());
		sTask.setOnSucceeded((e) -> {
			searchResults.addAll(sTask.getValue());
			flex3SearchBtn.setDisable(false);
		});
		
		sTask.setOnFailed((e) -> {
			sTask.getException().printStackTrace();
			flex3SearchBtn.setDisable(false);
		});
		
		new Thread(sTask).start();
	}

	@FXML
	void initialize() {
		feature.systemASubprograms().addListener((ListChangeListener.Change<? extends String> x) -> {
			while(x.next()) {
				if (x.wasAdded() || x.wasRemoved()) {
					filteredSearchResults.setPredicate(r -> !feature.systemASubprograms().contains(r.getName()));
				}
			}
		});
		flex3List.setItems(feature.systemASubprograms());
		flex3ClmnId.setCellValueFactory(new PropertyValueFactory<EntityView, Integer>("param"));
		flex3ClmnName.setCellValueFactory(new PropertyValueFactory<EntityView, String>("name"));
		flex3Tbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		flex3Tbl.setItems(filteredSearchResults);

	}

	@FXML
	void findClonesDeepRiser(ActionEvent event) throws IOException {
		Stage stage = new Stage();
		AnchorPane root = (AnchorPane) FXMLLoader.load(getClass().getResource("/com/woodplc/cora/gui/fxml/ClonesDeepRiser.fxml"));
		Scene scene = new Scene(root, 550, 450);
		scene.getStylesheets().add(getClass().getResource(Resource.CSS.path()).toExternalForm());
		stage.setScene(scene);
		stage.show();
	}

	@FXML
	void flex3AdjacentSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.ADJACENT_FXML, "adjacent_sub_title");
	}

	@FXML
	void flex3MarkSubprogram(ActionEvent event) {
		ObservableList<EntityView> selectedItems = flex3Tbl.getSelectionModel().getSelectedItems();
		if (selectedItems.isEmpty()) {return;}
		feature.systemASubprograms().addAll(selectedItems
				.stream()
				.map(EntityView::getName)
				.collect(Collectors.toList()));
	}

	@FXML
	void flex3VarControlledSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.VAR_FXML, "var_controlled_title");
	}
	
	private void loadStage(Resource resource, String title) throws IOException {
		String selectedSubprogram = flex3List.getSelectionModel().getSelectedItem();
		if (selectedSubprogram == null || selectedSubprogram.isEmpty()) {return;}
		
		if (moduleA.getGraph() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		FXMLLoader loader = new FXMLLoader(getClass().getResource(resource.path()), Main.getResources());
		Controller controller = getControllerForResource(resource, selectedSubprogram);
		loader.setController(controller);
		Pane root = (Pane) loader.load();
		Scene scene = new Scene(root);
		scene.getStylesheets().add(getClass().getResource(Resource.CSS.path()).toExternalForm());
		
		Stage stage = new Stage();
		stage.setScene(scene);
		stage.setTitle(Main.getResources().getString(title));
		stage.initModality(Modality.APPLICATION_MODAL);
		stage.showAndWait();
	}
	
	private Controller getControllerForResource(Resource resource, String selectedSubprogram) {
		switch(resource) {
		case ADJACENT_FXML:
		return new AdjacentSubprogramsController(
				selectedSubprogram,
				moduleA.getGraph(),
				feature.systemASubprograms());
		case VAR_FXML:
		return new VariableControlledController(
				selectedSubprogram,
				moduleA.getGraph(),
				feature.systemASubprograms());
		default:
			throw new IllegalArgumentException();
		}
		
	}
	
	static class ParseTask extends Task<SDGraph> {
		
		private final Path path;

		ParseTask(Path path){
			this.path = Objects.requireNonNull(path);
		}
		
		@Override
		protected SDGraph call() throws Exception {
			IREngine engine = IREngines.getLuceneEngineInstance(path);
			SDGraph graph = Graphs.getSDGraphInstance();
			Parser parser = Parsers.indexableFortranParser(engine);
			try (DirectoryStream<Path> stream = Files.newDirectoryStream(
					path, Parsers.fortranFileExtensions()
					)
				) 
			{
				long totalFiles = 0, parsedFiles = 0;
				Set<Path> paths = StreamSupport.stream(stream.spliterator(), false).collect(toSet());
				totalFiles = paths.size();
				
				for (Path entry: paths) {
					graph.merge(parser.parse(entry));
					updateProgress(++parsedFiles, totalFiles);
				}
				engine.save();
				return graph;
		    }
		}
		
	}
}
