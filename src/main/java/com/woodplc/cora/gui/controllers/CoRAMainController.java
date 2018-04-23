package com.woodplc.cora.gui.controllers;

import static java.util.stream.Collectors.toSet;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Set;
import java.util.stream.StreamSupport;

import com.woodplc.cora.app.Main;
import com.woodplc.cora.app.Main.Resource;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.gui.model.EntityView;
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
	private final FilteredList<EntityView> subprogramSearchResults = new FilteredList<>(
	        FXCollections.observableArrayList(
	            new EntityView(1, "calc_drag_factor"),
	            new EntityView(2, "calculate_drag_and_inertia_factors"),
	            new EntityView(3, "read_input_file_main"),
	            new EntityView(4, "default_buoyancy"),
	            new EntityView(5, "distributed_buoyancy")
	        ));
	
	private final Module moduleA = new Module();
	private final Module moduleB = new Module();
	private final Module moduleC = new Module();
	
	private final Feature feature = new Feature();
	
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
	private TableColumn<EntityView, Integer> flex3ClmnId;

	@FXML
	private TableColumn<EntityView, String> flex3ClmnName;

	@FXML
	private TableView<EntityView> flex3Tbl;
	
	

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


	private void open(Module module, String title, TextField txtField, Label label) {
		dirChooser.setTitle(title);
		File selectedDir = dirChooser.showDialog(txtField.getScene().getWindow());
		if (selectedDir != null) {
			txtField.setText(selectedDir.getAbsolutePath());
			module.path = selectedDir.toPath();
			label.setText(module.path.getFileName().toString());
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
	
	private void parse(Module module, Button parseBtn, ProgressBar progressBar) {
		if (module.path != null) {
			parseBtn.setDisable(true);
			ParseTask pTask = new ParseTask(module.path);
			pTask.setOnSucceeded((event) -> {
				module.graph = pTask.getValue();
				parseBtn.setDisable(false);
			});
			
			pTask.setOnFailed((event) -> {
				module.graph = Graphs.getSDGraphInstance();
				parseBtn.setDisable(false);
			});
			
			progressBar.progressProperty().bind(pTask.progressProperty());
			new Thread(pTask).start();
		}
	}

	@FXML
	void flex3Search(ActionEvent event) {
/*		flex3ClmnId.setCellValueFactory(new PropertyValueFactory<Subprogram, String>("id"));
		flex3ClmnName.setCellValueFactory(new PropertyValueFactory<Subprogram, String>("name"));
*/		// flex3Tbl.getColumns().addAll(flex3ClmnId, flex3ClmnName);
	}

	@FXML
	void initialize() {
		feature.systemASubprograms.addListener((ListChangeListener.Change<? extends String> x) -> {
			while(x.next()) {
				if (x.wasAdded() || x.wasRemoved()) {
					subprogramSearchResults.setPredicate(r -> !feature.systemASubprograms.contains(r.getName()));
				}
			}
		});
		flex3List.setItems(feature.systemASubprograms);
		flex3ClmnId.setCellValueFactory(new PropertyValueFactory<EntityView, Integer>("param"));
		flex3ClmnName.setCellValueFactory(new PropertyValueFactory<EntityView, String>("name"));
		flex3Tbl.setItems(subprogramSearchResults);

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
		EntityView selectedItem = flex3Tbl.getSelectionModel().getSelectedItem();
		feature.systemASubprograms.add(selectedItem.getName());
	}

	@FXML
	void flex3VarControlledSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.VAR_FXML, "var_controlled_title");
	}
	
	private void loadStage(Resource resource, String title) throws IOException {
		String selectedSubprogram = flex3List.getSelectionModel().getSelectedItem();
		if (selectedSubprogram == null || selectedSubprogram.isEmpty()) {return;}
		
		if (moduleA.graph == null) {
			new Alert(AlertType.ERROR, Main.getResources().getString("graph_not_found")).showAndWait();
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
				moduleA.graph,
				feature.systemASubprograms);
		case VAR_FXML:
		return new VariableControlledController(
				selectedSubprogram,
				moduleA.graph,
				feature.systemASubprograms);
		default:
			throw new IllegalArgumentException();
		}
		
	}
	
	private static class Module {
		private Path path;
		private SDGraph graph;

		private Module() {}
	}
	
	private static class Feature {
		private final ObservableList<String> systemASubprograms = FXCollections.observableArrayList();
		private final ObservableList<String> systemBSubprograms = FXCollections.observableArrayList();
		private final ObservableList<String> systemCSubprograms = FXCollections.observableArrayList();
		
		private Feature() {}
	}
	
	static class ParseTask extends Task<SDGraph> {
		
		private final Path path;

		ParseTask(Path path){
			this.path = Objects.requireNonNull(path);
		}
		
		@Override
		protected SDGraph call() throws Exception {
			SDGraph graph = Graphs.getSDGraphInstance();
			Parser parser = Parsers.fortranParser();
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
				return graph;
		    }
		}
		
	}
}
