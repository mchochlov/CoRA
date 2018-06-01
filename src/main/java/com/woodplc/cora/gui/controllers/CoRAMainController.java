package com.woodplc.cora.gui.controllers;

import static java.util.stream.Collectors.toSet;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.woodplc.cora.app.Main;
import com.woodplc.cora.app.Main.Resource;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.ImmutableModule;
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
import javafx.scene.layout.Pane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class CoRAMainController {
		
	private final DirectoryChooser dirChooser = new DirectoryChooser();
	private File lastKnownDir = null;
	private final Alert graphNotFoundAlert = new Alert(AlertType.ERROR, Main.getResources().getString("graph_not_found"));
	private final Alert multipleSelectionAlert = new Alert(AlertType.ERROR, Main.getResources().getString("multiselect"));
	
	private final ObservableList<EntityView> searchResults = FXCollections.observableArrayList();
	private final FilteredList<EntityView> filteredSearchResults = new FilteredList<>(searchResults);
	
	private final ModuleContainer moduleA = new ModuleContainer();
	private final ModuleContainer moduleB = new ModuleContainer();
	private final ModuleContainer moduleC = new ModuleContainer();
	
	private final Feature feature = Feature.newBlankFeature();
	
	@FXML
	private Label systemALbl;
	@FXML
	private TextField systemADirFld;
	@FXML
	private Button systemABrowseBtn;
	@FXML
	private Button systemAParseBtn;
	@FXML
	private ProgressBar systemAProgressBar;
	
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
	private ListView<String> systemASubprogramList;
    @FXML
    private ListView<String> systemBSubprogramList;
    @FXML
    private ListView<String> systemCSubprogramList;
	
	@FXML
    private TextField searchTxtFld;
	
	@FXML
    private Button systemASearchBtn;
	
	@FXML
	private TableColumn<EntityView, Integer> systemAClmnId;

	@FXML
	private TableColumn<EntityView, String> systemAClmnName;

	@FXML
	private TableView<EntityView> systemASearchResultTbl;
	
	@FXML
    private Label systemABottomLbl;
    @FXML
    private Label systemBBottomLbl;
    @FXML
    private Label systemCBottomLbl;

	private static class ModuleContainer {
		private Path path;
		private ImmutableModule module;
		
		public Path getPath() {	return path;}
		public void setPath(Path path) {this.path = path;}
		public ImmutableModule getModule() {return module;}
		public void setModule(ImmutableModule module) {	this.module = module;}
	}
	
	private enum ProgressBarColor{
		BLUE("-fx-accent: blue"),
		RED("-fx-accent: red"),
		GREEN("-fx-accent: green");
		
		
		private final String style;
		
		private ProgressBarColor(String style) {this.style = style;}
	}

	@FXML
	void openSystemABrowseDlg(ActionEvent event) {
		open(moduleA, Main.getResources().getString("select_system_a"), systemADirFld, systemALbl, systemABottomLbl);
	}
	
	@FXML
    void openSystemBBrowseDlg(ActionEvent event) {
		open(moduleB, Main.getResources().getString("select_system_b"), systemBDirFld, systemBLbl, systemBBottomLbl);
    }

    @FXML
    void openSystemCBrowseDlg(ActionEvent event) {
		open(moduleC, Main.getResources().getString("select_system_c"), systemCDirFld, systemCLbl, systemCBottomLbl);
    }


	private void open(ModuleContainer module, String title, TextField txtField, Label label, Label bLabel) {
		dirChooser.setTitle(title);
		dirChooser.setInitialDirectory(lastKnownDir);
		File selectedDir = dirChooser.showDialog(txtField.getScene().getWindow());
		if (selectedDir != null) {
			txtField.setText(selectedDir.getAbsolutePath());
			module.setPath(selectedDir.toPath());
			label.setText(module.getPath().getFileName().toString());
			bLabel.setText(module.getPath().getFileName().toString());
			lastKnownDir = selectedDir;
		}
	}
	
	@FXML
	void parseSystemA(ActionEvent event) {
		parse(moduleA, systemAParseBtn, systemAProgressBar);
	}
	
	@FXML
    void parseSystemB(ActionEvent event) {
		parse(moduleB, systemBParseBtn, systemBProgressBar);
    }

    @FXML
    void parseSystemC(ActionEvent event) {
		parse(moduleC, systemCParseBtn, systemCProgressBar);
    }
	
	private void parse(ModuleContainer module, Button parseBtn, ProgressBar progressBar) {
		if (module.getPath() != null) {
			parseBtn.setDisable(true);
			progressBar.setStyle(ProgressBarColor.BLUE.style);
			ParseTask pTask = new ParseTask(module.getPath());
			pTask.setOnSucceeded((event) -> {
				module.setModule(pTask.getValue());
				parseBtn.setDisable(false);
				progressBar.setStyle(ProgressBarColor.GREEN.style);
			});
			
			pTask.setOnFailed((event) -> {
				pTask.getException().printStackTrace();
				module.setModule(null);
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
	void systemASearch(ActionEvent event) {
		String query = searchTxtFld.getText();
		if (query == null || query.isEmpty()) { return;}
		
		if (moduleA.getModule() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		systemASearchBtn.setDisable(true);
		searchResults.clear();

		SearchTask sTask = new SearchTask(query, moduleA.getModule());
		sTask.setOnSucceeded((e) -> {
			searchResults.addAll(sTask.getValue());
			systemASearchBtn.setDisable(false);
		});
		
		sTask.setOnFailed((e) -> {
			sTask.getException().printStackTrace();
			systemASearchBtn.setDisable(false);
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
		systemASubprogramList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemBSubprogramList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemCSubprogramList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemASubprogramList.setItems(feature.systemASubprograms());
		systemBSubprogramList.setItems(feature.systemBSubprograms());
		systemCSubprogramList.setItems(feature.systemCSubprograms());
		
		systemAClmnId.setCellValueFactory(new PropertyValueFactory<EntityView, Integer>("param"));
		systemAClmnName.setCellValueFactory(new PropertyValueFactory<EntityView, String>("name"));
		systemASearchResultTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemASearchResultTbl.setItems(filteredSearchResults);

	}

	@FXML
	void findClonesSystemB(ActionEvent event) throws IOException {
		loadStage(Resource.CLONES_FXML, "clones_b_title", moduleB, feature.systemBSubprograms());
	}

	@FXML
    void findClonesSystemC(ActionEvent event) throws IOException {
		loadStage(Resource.CLONES_FXML, "clones_c_title", moduleC, feature.systemCSubprograms());
    }
	
	@FXML
	void systemAAdjacentSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.ADJACENT_FXML, "adjacent_sub_title", moduleA, feature.systemASubprograms());
	}

	@FXML
	void systemAMarkSubprogram(ActionEvent event) {
		ObservableList<EntityView> selectedItems = systemASearchResultTbl.getSelectionModel().getSelectedItems();
		if (selectedItems.isEmpty()) {return;}
		feature.systemASubprograms().addAll(selectedItems
				.stream()
				.map(EntityView::getName)
				.collect(Collectors.toList()));
	}

	@FXML
	void systemAVarControlledSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.VAR_FXML, "var_controlled_title", moduleA, feature.systemASubprograms());
	}
	
	@FXML
    void removeItemsSystemA(ActionEvent event) {
		removeItems(systemASubprogramList, feature.systemASubprograms());
    }

    @FXML
    void removeItemsSystemB(ActionEvent event) {
		removeItems(systemBSubprogramList, feature.systemBSubprograms());
    }

    @FXML
    void removeItemsSystemC(ActionEvent event) {
		removeItems(systemCSubprogramList, feature.systemCSubprograms());
    }
    
    private void removeItems(ListView<String> subprogramList, ObservableList<String> list) {
    	ObservableList<String> selectedItems = subprogramList.getSelectionModel().getSelectedItems();
		if (selectedItems.isEmpty()) {return;}
		list.removeAll(selectedItems);
    }
	
	private void loadStage(Resource resource, String title, 
			ModuleContainer module, ObservableList<String> fSubprograms) throws IOException {
		if (systemASubprogramList.getSelectionModel().getSelectedItems().size() > 1) {
			multipleSelectionAlert.showAndWait();
			return;
		}
		
		String selectedSubprogram = systemASubprogramList.getSelectionModel().getSelectedItem();
		if (selectedSubprogram == null || selectedSubprogram.isEmpty()) {return;}
		
		if (module.getModule() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		FXMLLoader loader = new FXMLLoader(getClass().getResource(resource.path()), Main.getResources());
		Controller controller = getControllerForResource(resource, selectedSubprogram, fSubprograms, module);
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
	
	private Controller getControllerForResource(Resource resource, String selectedSubprogram,
			ObservableList<String> fSubprograms, ModuleContainer module) {
		switch(resource) {
		case ADJACENT_FXML:
		return constructASController(selectedSubprogram, fSubprograms);
		case VAR_FXML:
		return constructVCController(selectedSubprogram, fSubprograms);
		case CLONES_FXML:
		return new ClonesController(
				selectedSubprogram,
				fSubprograms,
				moduleA.getModule().getEngine(),
				module.getModule().getEngine());
		default:
			throw new IllegalArgumentException();
		}
		
	}
	
	private AdjacentSubprogramsController constructASController(String selectedSubprogram,
			ObservableList<String> fSubprograms) {
		ObservableList<EntityView> callers = moduleA.getModule().getGraph().getSubprogramCallers(selectedSubprogram)
				.stream()
				.filter(s -> !fSubprograms.contains(s))
				.map(s -> new EntityView(moduleA.getModule().getGraph().getFanOut(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));

		ObservableList<EntityView> callees = moduleA.getModule().getGraph().getSubprogramCallees(selectedSubprogram)
				.stream()
				.filter(s -> !fSubprograms.contains(s))
				.map(s -> new EntityView(moduleA.getModule().getGraph().getFanIn(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));
		
		return new AdjacentSubprogramsController(selectedSubprogram, fSubprograms, callers, callees);
	}
	
	private VariableControlledController constructVCController(String selectedSubprogram,
			ObservableList<String> fSubprograms) {
		final Set<String> duplicates = new HashSet<>();
		Map<String, Set<String>> variables = moduleA.getModule().getGraph().getVariablesAndCallees(selectedSubprogram)
			.entrySet()
			.stream()
			.map(x -> {
				x.getValue().removeAll(fSubprograms);
				x.getValue().removeAll(duplicates);
				duplicates.addAll(x.getValue());
				return x;
			})
			.filter(x -> !x.getValue().isEmpty())
			.sorted((x, y) -> Integer.compare(x.getValue().size(), y.getValue().size()))
			.collect(Collectors.toMap(x -> x.getKey(), x -> x.getValue(), (x, y) -> x, LinkedHashMap::new));
		
		return new VariableControlledController(selectedSubprogram, fSubprograms, variables);
	}
	
	static class ParseTask extends Task<ImmutableModule> {
		
		private final Path path;

		ParseTask(Path path){
			this.path = Objects.requireNonNull(path);
		}
		
		@Override
		protected ImmutableModule call() throws Exception {
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
				return ImmutableModule.nonPersistent(graph, engine);
		    }
		}
		
	}
	
	static class SearchTask extends Task<List<EntityView>> {
		
		private final String query;
		private final ImmutableModule module;

		SearchTask(String query, ImmutableModule module){ 
			this.query = Objects.requireNonNull(query);
			this.module = Objects.requireNonNull(module);
		}
		
		@Override
		protected List<EntityView> call() throws Exception {
			IREngine engine = module.getEngine();
			final AtomicInteger counter = new AtomicInteger(0);
			return engine.search(query).stream()
					.map(res -> new EntityView(counter.incrementAndGet(), res))
					.collect(Collectors.toList());
		}
		
	}
}
