package com.woodplc.cora.gui.controllers;

import java.io.File;
import java.io.IOException;
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
import java.util.stream.Stream;

import com.woodplc.cora.app.Main;
import com.woodplc.cora.app.Main.Resource;
import com.woodplc.cora.data.ApplicationState;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.FeatureView;
import com.woodplc.cora.data.ImmutableModule;
import com.woodplc.cora.data.ModuleContainer;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.gui.model.CallDependencyView;
import com.woodplc.cora.gui.model.SearchEntryView;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.storage.JSONUtils;
import com.woodplc.cora.storage.Repositories;
import com.woodplc.cora.utils.CSVUtils;

import javafx.application.Platform;
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
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class CoRAMainController {
		
	private static final double DOUBLE_ZERO = 0;
	private final DirectoryChooser dirChooser = new DirectoryChooser();
	private final FileChooser exportChooser = new FileChooser();
	private String lastSearchQuery = null;
	private File lastKnownDir = null;
	private final Alert graphNotFoundAlert = new Alert(AlertType.ERROR, Main.getResources().getString("graph_not_found"));
	private final Alert multipleSelectionAlert = new Alert(AlertType.ERROR, Main.getResources().getString("multiselect"));
	private final Alert featureIsEmpty = new Alert(AlertType.INFORMATION, Main.getResources().getString("feature_empty"));
	private final Alert illegalStateAlert = new Alert(AlertType.ERROR, Main.getResources().getString("subprogram_not_found"));
	
	private final ObservableList<SearchEntryView> searchResults = FXCollections.observableArrayList();
	private final FilteredList<SearchEntryView> filteredSearchResults = new FilteredList<>(searchResults);
	
	private ModuleContainer moduleA = ModuleContainer.empty();
	private ModuleContainer moduleB = ModuleContainer.empty();
	private ModuleContainer moduleC = ModuleContainer.empty();
	
	private Feature feature = new Feature();
	
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
	private TableColumn<SearchEntryView, Integer> systemAClmnId;
	@FXML
	private TableColumn<SearchEntryView, Integer> systemAClmnScore;
	@FXML
	private TableColumn<SearchEntryView, String> systemAClmnName;

	@FXML
	private TableView<SearchEntryView> systemASearchResultTbl;
	
	@FXML
    private Label systemABottomLbl;
    @FXML
    private Label systemBBottomLbl;
    @FXML
    private Label systemCBottomLbl;
    
    @FXML
    private Label selectedALbl;
    @FXML
    private Label locALbl;
    @FXML
    private Label selectedBLbl;
    @FXML
    private Label locBLbl;
    @FXML
    private Label selectedCLbl;
    @FXML
    private Label locCLbl;
	
	private enum ProgressBarColor{
		BLUE("-fx-accent: blue"),
		RED("-fx-accent: red"),
		GREEN("-fx-accent: green");
			
		private final String style;
		
		private ProgressBarColor(String style) {this.style = style;}
	}
	
	private enum ExportType{JSON, CSV};

	public CoRAMainController() {}

	public CoRAMainController(ApplicationState state) {
		Objects.requireNonNull(state);
		this.lastKnownDir = state.getLastKnownDir() == null ? null : new File(state.getLastKnownDir());
		this.lastSearchQuery = state.getSearchQuery();
		this.searchResults.addAll(state.getSearchResults());
		this.moduleA = state.getModuleA();
		this.moduleB = state.getModuleB();
		this.moduleC = state.getModuleC();
		this.feature = state.getFeature();
		filteredSearchResults.setPredicate(r -> !feature.systemASubprograms().contains(r.getName()));
	}

	@FXML
	void initialize() {
		initializeModule(moduleA, systemADirFld, systemALbl, systemABottomLbl, systemAParseBtn, systemAProgressBar);
		initializeModule(moduleB, systemBDirFld, systemBLbl, systemBBottomLbl, systemBParseBtn, systemBProgressBar);
		initializeModule(moduleC, systemCDirFld, systemCLbl, systemCBottomLbl, systemCParseBtn, systemCProgressBar);
		
		searchTxtFld.setText(lastSearchQuery);
		
		feature.systemASubprograms().addListener((ListChangeListener.Change<? extends String> x) -> {
			while(x.next()) {
				if (x.wasAdded() || x.wasRemoved()) {
					filteredSearchResults.setPredicate(r -> !feature.systemASubprograms().contains(r.getName()));
				}
			}
			setSelectedText(selectedALbl, systemASubprogramList);
		});
		
		feature.systemBSubprograms().addListener((ListChangeListener.Change<? extends String> x) -> {
			setSelectedText(selectedBLbl, systemBSubprogramList);
		});
		
		feature.systemCSubprograms().addListener((ListChangeListener.Change<? extends String> x) -> {
			setSelectedText(selectedCLbl, systemCSubprogramList);
		});
		
		systemASubprogramList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemBSubprogramList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemCSubprogramList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemASubprogramList.setItems(feature.systemASubprograms());
		setSelectedText(selectedALbl, systemASubprogramList);
		systemBSubprogramList.setItems(feature.systemBSubprograms());
		setSelectedText(selectedBLbl, systemBSubprogramList);
		systemCSubprogramList.setItems(feature.systemCSubprograms());
		setSelectedText(selectedCLbl, systemCSubprogramList);
		systemASubprogramList.getSelectionModel().getSelectedItems().addListener((ListChangeListener.Change<? extends String> x) -> {
			setSelectedText(selectedALbl, systemASubprogramList);
		});
		
		systemBSubprogramList.getSelectionModel().getSelectedItems().addListener((ListChangeListener.Change<? extends String> x) -> {
			setSelectedText(selectedBLbl, systemBSubprogramList);
		});
		
		systemCSubprogramList.getSelectionModel().getSelectedItems().addListener((ListChangeListener.Change<? extends String> x) -> {
			setSelectedText(selectedCLbl, systemCSubprogramList);
		});

		
		systemAClmnId.setCellValueFactory(new PropertyValueFactory<SearchEntryView, Integer>("param"));
		systemAClmnScore.setCellValueFactory(new PropertyValueFactory<SearchEntryView, Integer>("score"));
		systemAClmnName.setCellValueFactory(new PropertyValueFactory<SearchEntryView, String>("name"));
		systemASearchResultTbl.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		systemASearchResultTbl.setItems(filteredSearchResults);

	}
	
	private void setSelectedText(Label label, ListView<String> lv) {
		label.setText(lv.getSelectionModel().getSelectedItems().size() + "/" 
				+ lv.getItems().size() + " "
				+ Main.getResources().getString("selected"));
	}
	
	private void initializeModule(ModuleContainer module, 
			TextField dirField, 
			Label topLabel, 
			Label bottomLabel,
			Button parseBtn,
			ProgressBar progressBar) 
	{
		if (module != null) {
			Path path = module.getPath();
			if (path != null) {
				dirField.setText(path.toString());
				topLabel.setText(path.getFileName().toString());
				bottomLabel.setText(path.getFileName().toString());
			}
			if (module.getCheckSum() != null) {
				parse(module, parseBtn, progressBar);
			}
		}
	}
	
	@FXML
	void openSystemABrowseDlg(ActionEvent event) {
		open(moduleA, systemAProgressBar, Main.getResources().getString("select_system_a"), systemADirFld, systemALbl, systemABottomLbl);
	}
	
	@FXML
    void openSystemBBrowseDlg(ActionEvent event) {
		open(moduleB, systemBProgressBar, Main.getResources().getString("select_system_b"), systemBDirFld, systemBLbl, systemBBottomLbl);
    }

    @FXML
    void openSystemCBrowseDlg(ActionEvent event) {
		open(moduleC, systemCProgressBar, Main.getResources().getString("select_system_c"), systemCDirFld, systemCLbl, systemCBottomLbl);
    }

	private void open(ModuleContainer module, ProgressBar pBar, String title, TextField txtField, Label label, Label bLabel) {
		dirChooser.setTitle(title);
		dirChooser.setInitialDirectory(lastKnownDir);
		File selectedDir = dirChooser.showDialog(txtField.getScene().getWindow());
		if (selectedDir != null) {
			if (selectedDir.toPath().getFileName() == null) throw new IllegalArgumentException();
			txtField.setText(selectedDir.getAbsolutePath());
			module.setPath(selectedDir.toPath());
			pBar.progressProperty().set(DOUBLE_ZERO);
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
		Path path = module.getPath();
		if (path != null) {
			parseBtn.setDisable(true);
			progressBar.setStyle(ProgressBarColor.BLUE.style);
			
			Task<ImmutableModule> pTask = new Task<ImmutableModule>() {

				@Override
				protected ImmutableModule call() throws Exception {
					String checkSum = module.getCheckSum();
					if (checkSum == null) {
						checkSum = Repositories.checkSumForPath(path);
						module.setCheckSum(checkSum);	
					}
					return Repositories.getInstance().retrieve(checkSum, path, this::updateProgress);
				}

				@Override
				protected void failed() {
					getException().printStackTrace();
					updateState(null, ProgressBarColor.RED);
				}

				@Override
				protected void succeeded() {
					updateState(getValue(), ProgressBarColor.GREEN);
				}
				
				private void updateState(ImmutableModule im, ProgressBarColor color) {
					module.setModule(im);
					parseBtn.setDisable(false);
					progressBar.progressProperty().unbind();
					progressBar.progressProperty().set(Double.MAX_VALUE);
					progressBar.setStyle(color.style);
				}			
			};

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
	void findClonesSystemB(ActionEvent event) throws IOException {
		if (moduleA == null || moduleA.getModule() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		loadStage(Resource.CLONES_FXML, "clones_b_title", moduleB, feature.systemBSubprograms());
	}

	@FXML
    void findClonesSystemC(ActionEvent event) throws IOException {
		if (moduleA == null || moduleA.getModule() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		loadStage(Resource.CLONES_FXML, "clones_c_title", moduleC, feature.systemCSubprograms());
    }
	
	@FXML
	void systemAAdjacentSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.ADJACENT_FXML, "adjacent_sub_title", moduleA, feature.systemASubprograms());
	}

	@FXML
	void systemAMarkSubprogram(ActionEvent event) {
		ObservableList<SearchEntryView> selectedItems = systemASearchResultTbl.getSelectionModel().getSelectedItems();
		if (selectedItems.isEmpty()) {return;}
		feature.systemASubprograms().addAll(selectedItems
				.stream()
				.map(SearchEntryView::getName)
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
		
		if (moduleA.getModule() == null || module.getModule() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		if (!moduleA.getModule().getGraph().containsSubprogram(selectedSubprogram)) {
			illegalStateAlert.showAndWait();
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
		ObservableList<CallDependencyView> callers = moduleA.getModule().getGraph().getSubprogramCallers(selectedSubprogram)
				.stream()
				.filter(s -> !fSubprograms.contains(s))
				.map(s -> new CallDependencyView(moduleA.getModule().getGraph().getFanOut(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));

		ObservableList<CallDependencyView> callees = moduleA.getModule().getGraph().getSubprogramCallees(selectedSubprogram)
				.stream()
				.filter(s -> !fSubprograms.contains(s))
				.map(s -> new CallDependencyView(moduleA.getModule().getGraph().getFanIn(s), s))
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
	
	@FXML
    void quit(ActionEvent event) {Platform.exit();}
	
	@FXML
    void about(ActionEvent event) {
		new Alert(AlertType.INFORMATION, Main.getResources()
				.getString("cora_about")).showAndWait();
    }
	
	@FXML
	void exportToCsv(ActionEvent event) {exportFeature(ExportType.CSV);}

	@FXML
	void exportToJson(ActionEvent event) {exportFeature(ExportType.JSON);}
		
    private void exportFeature(ExportType exportType) {
    	if (feature == null || feature.isEmpty()) {
    		featureIsEmpty.showAndWait();
    		return;
    	}
    	
    	if ((!feature.systemASubprograms().isEmpty() && (moduleA == null || moduleA.getModule() == null))
    			|| (!feature.systemBSubprograms().isEmpty() && (moduleB == null || moduleB.getModule() == null))
    			|| (!feature.systemCSubprograms().isEmpty() && (moduleC == null || moduleC.getModule() == null))) {
    		graphNotFoundAlert.showAndWait();
    		return;
    	}
    	
    	exportChooser.setTitle(Main.getResources().getString("export_feature_title"));
    	ExtensionFilter ef = null;
    	switch(exportType) {
    	case JSON: ef = new ExtensionFilter("JSON Files", "*.json"); break;
    	case CSV: ef = new ExtensionFilter("CSV Files", "*.csv"); break;
    	default:
    		throw new IllegalArgumentException();
    	}
    	
    	exportChooser.getExtensionFilters().addAll(ef);
    	File selectedFile = exportChooser.showSaveDialog(systemALbl.getScene().getWindow());
    	if (selectedFile != null) {
    		final Path exportPath = selectedFile.toPath();
    		Task<Void> exportTask = new Task<Void>() {

				@Override
				protected Void call() throws Exception {
					final Set<String> selectedSystemA = new HashSet<>(feature.readOnlySystemASubprograms());
					final Set<String> selectedSystemB = new HashSet<>(feature.readOnlySystemBSubprograms());
					final Set<String> selectedSystemC = new HashSet<>(feature.readOnlySystemCSubprograms());
					
					Set<SubProgram> subprogramsSystemA = null;
					Set<SubProgram> subprogramsSystemB = null;
					Set<SubProgram> subprogramsSystemC = null;
					if (!selectedSystemA.isEmpty()) {
						subprogramsSystemA = moduleA.getModule().getGraph().subprograms().stream()
								.filter(sub -> selectedSystemA.contains(sub.name()))
								.collect(Collectors.toSet());
					} else {
						subprogramsSystemA = new HashSet<>();
					}
					
					if (!selectedSystemB.isEmpty()) {
						subprogramsSystemB = moduleB.getModule().getGraph().subprograms().stream()
								.filter(sub -> selectedSystemB.contains(sub.name()))
								.collect(Collectors.toSet());
					} else {
						subprogramsSystemB = new HashSet<>();
					}
					
					if (!selectedSystemC.isEmpty()) {
						subprogramsSystemC = moduleC.getModule().getGraph().subprograms().stream()
								.filter(sub -> selectedSystemC.contains(sub.name()))
								.collect(Collectors.toSet());
					} else {
						subprogramsSystemC = new HashSet<>();
					}
					FeatureView fv = new FeatureView(subprogramsSystemA, subprogramsSystemB, subprogramsSystemC);
					switch(exportType) {
			    	case JSON: JSONUtils.exportFeatureToJson(exportPath, fv); break;
			    	case CSV: CSVUtils.exportFeatureToCsv(exportPath, subprogramsSystemA, subprogramsSystemB, subprogramsSystemC); break;
			    	default:
			    		throw new IllegalArgumentException();
			    	}
					
					return null;
				}
				
				@Override
				protected void failed() {
					getException().printStackTrace();
				}
    			
    		};
    		
    		new Thread(exportTask).start();
    	}
    }
	
    @FXML
    void locSystemA(ActionEvent event) {
    	calculateLoc(locALbl, feature.systemASubprograms(), moduleA.getModule().getGraph());
    }

    @FXML
    void locSystemB(ActionEvent event) {
    	calculateLoc(locBLbl, feature.systemBSubprograms(), moduleB.getModule().getGraph());
    }

    @FXML
    void locSystemC(ActionEvent event) {
    	calculateLoc(locCLbl, feature.systemCSubprograms(), moduleC.getModule().getGraph());
    }
    
    private void calculateLoc(Label label, ObservableList<String> subprogramNames, SDGraph graph) {
    	CalculateLocTask cTask = new CalculateLocTask(subprogramNames, graph);
		cTask.setOnSucceeded((e) -> {
			label.setText(cTask.getValue() + " " + Main.getResources().getString("loc"));
		});
		
		cTask.setOnFailed((e) -> {
			cTask.getException().printStackTrace();
			label.setText("N/A " + Main.getResources().getString("loc"));
		});
		
		new Thread(cTask).start();
    }
    
    private static class CalculateLocTask extends Task<Long> {
    	private final Set<String> subprogramNames;
    	private final SDGraph graph;
    	
    	CalculateLocTask(ObservableList<String> subprogramNames, SDGraph graph) {
			this.subprogramNames = new HashSet<>(subprogramNames);
			if (this.subprogramNames.size() != subprogramNames.size()) throw new IllegalArgumentException();
			this.graph = graph;
		}

		@Override
		protected Long call() throws Exception {
			Set<SubProgram> subprograms = graph.subprograms()
					.stream()
					.filter(sub -> subprogramNames.contains(sub.name()))
					.collect(Collectors.toSet());
			long loc = 0;
			
			for (SubProgram subprogram : subprograms) {
				try(Stream<String> lines = Files.lines(subprogram.path())){
					loc += 
							lines.limit(subprogram.endLine())
							.skip(subprogram.startLine())
							.filter(s -> !s.trim().startsWith("!") && !s.startsWith("c") && !s.startsWith("C") && !s.trim().isEmpty())
							//.forEach(System.out::println);
							.count();
				}
			}
			
			return loc;
		}
    }
    
	private static class SearchTask extends Task<List<SearchEntryView>> {
		
		private final String query;
		private final ImmutableModule module;

		SearchTask(String query, ImmutableModule module){ 
			this.query = Objects.requireNonNull(query);
			this.module = Objects.requireNonNull(module);
		}
		
		@Override
		protected List<SearchEntryView> call() throws Exception {
			IREngine engine = module.getEngine();
			final AtomicInteger counter = new AtomicInteger(0);
			return engine.search(query).stream()
					.map(res -> new SearchEntryView(counter.incrementAndGet(), res))
					.collect(Collectors.toList());
		}
		
	}

	public ApplicationState getApplicationState() {
		String searchQuery = searchTxtFld == null ? null : searchTxtFld.getText();
		return new ApplicationState(lastKnownDir, searchQuery, searchResults, 
				moduleA, moduleB, moduleC, feature);
	}
}
