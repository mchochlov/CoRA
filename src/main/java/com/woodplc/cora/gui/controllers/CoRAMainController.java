package com.woodplc.cora.gui.controllers;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.SetMultimap;
import com.google.gson.annotations.Expose;
import com.woodplc.cora.app.Main;
import com.woodplc.cora.app.Main.Resource;
import com.woodplc.cora.data.Feature;
import com.woodplc.cora.data.FeatureView;
import com.woodplc.cora.data.ImmutableModule;
import com.woodplc.cora.data.ModuleContainer;
import com.woodplc.cora.data.ProgramEntryNotFoundException;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.gui.model.CallDependencyView;
import com.woodplc.cora.gui.model.RefactoringCaseView;
import com.woodplc.cora.gui.model.SearchEntryView;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.refactoring.Refactoring;
import com.woodplc.cora.refactoring.Refactorings;
import com.woodplc.cora.storage.JSONUtils;
import com.woodplc.cora.storage.Repositories;
import com.woodplc.cora.utils.CSVUtils;
import com.woodplc.cora.utils.Utils;

import javafx.application.Platform;
import javafx.beans.property.SimpleIntegerProperty;
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
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.util.Callback;
import javafx.util.Pair;

public class CoRAMainController {
	    
	final Logger logger = LoggerFactory.getLogger(CoRAMainController.class);
	
	private static final double DOUBLE_ZERO = 0;
	private final DirectoryChooser dirChooser = new DirectoryChooser();
	private final FileChooser exportChooser = new FileChooser();
	@Expose
	private String lastSearchQuery = null;
	@Expose
	private File lastKnownDir = null;
	private final Alert graphNotFoundAlert = new Alert(AlertType.ERROR, Main.getResources().getString("graph_not_found"));
	private final Alert multipleSelectionAlert = new Alert(AlertType.ERROR, Main.getResources().getString("multiselect"));
	private final Alert featureIsEmpty = new Alert(AlertType.INFORMATION, Main.getResources().getString("feature_empty"));
	private final Alert successfulRefactoring = new Alert(AlertType.INFORMATION, Main.getResources().getString("refactor_success"));
	private final Alert failRefactoring = new Alert(AlertType.WARNING, Main.getResources().getString("refactor_success"));
	private final Alert illegalStateAlert = new Alert(AlertType.ERROR, Main.getResources().getString("subprogram_not_found"));
	private final Alert aboutAlert = new Alert(AlertType.INFORMATION, Main.getResources().getString("cora_about"));
	private final Alert dirChanged = new Alert(AlertType.INFORMATION);
	
	@Expose
	private final ObservableList<SearchEntryView> searchResults = FXCollections.observableArrayList();
	private FilteredList<SearchEntryView> filteredSearchResults;
	
	@Expose
	private ModuleContainer moduleA = ModuleContainer.empty();
	@Expose
	private ModuleContainer moduleB = ModuleContainer.empty();
	@Expose
	private ModuleContainer moduleC = ModuleContainer.empty();
	@Expose
	private ModuleContainer cafModule = ModuleContainer.empty();
	
	@Expose
	private Feature feature = new Feature();
	
	public static WatchService watchService;
	private final Map<WatchKey, Path> watchPaths = new HashMap<>();
	final static Queue<Path> processingQueue = new ConcurrentLinkedQueue<>();
	
	@Expose
	SetMultimap<Pair<String, String>, Pair<String, String>> cloneGroups = SetMultimapBuilder.hashKeys().linkedHashSetValues().build();
	
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
    private Label cafLbl;
    @FXML
    private TextField cafDirFld;
    @FXML
    private Button cafBrowseBtn;
    @FXML
    private Button cafParseBtn;
    @FXML
    private ProgressBar cafProgressBar;
	
	
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
    
    @FXML
    private MenuBar menuBar;
	@FXML
    private Menu fileMenu;
	@FXML
	private Menu analysisMenu;
	@FXML
	private Menu helpMenu;
	private Stack<String> commandStack = new Stack<>();
	@FXML
	private BorderPane borderPane;
	
	@FXML
    private Label statusLabel;

	@FXML
	private Label jobsNumberLabel;
	private final AtomicInteger atomicCounter = new AtomicInteger(0);
	private final SimpleIntegerProperty counter = new SimpleIntegerProperty(0);

    // TAB 2
    @FXML
	private TableColumn<RefactoringCaseView, String> caseClmnName;
    @FXML
   	private TableColumn<RefactoringCaseView, String> flClmnName;
    @FXML
   	private TableColumn<RefactoringCaseView, String> drClmnName;
    @FXML
   	private TableColumn<RefactoringCaseView, String> plClmnName;
    @FXML
	private TableView<RefactoringCaseView> refactoringCaseTbl;
	
	private enum ProgressBarColor{
		BLUE("-fx-accent: blue"),
		RED("-fx-accent: red"),
		GREEN("-fx-accent: green");
			
		private final String style;
		
		private ProgressBarColor(String style) {this.style = style;}
	}
	
	private enum ExportType{JSON, CSV};

	public CoRAMainController() {}
	
	public CoRAMainController(File lastKnownDir, String searchQuery, ObservableList<SearchEntryView> searchResults,
			ModuleContainer mc1, ModuleContainer mc2, ModuleContainer mc3, ModuleContainer caf, Feature feature, SetMultimap<Pair<String,String>,Pair<String,String>> cloneGroups2) {
		this.lastKnownDir = lastKnownDir;
		this.lastSearchQuery = searchQuery;
		this.searchResults.addAll(searchResults);
		this.moduleA = mc1;
		this.moduleB = mc2;
		this.moduleC = mc3;
		this.cafModule = caf;
		this.feature = feature;
		this.cloneGroups.putAll(cloneGroups);
	}

	public static CoRAMainController fromValues(File lastKnownDir, String searchQuery,
			ObservableList<SearchEntryView> searchResults, ModuleContainer mc1, ModuleContainer mc2,
			ModuleContainer mc3, ModuleContainer caf, Feature feature, SetMultimap<Pair<String,String>,Pair<String,String>> cloneGroups) {
		return new CoRAMainController(lastKnownDir, searchQuery, searchResults, mc1, mc2, mc3, caf, feature, cloneGroups);
	}

	@FXML
	void initialize() {     
		
		filteredSearchResults = new FilteredList<>(searchResults);
		filteredSearchResults.setPredicate(r -> !feature.systemASubprograms().contains(r.getName()));
		jobsNumberLabel.textProperty().bind(counter.asString());
		
		try {
			watchService = FileSystems.getDefault().newWatchService();
			Main.getWatchServicePoolInstance().submit(() -> 
				{
					try {
						for (;;) {
				            WatchKey key = watchService.take();
				            Path dir = watchPaths.get(key);
				            if (dir == null) {
				            	logger.error("WatchKey not recognized!!");
				                continue;
				            }
				            Thread.sleep(50);
				            key.pollEvents();
	
				            if (!CoRAMainController.processingQueue.isEmpty() && CoRAMainController.processingQueue.peek().equals(dir)) {
								CoRAMainController.processingQueue.poll();
							} else {
					            Platform.runLater(new Runnable() {
					                 @Override 
					                 public void run() {
					                	 if (!dirChanged.isShowing()) {
						                	 dirChanged.setContentText(dir.getFileName() + " system source code has changed. Refresh model.");
									         dirChanged.showAndWait();
					                	 }
					                 }
					            });
							}			          
				            if (!key.reset()) {
			                    break;
				            }
				        } 
					}	catch(InterruptedException ie) {
						logger.info("Watch service thread interrupted.");
			        	return;
					}
					
				});
		} catch (IOException e1) {
			logger.error("Failed to initialize watch service.");
		}
		
		if (SystemUtils.IS_OS_LINUX) {
			
			graphNotFoundAlert.initModality(Modality.NONE);
			multipleSelectionAlert.initModality(Modality.NONE);
			successfulRefactoring.initModality(Modality.NONE);
			failRefactoring.initModality(Modality.NONE);
			featureIsEmpty.initModality(Modality.NONE);
			illegalStateAlert.initModality(Modality.NONE);
			aboutAlert.initModality(Modality.NONE);
			dirChanged.initModality(Modality.NONE);
			
			fileMenu.setOnHiding(e -> commandStack.push("file"));
			
			analysisMenu.setOnHiding(e -> commandStack.push("analysis"));
			
			helpMenu.setOnHiding(e -> commandStack.push("help"));
			
		    menuBar.addEventFilter(MouseEvent.MOUSE_CLICKED, e -> {
		    		
		    	if (!commandStack.isEmpty()) {
		    		String command = commandStack.peek();
					if (command.equals("file")) {
						if (commandStack.size() > 1) {
							commandStack.clear();
							fileMenu.hide();
							//System.out.println("file hide " + commandStack);
						} else if (commandStack.size() == 1) {
							fileMenu.show();
							//System.out.println("file show " + commandStack);
						}
					}
					
					if (command.equals("analysis")) {
						if (commandStack.size() > 1) {
							commandStack.clear();
							analysisMenu.hide();
							//System.out.println("analysis hide " + commandStack);

						} else if (commandStack.size() == 1) {
							analysisMenu.show();
							//System.out.println("analysis show " + commandStack);
						}
					}
					
					if (command.equals("help")) {
						if (commandStack.size() > 1) {
							commandStack.clear();
							helpMenu.hide();
							//System.out.println("help hide " + commandStack);
						} else if (commandStack.size() == 1) {
							helpMenu.show();
							//System.out.println("help show " + commandStack);
						}
					}

				}
			
	    	});
		}

	    initializeModule(moduleA, systemADirFld, systemALbl, systemABottomLbl, systemAParseBtn, systemAProgressBar);
		initializeModule(moduleB, systemBDirFld, systemBLbl, systemBBottomLbl, systemBParseBtn, systemBProgressBar);
		initializeModule(moduleC, systemCDirFld, systemCLbl, systemCBottomLbl, systemCParseBtn, systemCProgressBar);
		initializeModule(cafModule, cafDirFld, cafLbl, null, cafParseBtn, cafProgressBar);
		
		searchTxtFld.setText(lastSearchQuery);
		systemASubprogramList.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
		     @Override 
		     public ListCell<String> call(ListView<String> list) {
		         return new SubprogramCell(moduleA);
		     }
		 });
		
		systemBSubprogramList.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
		     @Override 
		     public ListCell<String> call(ListView<String> list) {
		         return new SubprogramCell(moduleB);
		     }
		 });

		systemCSubprogramList.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
		     @Override 
		     public ListCell<String> call(ListView<String> list) {
		         return new SubprogramCell(moduleC);
		     }
		 });
		
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
		
		caseClmnName.setCellValueFactory(new PropertyValueFactory<RefactoringCaseView, String>("name"));
		flClmnName.setCellValueFactory(new PropertyValueFactory<RefactoringCaseView, String>("flName"));
		drClmnName.setCellValueFactory(new PropertyValueFactory<RefactoringCaseView, String>("drName"));
		plClmnName.setCellValueFactory(new PropertyValueFactory<RefactoringCaseView, String>("plName"));
		refactoringCaseTbl.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);

	}
	
	private class SubprogramCell extends ListCell<String> {

		private final ModuleContainer module;
	    public SubprogramCell(ModuleContainer module) { this.module = module; }
	       
	    @Override 
	    protected void updateItem(String item, boolean empty) {
	        super.updateItem(item, empty);
	        setText(item);
	        if (module == null || module.getModule() == null) return;
	        
	        Set<String> unreferencedSubprograms;
	        Set<String> externalSubprograms;
			try {
				unreferencedSubprograms = module.getModule().getGraph().getUnreferencedSubprograms();
				setTextFill(unreferencedSubprograms.contains(item) ? Color.RED : Color.BLACK);	
				externalSubprograms = module.getModule().getGraph().getExternalSubprograms();
				setTextFill(externalSubprograms.contains(item) ? Color.GREEN : Color.BLACK);
			} catch (ProgramEntryNotFoundException e) {
				logger.error("Program entry point not found for module [{}]: unreferenced subprograms cannot be detected.", 
						module.getPath().getFileName().toString());
			}
	        
	    }
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
				if (bottomLabel != null) {
					bottomLabel.setText(path.getFileName().toString());					
				}
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

    @FXML
    void openCafBrowseDlg(ActionEvent event) {
		open(cafModule, cafProgressBar, Main.getResources().getString("select_caf"), cafDirFld, cafLbl, null);
    }
    
	private void open(ModuleContainer module, ProgressBar pBar, String title, TextField txtField, Label label, Label bLabel) {
		dirChooser.setTitle(title);
		dirChooser.setInitialDirectory(lastKnownDir);
		File selectedDir = SystemUtils.IS_OS_LINUX ? dirChooser.showDialog(null) : dirChooser.showDialog(txtField.getScene().getWindow());
		if (selectedDir != null) {
			if (selectedDir.toPath().getFileName() == null) throw new IllegalArgumentException();
			txtField.setText(selectedDir.getAbsolutePath());
			module.setPath(selectedDir.toPath());
			pBar.progressProperty().set(DOUBLE_ZERO);
			label.setText(module.getPath().getFileName().toString());
			if (bLabel != null) {
				bLabel.setText(module.getPath().getFileName().toString());
			}
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
    
    @FXML
    void parseCaf(ActionEvent event) {
		parse(cafModule, cafParseBtn, cafProgressBar);
    }
	
	private void parse(ModuleContainer module, Button parseBtn, ProgressBar progressBar) {
		Path path = module.getPath();
		if (path != null) {
			parseBtn.setDisable(true);
			progressBar.setStyle(ProgressBarColor.BLUE.style);
			
			Task<ImmutableModule> pTask = new Task<ImmutableModule>() {

				@Override
				protected ImmutableModule call() throws Exception {
					String loadedCheckSum = module.getCheckSum();
					String currentCheckSum = Repositories.checkSumForPath(path);
					if (loadedCheckSum == null || !loadedCheckSum.equals(currentCheckSum)) {
						module.setCheckSum(currentCheckSum);	
						updateCloneGroups(loadedCheckSum, currentCheckSum);
					}
					return Repositories.getInstance().retrieve(module.getCheckSum(), path, this::updateProgress);
				}

				@Override
				protected void failed() {
					getException().printStackTrace();
					updateState(null, ProgressBarColor.RED);
				}

				@Override
				protected void succeeded() {
					updateState(getValue(), ProgressBarColor.GREEN);
					systemASubprogramList.refresh();
					try {
						if (watchService == null) throw new IOException();
						WatchKey key = path.register(watchService,
						           StandardWatchEventKinds.ENTRY_CREATE,
						           StandardWatchEventKinds.ENTRY_DELETE,
						           StandardWatchEventKinds.ENTRY_MODIFY);
						watchPaths.put(key, path);
					} catch (IOException e) {
						logger.error("Failure registering watch service for {}", path.getFileName());
					}
				}
				
				private void updateState(ImmutableModule im, ProgressBarColor color) {
					module.setModule(im);
					parseBtn.setDisable(false);
					progressBar.progressProperty().unbind();
					progressBar.progressProperty().set(Double.MAX_VALUE);
					progressBar.setStyle(color.style);
				}			
				
				private void updateCloneGroups(String oldCheckSum, String checkSum) {
					SetMultimap<Pair<String, String>, Pair<String, String>> newGroup = SetMultimapBuilder.hashKeys().linkedHashSetValues().build();
					//rewrite keys and values
					for (Iterator<Entry<Pair<String, String>, Pair<String, String>>> iterator = cloneGroups.entries().iterator(); iterator.hasNext();) {
						Entry<Pair<String, String>, Pair<String, String>> entry = (Entry<Pair<String, String>, Pair<String, String>>) iterator.next();
						Pair<String, String> key = entry.getKey();
						Pair<String, String> value = entry.getValue();
						boolean updated = false;
						if (key.getKey().equals(oldCheckSum)) {
							key = new Pair<>(checkSum, key.getValue());
							updated = true;
						}
								
						if (value.getKey().equals(oldCheckSum)) {
							value = new Pair<>(checkSum, key.getValue());
							updated = true;
						}
						
						if (updated) {
							iterator.remove();
							newGroup.put(key, value);
						}
					}
					if (!newGroup.isEmpty()) {
						cloneGroups.putAll(newGroup);				
					}
					
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
		
		lastSearchQuery = query;
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
		loadStage(Resource.CLONES_FXML, "clones_b_title", moduleB, feature.systemBSubprograms(), null, Modality.APPLICATION_MODAL);
	}

	@FXML
    void findClonesSystemC(ActionEvent event) throws IOException {
		if (moduleA == null || moduleA.getModule() == null) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		loadStage(Resource.CLONES_FXML, "clones_c_title", moduleC, feature.systemCSubprograms(), null, Modality.APPLICATION_MODAL);
    }
	
	@FXML
	void systemAAdjacentSubprograms(ActionEvent event) throws IOException {
		//TODO
		//moduleA.getModule().getGraph().printSubgraph(new HashSet<String>(feature.systemASubprograms()));
		loadStage(Resource.ADJACENT_FXML, "adjacent_sub_title", moduleA, feature.systemASubprograms(), null, Modality.APPLICATION_MODAL);
	}

	@FXML
	void systemAMarkSubprogram(ActionEvent event) {
		ObservableList<SearchEntryView> selectedItems = systemASearchResultTbl.getSelectionModel().getSelectedItems();
		if (selectedItems.isEmpty()) {return;}
		List<String> items = selectedItems
				.stream()
				.map(SearchEntryView::getName)
				.collect(Collectors.toList());
		feature.systemASubprograms().addAll(items);
	}

	@FXML
	void systemAVarControlledSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.VAR_FXML, "var_controlled_title", moduleA, feature.systemASubprograms(), null, Modality.APPLICATION_MODAL);
	}

	@FXML
    void systemBAdjacentSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.ADJACENT_FXML, "adjacent_sub_title", moduleB, feature.systemBSubprograms(), null, Modality.APPLICATION_MODAL);
    }

    @FXML
    void systemBVarControlledSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.VAR_FXML, "var_controlled_title", moduleB, feature.systemBSubprograms(), null, Modality.APPLICATION_MODAL);
    }

    @FXML
    void systemCAdjacentSubprograms(ActionEvent event) throws IOException {
    	loadStage(Resource.ADJACENT_FXML, "adjacent_sub_title", moduleC, feature.systemCSubprograms(), null, Modality.APPLICATION_MODAL);
    }

    @FXML
    void systemCVarControlledSubprograms(ActionEvent event) throws IOException {
		loadStage(Resource.VAR_FXML, "var_controlled_title", moduleC, feature.systemCSubprograms(), null, Modality.APPLICATION_MODAL);
    }

	@FXML
    void removeItemsSystemA(ActionEvent event) {
		removeItems(systemASubprogramList, feature.systemASubprograms(), moduleA);
    }

    @FXML
    void removeItemsSystemB(ActionEvent event) {
		removeItems(systemBSubprogramList, feature.systemBSubprograms(), moduleB);
    }

    @FXML
    void removeItemsSystemC(ActionEvent event) {
		removeItems(systemCSubprogramList, feature.systemCSubprograms(), moduleC);
    }
    
    private void removeItems(ListView<String> subprogramList, ObservableList<String> list, ModuleContainer module) {
    	List<String> selectedItems = new ArrayList<>(subprogramList.getSelectionModel().getSelectedItems());
		if (selectedItems.isEmpty()) {return;}
		list.removeAll(selectedItems);
		if (module.equals(moduleA)) {
			for (String item : selectedItems) {
				Set<Pair<String, String>> clones = cloneGroups.removeAll(new Pair<String, String>(moduleA.getCheckSum(), item));	
				if (!clones.isEmpty()) {
					logger.info("Removed clones for subprogram \"{}\"", item);
					for (Pair<String, String> pair : clones) {
						logger.info("Removed clone subprogram \"{}\" from module with checksum {}", pair.getValue(), pair.getKey());
					}				
				}
			}
		} else {
			for (String item : selectedItems) {
				cloneGroups.entries().removeIf(e -> 
					e.getValue().getKey().equals(module.getCheckSum()) && e.getValue().getValue().equals(item)
				);
			}
		}
    }
	
	private void loadStage(Resource resource, String title, 
			ModuleContainer module, ObservableList<String> fSubprograms, Control control,
			Modality modality) throws IOException {
		String selectedSubprogram = null;
		switch(resource) {
		case ADJACENT_FXML:
		case VAR_FXML:
			if (module.equals(moduleA)) {
				if (systemASubprogramList.getSelectionModel().getSelectedItems().size() > 1) {
					multipleSelectionAlert.showAndWait();
					return;
				}
				selectedSubprogram = systemASubprogramList.getSelectionModel().getSelectedItem();
			}
			
			if (module.equals(moduleB)) {
				if (systemBSubprogramList.getSelectionModel().getSelectedItems().size() > 1) {
					multipleSelectionAlert.showAndWait();
					return;
				}
				selectedSubprogram = systemBSubprogramList.getSelectionModel().getSelectedItem();
			}
			
			if (module.equals(moduleC)) {
				if (systemCSubprogramList.getSelectionModel().getSelectedItems().size() > 1) {
					multipleSelectionAlert.showAndWait();
					return;
				}
				selectedSubprogram = systemCSubprogramList.getSelectionModel().getSelectedItem();
			}
			if (!module.getModule().getGraph().containsSubprogram(selectedSubprogram)) {
				illegalStateAlert.showAndWait();
				return;
			}

			break;

		case CLONES_FXML:
			if (moduleA.getModule() == null || module.getModule() == null) {
				graphNotFoundAlert.showAndWait();
				return;
			}
			if (systemASubprogramList.getSelectionModel().getSelectedItems().size() > 1) {
				multipleSelectionAlert.showAndWait();
				return;
			}
			selectedSubprogram = systemASubprogramList.getSelectionModel().getSelectedItem();
			if (!moduleA.getModule().getGraph().containsSubprogram(selectedSubprogram)) {
				illegalStateAlert.showAndWait();
				return;
			}
			break;
		case CODEVIEW_FXML:
			if (control instanceof TableView<?>) {
				TableView<SearchEntryView> tv = (TableView<SearchEntryView>) control;
				if (tv.getSelectionModel().getSelectedItems().size() > 1) {
					multipleSelectionAlert.showAndWait();
					return;
				}
				selectedSubprogram = tv.getSelectionModel().getSelectedItem().getName();
			} else if (control instanceof ListView<?>) {
				ListView<String> lv = (ListView<String>)control;
				if (lv.getSelectionModel().getSelectedItems().size() > 1) {
					multipleSelectionAlert.showAndWait();
					return;
				}
				selectedSubprogram = lv.getSelectionModel().getSelectedItem();
			} else {
				throw new IllegalArgumentException();
			}
			break;
		default:
			throw new IllegalArgumentException();
		}

		if (selectedSubprogram == null || selectedSubprogram.isEmpty()) {return;}
				
		FXMLLoader loader = new FXMLLoader(getClass().getResource(resource.path()), Main.getResources());
		Controller controller = getControllerForResource(resource, selectedSubprogram, fSubprograms, module);
		if (controller == null) return;
		loader.setController(controller);
		Pane root = (Pane) loader.load();
		Scene scene = new Scene(root);
		scene.getStylesheets().add(getClass().getResource(Resource.CSS.path()).toExternalForm());
		
		Stage stage = new Stage();
		stage.setScene(scene);
		stage.setTitle(Main.getResources().getString(title));
		if (SystemUtils.IS_OS_LINUX) {
			stage.initModality(Modality.NONE);
		} else {
			stage.initModality(modality);
		}
		stage.show();
	}
	
	private Controller getControllerForResource(Resource resource, String selectedSubprogram,
			ObservableList<String> fSubprograms, ModuleContainer module) {
		switch(resource) {
		case ADJACENT_FXML:
		return constructASController(selectedSubprogram, fSubprograms, module);
		case VAR_FXML:
		return constructVCController(selectedSubprogram, fSubprograms, module);
		case CLONES_FXML:
		return new ClonesController(
				selectedSubprogram,	fSubprograms,
				moduleA, module, cloneGroups);
		case CODEVIEW_FXML:
		return constructCVController(selectedSubprogram, fSubprograms, module);
		default:
			throw new IllegalArgumentException();
		}
		
	}
	
	private Controller constructCVController(String selectedSubprogram, ObservableList<String> fSubprograms, ModuleContainer module) {
		Optional<SubProgram> subprogram = module.getModule().getGraph().subprograms()
			.stream()
			.filter(s -> s.name().equals(selectedSubprogram))
			.findFirst();
		if (!subprogram.isPresent()) {
			illegalStateAlert.showAndWait();
			return null;
		}
		return new CodeViewController(subprogram.get(), selectedSubprogram, fSubprograms);
	}

	private AdjacentSubprogramsController constructASController(String selectedSubprogram,
			ObservableList<String> fSubprograms, ModuleContainer module) {
		ObservableList<CallDependencyView> callers = module.getModule().getGraph().getSubprogramCallers(selectedSubprogram)
				.stream()
				.filter(s -> !fSubprograms.contains(s))
				.map(s -> new CallDependencyView(module.getModule().getGraph().getFanOut(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));

		ObservableList<CallDependencyView> callees = module.getModule().getGraph().getSubprogramCallees(selectedSubprogram)
				.stream()
				.filter(s -> !fSubprograms.contains(s))
				.map(s -> new CallDependencyView(module.getModule().getGraph().getFanIn(s), s))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));
		
		return new AdjacentSubprogramsController(selectedSubprogram, fSubprograms, callers, callees);
	}
	
	private VariableControlledController constructVCController(String selectedSubprogram,
			ObservableList<String> fSubprograms, ModuleContainer module) {
		final Set<String> duplicates = new HashSet<>();
		Map<String, Set<String>> variables = module.getModule().getGraph().getVariablesAndCallees(selectedSubprogram)
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
		aboutAlert.showAndWait();
		if (SystemUtils.IS_OS_LINUX) {
			commandStack.clear();
		}
    }
	
	@FXML
	void exportToCsv(ActionEvent event) {exportFeature(ExportType.CSV);}

	@FXML
	void exportToJson(ActionEvent event) {exportFeature(ExportType.JSON);}
		
    private void exportFeature(ExportType exportType) {
    	if (SystemUtils.IS_OS_LINUX) {
			commandStack.clear();
		}
    	
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
    	File selectedFile = SystemUtils.IS_OS_LINUX ? exportChooser.showSaveDialog(null) : exportChooser.showSaveDialog(systemALbl.getScene().getWindow());
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
    	if (SystemUtils.IS_OS_LINUX) {
			commandStack.clear();
		}
    	if (feature.systemASubprograms().isEmpty() || moduleA.getModule() == null) {
    		locALbl.setText("0 " + Main.getResources().getString("loc"));
		} else {
			calculateLoc(locALbl, feature.systemASubprograms(), moduleA.getModule().getGraph());
		}
    }

    @FXML
    void locSystemB(ActionEvent event) {
    	if (SystemUtils.IS_OS_LINUX) {
			commandStack.clear();
		}
    	if (feature.systemBSubprograms().isEmpty() || moduleB.getModule() == null) {
    		locBLbl.setText("0 " + Main.getResources().getString("loc"));
		} else {
			calculateLoc(locBLbl, feature.systemBSubprograms(), moduleB.getModule().getGraph());
		}
    }

    @FXML
    void locSystemC(ActionEvent event) {
    	if (SystemUtils.IS_OS_LINUX) {
			commandStack.clear();
		}
    	if (feature.systemCSubprograms().isEmpty() || moduleC.getModule() == null) {
    		locCLbl.setText("0 " + Main.getResources().getString("loc"));
		} else {
			calculateLoc(locCLbl, feature.systemCSubprograms(), moduleC.getModule().getGraph());
		}
    }
    
    @FXML
    void viewSubprogramCodeFromSearch(MouseEvent event) throws IOException {
    	if (event.getClickCount() == 2) {
    		loadStage(Resource.CODEVIEW_FXML, "code_view", moduleA, feature.systemASubprograms(), systemASearchResultTbl, Modality.WINDOW_MODAL);
    	}
    }

    @FXML
    void viewSubprogramCodeSystemA(MouseEvent event) throws IOException {
    	if (event.getClickCount() == 2) {
    		loadStage(Resource.CODEVIEW_FXML, "code_view", moduleA, feature.systemASubprograms(), systemASubprogramList, Modality.WINDOW_MODAL);
    	}
    }

    @FXML
    void viewSubprogramCodeSystemB(MouseEvent event) throws IOException {
    	if (event.getClickCount() == 2) {
    		loadStage(Resource.CODEVIEW_FXML, "code_view", moduleB, feature.systemBSubprograms(), systemBSubprogramList, Modality.WINDOW_MODAL);
    	}
    }

    @FXML
    void viewSubprogramCodeSystemC(MouseEvent event) throws IOException {
    	if (event.getClickCount() == 2) {
    		loadStage(Resource.CODEVIEW_FXML, "code_view", moduleC, feature.systemCSubprograms(), systemCSubprogramList, Modality.WINDOW_MODAL);
    	}
    }
    
    @FXML
    void refactorItemSystemA(ActionEvent event) {
		refactorItem(systemASubprogramList, moduleA);
    }

    @FXML
    void refactorItemSystemB(ActionEvent event) {
		refactorItem(systemBSubprogramList, moduleB);
	}

    @FXML
    void refactorItemSystemC(ActionEvent event) {
		refactorItem(systemCSubprogramList, moduleC);
	}
    
    private void refactorItem(ListView<String> subprogramList, ModuleContainer module) {
    	List<String> selectedItems = new ArrayList<>(subprogramList.getSelectionModel().getSelectedItems());
    	if (selectedItems.isEmpty()) return;
		if (selectedItems.size() > 1) {
			multipleSelectionAlert.showAndWait();
			return;
		}	
		
		if (module == null || cafModule == null || 
				module.getModule() == null || cafModule.getModule() == null || 
				(module.getModule().getGraph() == null || module.getModule().getGraph().isEmpty()) || 
				(cafModule.getModule().getGraph() == null || cafModule.getModule().getGraph().isEmpty())) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		if (!module.getModule().getGraph().containsSubprogram(selectedItems.get(0))) {
			illegalStateAlert.showAndWait();
			return;
		}
		
		RWARefactoringTask rTask = new RWARefactoringTask(selectedItems.get(0), module, cafModule.getModule().getGraph(), cloneGroups);
		
		statusLabel.textProperty().bind(rTask.messageProperty());
		
		rTask.setOnSucceeded((e) -> {
			statusLabel.textProperty().unbind();
			counter.set(atomicCounter.decrementAndGet());

			if (rTask.getValue().isEmpty()) {
				statusLabel.setText("Refactoring successfully completed.");
				successfulRefactoring.showAndWait();
			} else {
				statusLabel.setText("Refactoring completed with errors.");
				TextArea textArea = new TextArea(String.join("\n", rTask.getValue()));
				textArea.setPrefColumnCount(40);
				textArea.setPrefRowCount(20);
				textArea.setEditable(false);
				textArea.setWrapText(true);
				failRefactoring.getDialogPane().setContent(textArea);
				failRefactoring.setHeaderText("There were some refactoring errors.");
				failRefactoring.showAndWait();
			}
			
		});
		
		rTask.setOnFailed((e) -> {
			statusLabel.textProperty().unbind();
			statusLabel.setText("Refactoring failed.");
			rTask.getException().printStackTrace();
			counter.set(atomicCounter.decrementAndGet());
		});
		counter.set(atomicCounter.incrementAndGet());
		Main.getRefactoringPoolInstance().execute(rTask);
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
				try(Stream<String> lines = Utils.readAllFromFile(subprogram.path()).stream()){
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

	private static class RWARefactoringTask extends Task<List<String>> {
		
		final Logger logger = LoggerFactory.getLogger(RWARefactoringTask.class);
		
		private final String subName;
		private final ModuleContainer module;
		private final SDGraph cafGraph;
		private final SetMultimap<Pair<String, String>, Pair<String, String>> cloneGroups;
		
		public RWARefactoringTask(String subName, ModuleContainer module, SDGraph cafGraph, 
				SetMultimap<Pair<String, String>, Pair<String, String>> cloneGroups) {
			this.subName = subName;
			this.module = module;
			this.cafGraph = cafGraph;
			this.cloneGroups = cloneGroups;
		}

		@Override
		protected List<String> call() throws Exception {
			logger.info("Starting refactoring task for " + subName);
			updateMessage("Starting refactoring task for " + subName);
			List<String> refactoringErrors = new ArrayList<>();
			
			Optional<SubProgram> subprogram = module.getModule().getGraph().subprograms()
					.stream()
					.filter(s -> s.name().equalsIgnoreCase(subName))
					.findFirst();
			if (subprogram.isEmpty()) {
				throw new IllegalStateException();
			}
			List<String> originalFile = Utils.readFullLinesFromFile(subprogram.get().path());
			
			Stream<String> originalSubprogram = originalFile.stream().limit(subprogram.get().endLine()).skip(subprogram.get().startLine() - 1);
			
			Refactoring refactoring = Refactorings.createRWARefactoring(subprogram.get().path(), originalSubprogram, module.getModule().getGraph(), cafGraph);
			
			List<String> autoRefactoredSubprogram = refactoring.refactor();
			
			List<String> originalSubprogramList = originalFile.stream()
					.limit(subprogram.get().endLine())
					.skip(subprogram.get().startLine() - 1)
					.map(s -> s.replace("\r\n", ""))
					.collect(Collectors.toList());
			if (originalSubprogramList.equals(autoRefactoredSubprogram)) {
				updateMessage("Nothing to refactor: exit.");
				logger.info("Nothing to refactor: exit.");
				return refactoringErrors;
			}
			if (!refactoring.getErrors().isEmpty()) {
				refactoringErrors.add(subprogram.get().toString());
				logger.error("Refactoring errors for : {}", subprogram.get().toString());
				refactoringErrors.addAll(refactoring.getErrors());
				refactoring.getErrors().forEach(logger::error);
			}
			
			Utils.updateSubprogramInFile(originalFile, autoRefactoredSubprogram, subprogram.get());
			CoRAMainController.processingQueue.add(module.getPath());

			updateMessage("Updating IR and SDGraph for " + subName);
			logger.info("Updating IR and SDGraph for {}", subName);
			
			String oldCheckSum = module.updateCheckSum(Repositories.checkSumForPath(module.getPath()));	
			updateCloneGroups(oldCheckSum, module.getCheckSum());
			module.setModule(Repositories.getInstance().updateOrRetrieve(module, oldCheckSum, subprogram.get().path()));
			
			updateMessage("Refactoring callers of " + subName);
			logger.info("Refactoring {} callers", subName);

			Set<String> callerNames = module.getModule().getGraph().getSubprogramCallers(subprogram.get().name());
			for (String callerName : callerNames) {
				SubProgram caller = module.getModule().getGraph().subprograms()
						.stream()
						.filter(s -> s.name().equalsIgnoreCase(callerName))
						.findFirst().get();
				
				originalFile = Utils.readFullLinesFromFile(caller.path());
				originalSubprogram = originalFile.stream().limit(caller.endLine()).skip(caller.startLine() - 1);
				Refactoring callRefactoring = Refactorings.createCallerRefactoring(caller.path(), originalSubprogram, refactoring);
				autoRefactoredSubprogram = callRefactoring.refactor();	
				if (!callRefactoring.getErrors().isEmpty()) {
					refactoringErrors.add(caller.toString());
					logger.error("Refactoring errors for : {}", caller.toString());
					refactoringErrors.addAll(callRefactoring.getErrors());
					callRefactoring.getErrors().forEach(logger::error);
				}
				
				Utils.updateSubprogramInFile(originalFile, autoRefactoredSubprogram, caller);
				CoRAMainController.processingQueue.add(module.getPath());

				updateMessage("Updating IR and SDGraph for " + caller.name());
				logger.info("Updating IR and SDGraph for {}", caller.name());
				oldCheckSum = module.updateCheckSum(Repositories.checkSumForPath(module.getPath()));	
				updateCloneGroups(oldCheckSum, module.getCheckSum());
				module.setModule(Repositories.getInstance().updateOrRetrieve(module, oldCheckSum, caller.path()));
			}
						
			updateMessage("Refactoring completed for " + subName);
			logger.info("Refactoring completed for " + subName);
			return refactoringErrors;
		}

		private void updateCloneGroups(String oldCheckSum, String checkSum) {
			SetMultimap<Pair<String, String>, Pair<String, String>> newGroup = SetMultimapBuilder.hashKeys().linkedHashSetValues().build();
			//rewrite keys and values
			for (Iterator<Entry<Pair<String, String>, Pair<String, String>>> iterator = cloneGroups.entries().iterator(); iterator.hasNext();) {
				Entry<Pair<String, String>, Pair<String, String>> entry = (Entry<Pair<String, String>, Pair<String, String>>) iterator.next();
				Pair<String, String> key = entry.getKey();
				Pair<String, String> value = entry.getValue();
				boolean updated = false;
				if (key.getKey().equals(oldCheckSum)) {
					key = new Pair<>(checkSum, key.getValue());
					updated = true;
				}
						
				if (value.getKey().equals(oldCheckSum)) {
					value = new Pair<>(checkSum, key.getValue());
					updated = true;
				}
				
				if (updated) {
					iterator.remove();
					newGroup.put(key, value);
				}
			}
			if (!newGroup.isEmpty()) {
				cloneGroups.putAll(newGroup);				
			}
			
		}

		
	}
	
	public boolean equalsExposedFields(CoRAMainController cmc) {
		if (this == cmc) {
			return true;
		}
		return (this.lastSearchQuery == cmc.lastSearchQuery || this.lastSearchQuery.equals(cmc.lastSearchQuery)) &&
				(this.lastKnownDir == cmc.lastKnownDir || this.lastKnownDir.equals(cmc.lastKnownDir)) &&
				this.searchResults.equals(cmc.searchResults) &&
				this.moduleA.equals(cmc.moduleA) &&
				this.moduleB.equals(cmc.moduleB) &&
				this.moduleC.equals(cmc.moduleC) &&
				this.cafModule.equals(cmc.cafModule) &&
				this.feature.equals(cmc.feature) &&
				this.cloneGroups.equals(cmc.cloneGroups);
	}
	
	@FXML
    void refactorCloneClass(ActionEvent event) throws IOException {
		List<String> selectedItems = new ArrayList<>(systemASubprogramList.getSelectionModel().getSelectedItems());
    	if (selectedItems.isEmpty()) return;
		if (selectedItems.size() > 1) {
			multipleSelectionAlert.showAndWait();
			return;
		}	
		
		if (moduleA == null || cafModule == null || 
				moduleA.getModule() == null || cafModule.getModule() == null || 
				(moduleA.getModule().getGraph() == null || moduleA.getModule().getGraph().isEmpty()) || 
				(cafModule.getModule().getGraph() == null || cafModule.getModule().getGraph().isEmpty())) {
			graphNotFoundAlert.showAndWait();
			return;
		}
		
		if (!moduleA.getModule().getGraph().containsSubprogram(selectedItems.get(0))) {
			illegalStateAlert.showAndWait();
			return;
		}
		
		Pair<String, String> key = new Pair<>(moduleA.getCheckSum(), selectedItems.get(0));
		if (!cloneGroups.containsKey(key)) {
			illegalStateAlert.showAndWait();
			return;
		}
		
		List<Pair<String, String>> value = new ArrayList<>(cloneGroups.get(key));
		value.add(0, key);
		FXMLLoader loader = new FXMLLoader(getClass().getResource(Resource.CLONECLASS_FXML.path()), Main.getResources());
		CloneClassController controller = new CloneClassController(value);
		
		loader.setController(controller);
		Pane root = (Pane) loader.load();
		Scene scene = new Scene(root);
		scene.getStylesheets().add(getClass().getResource(Resource.CSS.path()).toExternalForm());
		
		Stage stage = new Stage();
		stage.setScene(scene);
		stage.setTitle(Main.getResources().getString("cc_title"));
		if (SystemUtils.IS_OS_LINUX) {
			stage.initModality(Modality.NONE);
		} else {
			stage.initModality(Modality.APPLICATION_MODAL);
		}
		stage.show();
    }

}
