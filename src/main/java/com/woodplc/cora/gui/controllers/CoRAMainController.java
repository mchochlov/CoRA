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
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.gui.model.DataModel;
import com.woodplc.cora.gui.model.DataModel.Subprogram;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;

import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;

public class CoRAMainController {
	
	private final DataModel model = DataModel.instance();
	
	private final DirectoryChooser dirChooser = new DirectoryChooser();
	
	private final Module moduleA = new Module();
	private final Module moduleB = new Module();
	private final Module moduleC = new Module();
	
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
	private TableColumn<Subprogram, String> flex3ClmnId;

	@FXML
	private TableColumn<Subprogram, String> flex3ClmnName;

	@FXML
	private TableView<Subprogram> flex3Tbl;
	
	

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
		flex3List.setItems(model.getFlex3Subprograms());
		flex3ClmnId.setCellValueFactory(new PropertyValueFactory<Subprogram, String>("id"));
		flex3ClmnName.setCellValueFactory(new PropertyValueFactory<Subprogram, String>("name"));
		flex3Tbl.setItems(model.getData());

	}

	@FXML
	void findClonesDeepRiser(ActionEvent event) throws IOException {
		Stage stage = new Stage();
		AnchorPane root = (AnchorPane) FXMLLoader.load(getClass().getResource("/com/woodplc/cora/gui/fxml/ClonesDeepRiser.fxml"));
		Scene scene = new Scene(root, 550, 450);
		scene.getStylesheets().add(getClass().getResource("/com/woodplc/cora/gui/css/application.css").toExternalForm());
		stage.setScene(scene);
		stage.show();
	}

	@FXML
	void flex3AdjacentSubprograms(ActionEvent event) throws IOException {
		Stage stage = new Stage();
		AnchorPane root = (AnchorPane) FXMLLoader.load(getClass().getResource("/com/woodplc/cora/gui/fxml/AdjacentSubprograms.fxml"));
		Scene scene = new Scene(root, 530, 420);
		scene.getStylesheets().add(getClass().getResource("/com/woodplc/cora/gui/css/application.css").toExternalForm());
		stage.setScene(scene);
		stage.show();
		//flex3CallersListView.setItems(flex3Callers);
	}

	@FXML
	void flex3MarkSubprogram(ActionEvent event) {
		Subprogram selectedItem = flex3Tbl.getSelectionModel().getSelectedItem();
		flex3Tbl.getItems().remove(selectedItem);
		flex3List.getItems().add(selectedItem.getName());
	}

	@FXML
	void flex3VarControlledSubprograms(ActionEvent event) throws IOException {
		Stage stage = new Stage();
		AnchorPane root = (AnchorPane) FXMLLoader.load(getClass().getResource("/com/woodplc/cora/gui/fxml/VariableControlledSubprograms.fxml"));
		Scene scene = new Scene(root, 530, 420);
		scene.getStylesheets().add(getClass().getResource("/com/woodplc/cora/gui/css/application.css").toExternalForm());
		stage.setScene(scene);
		stage.show();
	}
	
	private static class Module {
		private Path path;
		private SDGraph graph;
		
		private Module() {}
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
