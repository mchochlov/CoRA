package com.woodplc.cora.gui.controllers;

import java.io.File;
import java.io.IOException;

import com.woodplc.cora.gui.model.DataModel;
import com.woodplc.cora.gui.model.DataModel.Subprogram;

import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Button;
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
	
	private DataModel model;

	@FXML
	private ListView<String> flex3List;
	
	@FXML
	private TableColumn<Subprogram, String> flex3ClmnId;

	@FXML
	private TableColumn<Subprogram, String> flex3ClmnName;

	@FXML
	private TableView<Subprogram> flex3Tbl;
	
	@FXML
	private TextField flex3DirFld;
	
	@FXML
	private Button flex3BrowseBtn;
	
	@FXML
	private ProgressBar flex3ProgressBar;

	@FXML
	void openFlex3BrowseDlg(ActionEvent event) {
		DirectoryChooser fileChooser = new DirectoryChooser();
		fileChooser.setTitle("Select Flex3 directory");
		File selectedFile = fileChooser.showDialog(flex3BrowseBtn.getScene().getWindow());
		flex3DirFld.setText(selectedFile.getAbsolutePath());
	}

	@FXML
	void parseFlex3(ActionEvent event) {
		Task<Void> task = new Task<Void>() {
			@Override
			public Void call() throws Exception {
				final int max = 10;
				for (int i = 1; i <= max; i++) {
					if (isCancelled()) {
						break;
					}
					updateProgress(i, max);
					Thread.sleep(1000);
				}
				return null;
			}
		};

		flex3ProgressBar.progressProperty().bind(task.progressProperty());
		new Thread(task).start();
	}

	@FXML
	void flex3Search(ActionEvent event) {
/*		flex3ClmnId.setCellValueFactory(new PropertyValueFactory<Subprogram, String>("id"));
		flex3ClmnName.setCellValueFactory(new PropertyValueFactory<Subprogram, String>("name"));
*/		// flex3Tbl.getColumns().addAll(flex3ClmnId, flex3ClmnName);
	}

	@FXML
	void initialize() {
		this.model = DataModel.instance();
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
}
