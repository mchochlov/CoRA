package com.woodplc.cora.app;
	
import java.util.Optional;
import java.util.ResourceBundle;

import com.woodplc.cora.gui.controllers.CoRAMainController;
import com.woodplc.cora.storage.JSONUtils;
import com.woodplc.cora.storage.Repositories;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;


public class Main extends Application {

	public enum Resource {
		CSS("/com/woodplc/cora/gui/css/application.css"),
		TEXT("com.woodplc.cora.gui.fxml.CoraResources"),
		MAIN_FXML("/com/woodplc/cora/gui/fxml/CoRAMain.fxml"),
		ADJACENT_FXML("/com/woodplc/cora/gui/fxml/AdjacentSubprograms.fxml"),
		VAR_FXML("/com/woodplc/cora/gui/fxml/VariableControlledSubprograms.fxml"),
		CLONES_FXML("/com/woodplc/cora/gui/fxml/Clones.fxml"),
		FORTRAN_KEYWORDS("fortran_keywords.txt");
		
		private final String path;
		
		Resource(String path){this.path = path;}
		
		public String path() {return this.path;}
	}

	private final static ResourceBundle resources = ResourceBundle.getBundle(Resource.TEXT.path);
	private CoRAMainController mainController = null;
	
	@Override
	public void start(Stage primaryStage) {
		try {
			if (JSONUtils.stateFileExists()) {
				Alert alert = new Alert(AlertType.CONFIRMATION, Main.getResources().getString("restore_question"),
						ButtonType.YES, ButtonType.NO);
				Optional<ButtonType> result = alert.showAndWait();
				if (result.isPresent() && result.get() == ButtonType.YES) {
					mainController = new CoRAMainController(JSONUtils.stateFromJson());
				} 
			}
			if (mainController == null) mainController = new CoRAMainController();
			
			FXMLLoader loader = new FXMLLoader(getClass().getResource(Resource.MAIN_FXML.path), resources);
			loader.setController(mainController);
			primaryStage.setTitle(resources.getString("cora_main_title"));
			Pane root = (Pane) loader.load();
			Scene scene = new Scene(root);
			scene.getStylesheets().add(getClass().getResource(Resource.CSS.path).toExternalForm());
			primaryStage.setScene(scene);
			primaryStage.show();
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		launch(args);
	}
	
	public static ResourceBundle getResources() {
		return resources;
	}
	
	@Override
	public void stop() throws Exception {
		super.stop();
		Alert alert = new Alert(AlertType.NONE, Main.getResources().getString("sync_alert"));
		alert.show();
		Repositories.getInstance().closeAndSync();
		if (mainController != null) JSONUtils.stateToJson(mainController.getApplicationState());
		alert.close();
	}

}
