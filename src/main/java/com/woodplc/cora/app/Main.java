package com.woodplc.cora.app;
	
import java.util.ResourceBundle;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;


public class Main extends Application {

	public enum Resource {
		CSS("/com/woodplc/cora/gui/css/application.css"),
		TEXT("com.woodplc.cora.gui.fxml.CoraResources"),
		MAIN_FXML("/com/woodplc/cora/gui/fxml/CoRAMain.fxml"),
		ADJACENT_FXML("/com/woodplc/cora/gui/fxml/AdjacentSubprograms.fxml");
		
		private final String path;
		
		Resource(String path){this.path = path;}
		
		public String path() {return this.path;}
	}

	private final static ResourceBundle resources = ResourceBundle.getBundle(Resource.TEXT.path);
	
	
	@Override
	public void start(Stage primaryStage) {
		try {
			primaryStage.setTitle(resources.getString("cora_main_title"));
			AnchorPane root = (AnchorPane)FXMLLoader.load(getClass().getResource(Resource.MAIN_FXML.path), resources);
			Scene scene = new Scene(root,600,600);
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
}
