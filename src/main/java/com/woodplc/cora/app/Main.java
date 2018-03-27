package com.woodplc.cora.app;
	
import java.util.ResourceBundle;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;


public class Main extends Application {
	
	private final static String CORA_RESOURCES = "com.woodplc.cora.gui.fxml.CoraResources";
	private final static ResourceBundle resources = ResourceBundle.getBundle(CORA_RESOURCES);
	
	@Override
	public void start(Stage primaryStage) {
		try {
			primaryStage.setTitle(resources.getString("cora_main_title"));
			AnchorPane root = (AnchorPane)FXMLLoader.load(getClass().getResource("/com/woodplc/cora/gui/fxml/CoRAMain.fxml"), resources);
			Scene scene = new Scene(root,600,600);
			scene.getStylesheets().add(getClass().getResource("/com/woodplc/cora/gui/css/application.css").toExternalForm());
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
