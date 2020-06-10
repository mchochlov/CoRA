package com.woodplc.cora.storage;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testfx.framework.junit5.ApplicationExtension;

import com.woodplc.cora.gui.controllers.CoRAMainController;
import com.woodplc.cora.utils.TestUtils;

import javafx.application.Platform;

@ExtendWith(ApplicationExtension.class)
class JSONUtilsJavaFXTest {
	
	@Test
	void testStateLoadRestoreIntegrity() throws IOException {
		Platform.runLater(() -> {
			try {
				Path tmpFile = Paths.get("test");
				CoRAMainController emptyController = new CoRAMainController();
				assertNotNull(emptyController);
				JSONUtils.stateToJson(emptyController, tmpFile);
				
				CoRAMainController restoredEmptyController = JSONUtils.stateFromJson(tmpFile);
				
				assertNotNull(restoredEmptyController);
				assertTrue(emptyController != restoredEmptyController);
				assertTrue(emptyController.equalsExposedFields(restoredEmptyController));
				
				CoRAMainController nonEmptyController = TestUtils.fullyInitializedMainController();
				assertNotNull(nonEmptyController);
				JSONUtils.stateToJson(nonEmptyController, tmpFile);
				
				CoRAMainController restoredNonEmptyController = JSONUtils.stateFromJson(tmpFile);
				assertNotNull(restoredNonEmptyController);
				assertTrue(nonEmptyController != restoredNonEmptyController);
				assertTrue(nonEmptyController.equalsExposedFields(restoredNonEmptyController));
				Files.delete(tmpFile);
				
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}
		});
	}

}
