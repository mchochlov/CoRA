package com.woodplc.cora.gui.controllers;

import java.time.Duration;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.fxmisc.richtext.CodeArea;
import org.fxmisc.richtext.LineNumberFactory;
import org.fxmisc.richtext.model.StyleSpans;
import org.fxmisc.richtext.model.StyleSpansBuilder;
import org.reactfx.Subscription;

import com.woodplc.cora.app.Main;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.gui.async.CloneMergeTask;
import com.woodplc.cora.trees.algorithms.MergeAlgorithms;
import com.woodplc.cora.trees.algorithms.MergeAlgorithms.AlgorithmBuilder;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.paint.Color;
import javafx.util.Pair;

public class CloneClassController {

	@FXML
    private ListView<Pair<String, String>> ccListview;

    @FXML
    private CheckBox useSFcheckBox;

    @FXML
    private CodeArea codeMergeArea;

    @FXML
    private Button mergeBtn;
    
    @FXML
    private Label mergeProgressLbl;
    
    private final List<Pair<String,String>> clones;
    private final Map<Pair<String, String>, Color> colors;
    private static final List<Color> COLORS = List.of(Color.RED, Color.BLUE, Color.GREEN); 
    private static final int MAX_COLORS = 3;
    private final List<SubProgram> subprograms;
    private final ObservableList<Pair<String, String>> obsCloneList = FXCollections.observableArrayList();
    private Subscription cleanupWhenNoLongerNeedIt;
        
    private final Pattern PATTERN;
    private final int numStyles;

    public CloneClassController(List<Pair<String,String>> value, List<SubProgram> subprograms) {
		this.clones = value;
		this.subprograms = subprograms;
		colors = new HashMap<>();
		
		StringBuilder pattern = new StringBuilder();
		for (int i = 0; i < subprograms.size(); i++) {
			pattern.append("(?<TREE").append(i).append(">").append("\\<\\<INS from ").append(i).append(":(.|\n)*?\\>\\>").append(")").append("|");
			pattern.append("(?<UPDTREE").append(i).append(">").append("\\<\\<UPD from ").append(i).append(":(.|\n)*?\\>\\>").append(")").append("|");
			colors.put(value.get(i), COLORS.get(i%MAX_COLORS));
		}
		PATTERN = Pattern.compile(pattern.substring(0, pattern.length() - 1));
		this.numStyles = subprograms.size();
	}

    @FXML
	void initialize() {
    	ccListview.setCellFactory(f -> new PairFormatCell(colors));
    	obsCloneList.addAll(clones);
    	ccListview.setItems(obsCloneList);
    	
    	codeMergeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeMergeArea));
    	
        cleanupWhenNoLongerNeedIt = codeMergeArea
        		.multiPlainChanges()
                .successionEnds(Duration.ofMillis(500))
                .subscribe(ignore -> codeMergeArea.setStyleSpans(0, computeHighlighting(codeMergeArea.getText())));
    }
    
	@FXML
    void mergeClones(ActionEvent event) {
		AlgorithmBuilder aBuilder = MergeAlgorithms.configure();
		if (useSFcheckBox.isSelected()) {
			aBuilder.withSuperFunctionalityRefactoring();
		}
		
		CloneMergeTask cmt = new CloneMergeTask(aBuilder, subprograms);
		
		mergeProgressLbl.textProperty().bind(cmt.messageProperty());
		
		cmt.setOnSucceeded((e) -> {
			updateState("Clone class merge successfully completed.");		
			try {
				String content = String.join("\n", cmt.get());
				codeMergeArea.replaceText(content);
				codeMergeArea.setStyleSpans(0, computeHighlighting(content));
			} catch (InterruptedException | ExecutionException e1) {
				e1.printStackTrace();
			}
		});
		
		cmt.setOnFailed((e) -> {
			updateState("Clone class merge failed.");
			cmt.getException().printStackTrace();
		});
		
		mergeBtn.setDisable(true);
		Main.getRefactoringPoolInstance().execute(cmt);
    }
	
	private void updateState(String text) {
		mergeProgressLbl.textProperty().unbind();
		mergeBtn.setDisable(false);
		mergeProgressLbl.setText(text);
	}
	
	private StyleSpans<Collection<String>> computeHighlighting(String text) {
        Matcher matcher = PATTERN.matcher(text);
        int lastKwEnd = 0;
        StyleSpansBuilder<Collection<String>> spansBuilder
                = new StyleSpansBuilder<>();
        while(matcher.find()) {
            String styleClass = null;
            for (int i = 0; i < numStyles; i++) {
				if (matcher.group("TREE" + i) != null || matcher.group("UPDTREE" + i) != null) {
					styleClass = "tree" + i%MAX_COLORS;
					break;
				}
			}
            if (styleClass == null) {
				throw new IllegalStateException();
			}
            
            spansBuilder.add(Collections.emptyList(), matcher.start() - lastKwEnd);
            spansBuilder.add(Collections.singleton(styleClass), matcher.end() - matcher.start());
            lastKwEnd = matcher.end();
        }
        spansBuilder.add(Collections.emptyList(), text.length() - lastKwEnd);
        return spansBuilder.create();
    }
	
	
	private static class PairFormatCell extends ListCell<Pair<String, String>> {
		
		private final Map<Pair<String, String>, Color> colors;
		
		public PairFormatCell(Map<Pair<String, String>, Color> colors) {
			this.colors = colors;
		}
		
		@Override
		protected void updateItem(Pair<String, String> item, boolean empty) {
			super.updateItem(item, empty);
			if (item !=  null) {
				setText(item.getValue() + " (" + item.getKey() + ")");
				setTextFill(colors.get(item));
			}
		}
		
	}


	public void deinit() {
		if (cleanupWhenNoLongerNeedIt != null) {
			cleanupWhenNoLongerNeedIt.unsubscribe();
		}
	}
}
