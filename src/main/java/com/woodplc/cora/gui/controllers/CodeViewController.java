package com.woodplc.cora.gui.controllers;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.fxmisc.richtext.CodeArea;
import org.fxmisc.richtext.LineNumberFactory;

import com.woodplc.cora.data.SubProgram;

import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.scene.control.Label;

class CodeViewController extends Controller {

	private final SubProgram subprogram;
	
	@FXML
    private Label subprogramLbl;
	
	@FXML
    private Label pathLbl;
	
	@FXML
    private CodeArea viewCodeArea;
    
	CodeViewController(SubProgram subprogram, String subname, ObservableList<String> systemASubprograms) {
		super(subname, systemASubprograms);
		this.subprogram = subprogram;
	}
	
	@FXML
	void initialize() {
		subprogramLbl.setText(subprogramLbl.getText() + " " + subprogram.name() + " [" + subprogram.startLine() + ", " + subprogram.endLine() + "]");
		pathLbl.setText(pathLbl.getText() + " " + subprogram.path());
		viewCodeArea.setParagraphGraphicFactory(LineNumberFactory.get(viewCodeArea));
		
		CodeReadTask sTask = new CodeReadTask(this.subprogram);
		
		sTask.setOnSucceeded((e) -> {
			viewCodeArea.replaceText(0, 0, String.join("\n", sTask.getValue()));
			viewCodeArea.showParagraphAtTop(subprogram.startLine() - 1);
		});
		
		sTask.setOnFailed((e) -> {
			sTask.getException().printStackTrace();
		});
		
		new Thread(sTask).start();
	}
	
	
	private static class CodeReadTask extends Task<List<String>> {
		
		private final SubProgram subprogram;

		CodeReadTask(SubProgram subprogram){ 
			this.subprogram = Objects.requireNonNull(subprogram);
		}
		
		@Override
		protected List<String> call() throws Exception {
			return Files.lines(this.subprogram.path(), Charset.defaultCharset())
					//.limit(this.subprogram.endLine())
					//.skip(this.subprogram.startLine())
					.collect(Collectors.toList());
		}
		
	}

}
