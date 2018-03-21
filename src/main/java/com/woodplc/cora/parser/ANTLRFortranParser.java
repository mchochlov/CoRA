package com.woodplc.cora.parser;

import static java.util.stream.Collectors.toSet;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import com.woodplc.cora.data.FortranFileModel;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.grammar.FuzzyFortranBaseListener;
import com.woodplc.cora.grammar.FuzzyFortranLexer;
import com.woodplc.cora.grammar.FuzzyFortranParser;
import com.woodplc.cora.grammar.FuzzyFortranParser.CallStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.IfOneLineContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.IfStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.SubprogramContext;

class ANTLRFortranParser implements Parser {

	@Override
	public FortranFileModel parse(Path path) {
		Objects.requireNonNull(path);
		
		CharStream stream = null;
		try {
			stream = CharStreams.fromString(new String(Files.readAllBytes(path)).toLowerCase());
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		FuzzyFortranLexer lexer = new FuzzyFortranLexer(stream);
		CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
		FuzzyFortranParser parser = new FuzzyFortranParser(commonTokenStream);
		parser.setErrorHandler(new BailErrorStrategy());
		parser.removeErrorListeners();
		parser.getInterpreter().setPredictionMode(PredictionMode.SLL);
		FuzzyListener fl = new FuzzyListener(path);
		ParseTree tree = null;
		try {
			tree = parser.inputFile();
		} catch (ParseCancellationException e) {
			e.printStackTrace();
			commonTokenStream.seek(0);
			parser.addErrorListener(ConsoleErrorListener.INSTANCE);
			parser.setErrorHandler(new DefaultErrorStrategy());
			parser.getInterpreter().setPredictionMode(PredictionMode.LL);
			tree = parser.inputFile();
		}
		ParseTreeWalker ptw = new ParseTreeWalker();
		ptw.walk(fl, tree);
		return fl.getFortranFileModel();
	}
	
	private static class FuzzyListener extends FuzzyFortranBaseListener {

		private final Path fname;
		private final FortranFileModel model = new FortranFileModel();
		private Set<String> localCallees = new HashSet<>();

		private FuzzyListener(Path fname){
			this.fname = fname;
		}
		
		public FortranFileModel getFortranFileModel() {
			return model;
		}

		@Override
		public void exitSubprogram(SubprogramContext ctx) {
			SubProgram subprogram = SubProgram.fromValues(ctx.ID(0).getText(), 
					ctx.getStart().getLine(), 
					ctx.getStop().getLine(), 
					fname, 
					localCallees);
			localCallees.clear();
			model.addSubprogram(subprogram);
		}

		@Override
		public void exitCallStatement(CallStatementContext ctx) {
			localCallees.add(ctx.ID().getText());
		}

		@Override
		public void exitIfOneLine(IfOneLineContext ctx) {
			if (ctx.callStatement() != null) {		
				Set<String> identifiers = ctx.block().ID()
						.stream()
						.map(x -> x.getText())
						.collect(toSet());
				model.addControlVariables(identifiers, Collections.singleton(ctx.callStatement().ID().getText()));
			}
		}

		@Override
		public void exitIfStatement(IfStatementContext ctx) {
			Set<String> callees = ctx.body()
					.stream()
					.flatMap(x -> x.callStatement()
							.stream()
							.map(y -> y.ID().getText())
					)
					.collect(toSet());
			if (!callees.isEmpty()) {
				Set<String> identifiers = ctx.block(0).ID()
						.stream()
						.map(x -> x.getText())
						.collect(toSet());
				model.addControlVariables(identifiers, callees);
			}
		}
	
	}

}
