package com.woodplc.cora.parser;

import static java.util.stream.Collectors.toSet;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
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
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;

import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.grammar.FuzzyFortranFastBaseListener;
import com.woodplc.cora.grammar.FuzzyFortranFastLexer;
import com.woodplc.cora.grammar.FuzzyFortranFastParser;
import com.woodplc.cora.grammar.FuzzyFortranFastParser.CallStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranFastParser.IfOneLineContext;
import com.woodplc.cora.grammar.FuzzyFortranFastParser.IfStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranFastParser.ModuleContext;
import com.woodplc.cora.grammar.FuzzyFortranFastParser.SubprogramContext;
import com.woodplc.cora.grammar.FuzzyFortranLexer;
import com.woodplc.cora.grammar.FuzzyFortranParser;
import com.woodplc.cora.ir.IREngine;

class ANTLRFortranParser implements Parser {

	private final IREngine engine;
	private static final String MODULE_MARKER = "end module";
	
	public ANTLRFortranParser(IREngine engine) {
		this.engine = engine;
	}

	public ANTLRFortranParser() {
		this.engine = null;
	}

	@Override
	public SDGraph parse(Path path) {
		Objects.requireNonNull(path);
		
		CharStream stream = null;
		String data = null; 
		try {
			data = new String(Files.readAllBytes(path));
			stream = CharStreams.fromString(data);
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		if (data.contains(MODULE_MARKER) || data.contains("END MODULE")) {
			FuzzyFortranLexer lexer = new FuzzyFortranLexer(stream);
			CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
			FuzzyFortranParser parser = new FuzzyFortranParser(commonTokenStream);	
			parser.setErrorHandler(new BailErrorStrategy());
			parser.removeErrorListeners();
			parser.getInterpreter().setPredictionMode(PredictionMode.SLL);
			FuzzyListener fl = new FuzzyListener(path, engine, stream);
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
			return fl.getSDGraph();
		} else {
			FuzzyFortranFastLexer lexer = new FuzzyFortranFastLexer(stream);
			CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
			FuzzyFortranFastParser parser = new FuzzyFortranFastParser(commonTokenStream);
			parser.setErrorHandler(new BailErrorStrategy());
			parser.removeErrorListeners();
			parser.getInterpreter().setPredictionMode(PredictionMode.SLL);
			FuzzyListenerFast fl = new FuzzyListenerFast(path, engine, stream);
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
			return fl.getSDGraph();
		}	
	}
	
	private static class FuzzyListenerFast extends FuzzyFortranFastBaseListener {

		private static final Set<String> RELATIONAL_LOGICAL_EXPRESSSIONS = new HashSet<>(Arrays.asList(
				"lt", "le", "eq", "ne", "gt", "ge", "and", "or", "neqv", "xor", "eqv", "not"
				));
		
		private final CharStream charStream;
		private final Path fname;
		private final IREngine engine;
		protected final SDGraph graph = Graphs.newInstance();
		private final Set<String> localCallees = new HashSet<>();

		private FuzzyListenerFast(Path fname, IREngine engine, CharStream charStream){
			this.fname = fname;
			this.engine = engine;
			this.charStream = charStream;
		}
		
		protected SDGraph getSDGraph() {return graph;}
		
		@Override
		public void exitSubprogram(SubprogramContext ctx) {
			String module = "";
			if (ctx.getParent() != null && ctx.getParent() instanceof ModuleContext) {
				ModuleContext mc = (ModuleContext) ctx.getParent();
				module = mc.ID(0).getText();
			}
			SubProgram subprogram = SubProgram.ofType(ctx.subType(0).getText(),
					module, ctx.ID(0).getText(), 
					ctx.getStart().getLine(), 
					ctx.getStop().getLine(), 
					fname);
			
			graph.addSubprogramAndCallees(subprogram, new HashSet<>(localCallees));
			if(engine !=  null) {
				engine.index(subprogram.name(), charStream.getText(Interval.of(ctx.start.getStartIndex(), ctx.stop.getStopIndex())));
			}
			localCallees.clear();
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
						.map(TerminalNode::getText)
						.filter(x -> !RELATIONAL_LOGICAL_EXPRESSSIONS.contains(x))
						.collect(toSet());
				graph.addVariablesAndCallees(identifiers, Collections.singleton(ctx.callStatement().ID().getText()));
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
						.map(TerminalNode::getText)
						.filter(x -> !RELATIONAL_LOGICAL_EXPRESSSIONS.contains(x))
						.collect(toSet());
				graph.addVariablesAndCallees(identifiers, callees);
			}
		}
	
	}


}
