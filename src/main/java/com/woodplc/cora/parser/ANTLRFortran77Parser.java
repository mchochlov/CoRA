package com.woodplc.cora.parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.common.io.MoreFiles;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.grammar.Fortran77Lexer;
import com.woodplc.cora.grammar.Fortran77Parser;
import com.woodplc.cora.ir.IREngine;

class ANTLRFortran77Parser implements Parser {

	private final IREngine engine;
	
	public ANTLRFortran77Parser(IREngine engine) {
		this.engine = engine;
	}

	@Override
	public SDGraph parse(Path path) {
		System.out.println(path.toString());
		Objects.requireNonNull(path);
		
		CharStream stream = null;
		try {
			stream = CharStreams.fromString(new String(Files.readAllBytes(path)));
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		
		Fortran77Lexer lexer = new Fortran77Lexer(stream);
		if (MoreFiles.getFileExtension(path).toLowerCase().equals("for")) {
			lexer.fixedForm = true;
		} else {
			lexer.fixedForm = false;
		}
		
		CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
		
		//TokenStreamRewriter rewriter = new TokenStreamRewriter(commonTokenStream);
		
		Fortran77Parser parser = new Fortran77Parser(commonTokenStream);
		
		ParseTree tree = parser.program();
		
		return null;
/*		ParseTreeWalker ptw = new ParseTreeWalker();
		ptw.walk(new SubprogramModifier(rewriter), tree);
		System.out.println("Tokens size: " + commonTokenStream.size());
		System.out.println("Tree size: " + Trees.findAllNodes(tree, parser.getRuleIndex("RULE_program"), true).size());
		System.out.println("Tree size no leaves: " + Trees.findAllNodes(tree, 0, false));
		return rewriter.getText();		*/	
		
/*		CharStream stream = null;
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
*/	}
	
/*	private static class FuzzyListener extends FuzzyFortranBaseListener {

		private static final Set<String> RELATIONAL_LOGICAL_EXPRESSSIONS = new HashSet<>(Arrays.asList(
				"lt", "le", "eq", "ne", "gt", "ge", "and", "or", "neqv", "xor", "eqv", "not"
				));
		
		private final CharStream charStream;
		private final Path fname;
		private final IREngine engine;
		private final SDGraph graph = Graphs.newInstance();
		private final Set<String> localCallees = new HashSet<>();

		private FuzzyListener(Path fname, IREngine engine, CharStream charStream){
			this.fname = fname;
			this.engine = engine;
			this.charStream = charStream;
		}
		
		private SDGraph getSDGraph() {return graph;}
		
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
			engine.index(subprogram.name(), charStream.getText(Interval.of(ctx.start.getStartIndex(), ctx.stop.getStopIndex())));
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
	
	}*/

}
