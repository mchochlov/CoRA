package com.woodplc.cora.parser;

import static java.util.stream.Collectors.toSet;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.tree.Trees;

import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.ModuleVariable;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.grammar.FuzzyFortranBaseListener;
import com.woodplc.cora.grammar.FuzzyFortranLexer;
import com.woodplc.cora.grammar.FuzzyFortranParser.AllocateStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.AssignmentStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.CallStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.IdentifierContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.IfOneLineContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.IfStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.ModuleContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.ModuleStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.PrivateStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.PublicStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.SubprogramContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.TypeStatementContext;
import com.woodplc.cora.grammar.FuzzyFortranParser.TypeStatementNameContext;
import com.woodplc.cora.ir.IREngine;

class FuzzyListener extends FuzzyFortranBaseListener {

	private static final Set<String> RELATIONAL_LOGICAL_EXPRESSSIONS = new HashSet<>(Arrays.asList(
			"lt", "le", "eq", "ne", "gt", "ge", "and", "or", "neqv", "xor", "eqv", "not"
			));
	
	private final CharStream charStream;
	private final Path fname;
	private final IREngine engine;
	protected final SDGraph graph = Graphs.newInstance();
	private final Set<String> localCallees = new HashSet<>();
	private String currentModule = null;
	private boolean privateSpecifier = false;
	private Set<String> currentPublicSubprograms = new HashSet<>();
	private Set<String> collectionTypes = new HashSet<>();

	FuzzyListener(Path fname, IREngine engine, CharStream charStream){
		this.fname = fname;
		this.engine = engine;
		this.charStream = charStream;
	}
	
	@Override
	public void exitTypeStatement(TypeStatementContext ctx) {
		//System.out.println(ctx.getText());
		if (!ctx.accessSpecifier().isEmpty() && ctx.accessSpecifier(0).getText().equals("private")) {
			return;
		}
		
		for (TypeStatementNameContext tsnc: ctx.typeStatementNameList().typeStatementName()) {
			ModuleVariable mv = new ModuleVariable(ctx.typename().getText());
			if (tsnc.identifier() != null && !tsnc.identifier().isEmpty()) {
				if (!ctx.ALLOCATABLE().isEmpty()) {
					collectionTypes.add(tsnc.identifier().getText());
				} else if (!ctx.dimensionStatement().isEmpty()) {
					setAllocation(mv, ctx.dimensionStatement(0).arrayDeclaratorExtents());
				}
				graph.addModuleVariable(currentModule, tsnc.identifier().getText(), mv);
			} else if (tsnc.arrayDeclarator() != null && !tsnc.arrayDeclarator().isEmpty()) {
				if (!ctx.ALLOCATABLE().isEmpty()) {
					collectionTypes.add(tsnc.arrayDeclarator().identifier().getText());
				} else {
					setAllocation(mv, tsnc.arrayDeclarator().arrayDeclaratorExtents());
				}
				graph.addModuleVariable(currentModule, tsnc.arrayDeclarator().identifier().getText(), mv);
			} else {
				throw new IllegalStateException();
			}	
		}
		
		for (AssignmentStatementContext asc : ctx.typeStatementNameList().assignmentStatement()) {
			//System.out.println(currentModule + " " + asc.getText());
			ModuleVariable mv = new ModuleVariable(ctx.typename().getText());
			if (asc.expression1(0).identifier() != null && !asc.expression1(0).identifier().isEmpty()) {
				if (!ctx.ALLOCATABLE().isEmpty()) {
					collectionTypes.add(asc.expression1(0).identifier().getText());
				} else if (asc.expression1(0).exprList1() != null && !asc.expression1(0).exprList1().isEmpty()) {
					setAllocation(mv, asc.expression1(0).exprList1());
				} else if (!ctx.dimensionStatement().isEmpty()) {
					setAllocation(mv, ctx.dimensionStatement(0).arrayDeclaratorExtents());
				}
				
				graph.addModuleVariable(currentModule, asc.expression1(0).identifier().getText(), mv);
			} else {
				throw new IllegalStateException();
			}	
		}
	}

	@Override
	public void enterModuleStatement(ModuleStatementContext ctx) {
		currentModule = ctx.identifier().getText();
		currentPublicSubprograms.clear();
		collectionTypes.clear();
		privateSpecifier = false;
		graph.addModule(currentModule);
		//System.out.println(ctx.identifier().getText());
	}

	@Override
	public void exitPublicStatement(PublicStatementContext ctx) {
		//System.out.println(ctx.getText());
		for (IdentifierContext ic : ctx.identifier()) {
			currentPublicSubprograms.add(ic.getText());
		}
	}

	@Override
	public void exitAllocateStatement(AllocateStatementContext ctx) {
	
		//System.out.println("ALLOCATE EXIT: " +  fname + ctx.getText());
		//ignore allocation of class members
		if (ctx.getText().contains("%")) return;
		String allocatedVariable = ctx.exprList1().expression1(0).identifier().getText();
	
		if (collectionTypes.contains(allocatedVariable)) {
			ModuleVariable mv = graph.getModuleVariable(currentModule, allocatedVariable);
			if (mv == null) throw new IllegalStateException();
			
			setAllocation(mv, ctx.exprList1().expression1(0).exprList1());
		}
	}
	
	private void setAllocation(ModuleVariable mv, ParserRuleContext ctx) {
		if (ctx == null || ctx.isEmpty()) return;
		mv.setAllocation(ctx.getText());
		List<String> allocationParameters = Trees.findAllTokenNodes(ctx, FuzzyFortranLexer.ID)
				.stream()
				.map(x -> x.getText())
				.collect(Collectors.toList());
		mv.setAllocationParameters(allocationParameters);
	}
	
	protected SDGraph getSDGraph() {return graph;}
	
	@Override
	public void exitSubprogram(SubprogramContext ctx) {
		String module = "";
		if (ctx.getParent() != null && ctx.getParent() instanceof ModuleContext) {
			ModuleContext mc = (ModuleContext) ctx.getParent();
			module = mc.moduleStatement().identifier().getText();
		}
		SubProgram subprogram = SubProgram.ofType(ctx.subType(0).getText(),
				module, ctx.identifier(0).getText(), 
				ctx.getStart().getLine(), 
				ctx.getStop().getLine(), 
				fname);
		
		graph.addSubprogramAndCallees(subprogram, new HashSet<>(localCallees));
		if (currentPublicSubprograms.contains(subprogram.name()) || !privateSpecifier) {
			ModuleVariable mv = new ModuleVariable("external");
			graph.addModuleVariable(currentModule, subprogram.name(), mv);
		}
		if (engine != null) {
			engine.index(subprogram.name(), charStream.getText(Interval.of(ctx.start.getStartIndex(), ctx.stop.getStopIndex())));			
		}
		localCallees.clear();
	}

	@Override
	public void exitPrivateStatement(PrivateStatementContext ctx) {
		if (ctx.identifier().isEmpty()) {
			privateSpecifier = true;
		}
	}

	@Override
	public void exitCallStatement(CallStatementContext ctx) {
		localCallees.add(ctx.identifier().getText());
	}

	@Override
	public void exitIfOneLine(IfOneLineContext ctx) {
		if (ctx.callStatement() != null) {		
			Set<String> identifiers = ctx.block().ID()
					.stream()
					.map(TerminalNode::getText)
					.filter(x -> !RELATIONAL_LOGICAL_EXPRESSSIONS.contains(x))
					.collect(toSet());
			graph.addVariablesAndCallees(identifiers, Collections.singleton(ctx.callStatement().identifier().getText()));
		}
	}

	@Override
	public void exitIfStatement(IfStatementContext ctx) {
		Set<String> callees = ctx.body()
				.stream()
				.flatMap(x -> x.callStatement()
						.stream()
						.map(y -> y.identifier().getText())
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