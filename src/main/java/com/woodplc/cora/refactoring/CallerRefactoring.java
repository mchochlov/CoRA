package com.woodplc.cora.refactoring;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStreamRewriter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.google.common.collect.Sets.SetView;
import com.google.common.io.MoreFiles;
import com.woodplc.cora.data.DuplicateKeyException;
import com.woodplc.cora.data.OrderedCaseInsensitiveSet;
import com.woodplc.cora.grammar.Fortran77Lexer;
import com.woodplc.cora.grammar.Fortran77Parser;
import com.woodplc.cora.grammar.Fortran77Parser.AssignmentStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.BindingStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.CallStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.CommonBlockContext;
import com.woodplc.cora.grammar.Fortran77Parser.CommonStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.FunctionStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.FunctionSubprogramContext;
import com.woodplc.cora.grammar.Fortran77Parser.IdentifierContext;
import com.woodplc.cora.grammar.Fortran77Parser.IfStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.ImplicitStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.InterfaceDefinitionContext;
import com.woodplc.cora.grammar.Fortran77Parser.LogicalIfStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.OnlyListItemContext;
import com.woodplc.cora.grammar.Fortran77Parser.SubroutineStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.SubroutineSubprogramContext;
import com.woodplc.cora.grammar.Fortran77Parser.TypeStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.TypeStatementNameContext;
import com.woodplc.cora.grammar.Fortran77Parser.TypeStatementNameListContext;
import com.woodplc.cora.grammar.Fortran77Parser.UseStatementContext;
import com.woodplc.cora.grammar.Fortran77ParserBaseListener;

class CallerRefactoring extends AbstractFortran77Refactoring implements Refactoring {
	
	private final Path path;
	private final Stream<String> originalSubprogram;
	private final RWARefactoring refactoring;
	private CallerRewriter callerRewriter;
	
	CallerRefactoring(Path path, Stream<String> content, RWARefactoring refactoring) {
		this.path = path;
		this.originalSubprogram = content;
		this.refactoring = refactoring;
		
	}

	@Override
	public List<String> refactor() {
		FortranFormatter fr = null;
		Fortran77Lexer lexer = new Fortran77Lexer(CharStreams.fromString(originalSubprogram.collect(Collectors.joining())));
		if (MoreFiles.getFileExtension(path).toLowerCase().equals("for")) {
			lexer.fixedForm = true;
			fr = FortranFormatter.ofFixedForm();
		} else {
			lexer.fixedForm = false;
			fr = FortranFormatter.ofFreeForm();
		}
		CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
		TokenStreamRewriter rewriter = new TokenStreamRewriter(commonTokenStream);
		Fortran77Parser parser = new Fortran77Parser(commonTokenStream);
		ParseTree tree = parser.program();
		ParseTreeWalker ptw = new ParseTreeWalker();
		callerRewriter = new CallerRewriter(rewriter, refactoring, fr); 
		ptw.walk(callerRewriter, tree);
		return Arrays.asList(rewriter.getText().split(NEW_LINE));	
	}
	
	private static class CallerRewriter extends Fortran77ParserBaseListener {
		
		final Logger logger = LoggerFactory.getLogger(CallerRewriter.class);

		private final TokenStreamRewriter rw;
		private final RWARefactoring refactoring;
		private final FortranFormatter fr;
		private final SetMultimap<String, String> savedModuleVariables;
		private final OrderedCaseInsensitiveSet<String> savedArgumentSet;
		private final Map<String, OrderedCaseInsensitiveSet<String>> savedCommonStatements;
		private final Map<String, String> savedTypeStatements;
		private final Set<String> existingIdentifiers;
		
		private int useStatementOffset = 0;
		private Token useStatementInsertionToken = null;
		private int commonStatementOffset = 0;
		private Token commonStatementInsertionToken = null;
		private Token implicitToken = null;
		private Token subprogramDefToken = null;

		private final List<String> useStatementErrors;
		private final List<String> commonStatementErrors;
		
		private final Stack<ParserRuleContext> scope;

		public CallerRewriter(TokenStreamRewriter rewriter, RWARefactoring refactoring, FortranFormatter fr) {
			this.rw = rewriter;
			this.refactoring = refactoring;
			this.fr = fr;
			this.savedModuleVariables = SetMultimapBuilder
					.treeKeys(String.CASE_INSENSITIVE_ORDER)
					.treeSetValues(String.CASE_INSENSITIVE_ORDER)
					.build(refactoring.getSavedModuleVariables());
			this.savedArgumentSet = OrderedCaseInsensitiveSet.from(refactoring.getArgumentSet());
			useStatementErrors = new ArrayList<>();
			commonStatementErrors = new ArrayList<>();
			savedCommonStatements = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
			savedCommonStatements.putAll(refactoring.getSavedCommonStatements());
			savedTypeStatements = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
			savedTypeStatements.putAll(refactoring.getTypestatements());
			scope = new Stack<>();
			existingIdentifiers = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
		}		
		
		@Override
		public void exitImplicitStatement(ImplicitStatementContext ctx) {
			implicitToken = ctx.getStop();
		}
		
		@Override
		public void enterInterfaceDefinition(InterfaceDefinitionContext ctx) {
			scope.push(ctx);
		}

		@Override
		public void exitInterfaceDefinition(InterfaceDefinitionContext ctx) {
			scope.pop();
		}

		@Override
		public void exitTypeStatement(TypeStatementContext ctx) {
			if (!scope.isEmpty()) return;
			if (commonStatementInsertionToken == null) {
				commonStatementInsertionToken = ctx.getStart();
				commonStatementOffset = commonStatementInsertionToken.getCharPositionInLine();
			}
		}
		
		@Override
		public void exitSubroutineStatement(SubroutineStatementContext ctx) {
			if (!scope.isEmpty()) return;
			useStatementInsertionToken = ctx.getStop();
			logger.info("Caller refactoring in " + ctx.subName().getText());
		}
		
		@Override
		public void exitFunctionStatement(FunctionStatementContext ctx) {
			if (!scope.isEmpty()) return;
			useStatementInsertionToken = ctx.getStop();
		}

		@Override
		public void exitCommonStatement(CommonStatementContext ctx) {
			if (!scope.isEmpty()) return;

			if (ctx.commonBlock() == null || ctx.commonBlock().isEmpty()) {
				throw new UnsupportedOperationException();
			}
			for (CommonBlockContext cbc : ctx.commonBlock()) {
				String blockName = cbc.commonName().identifier().getText();
				if (savedCommonStatements.containsKey(blockName)) {
					OrderedCaseInsensitiveSet<String> blockItems = savedCommonStatements.get(blockName);
					if (blockItems.size() != cbc.commonItems().commonItem().size()) {
						commonStatementErrors.add("Unequal common size: /" + blockName +
								"/ " + blockItems.allValues());
						
					} else {
						for (int i = 0; i < cbc.commonItems().commonItem().size(); i++) {
							if (!blockItems.get(i).equalsIgnoreCase(cbc.commonItems().commonItem(i).getText())) {
								commonStatementErrors.add("Different common item name: /" + blockName +
										"/ " + blockItems.get(i));
								break;
							}
						}
					}
					savedCommonStatements.remove(blockName);
				}			
			}

		}

		@Override
		public void exitCallStatement(CallStatementContext ctx) {
			if (!scope.isEmpty()) return;

			if (ctx.subroutineCall().identifier().getText().equalsIgnoreCase(refactoring.subprogramName()) && !savedArgumentSet.isEmpty()) {
				int initialArgumentListOffset = ctx.subroutineCall().identifier().getStart().getCharPositionInLine() + ctx.subroutineCall().identifier().getText().length();
				if (ctx.subroutineCall().callArgumentList() == null) {
					String argList = fr.formatArgumentList(savedArgumentSet.nonNullValues(), initialArgumentListOffset);
					rw.insertAfter(ctx.subroutineCall().identifier().getStart(), argList);
				} else {
					if (ctx.subroutineCall().callArgumentList().callArgument().size() ==
							savedArgumentSet.allValues().size() && 
							savedArgumentSet.allValues().size() == savedArgumentSet.nonNullValues().size()) {
						logger.info("Caller argument list is similar: nothing to refactor.");
						return;
					}
					List<String> adjustedArgList = new ArrayList<>();
					for (int i = 0; i < savedArgumentSet.size(); i++) {
						if (savedArgumentSet.get(i) != null) {
							if (ctx.subroutineCall().callArgumentList().callArgument().size() > i) {
								adjustedArgList.add(ctx.subroutineCall().callArgumentList().callArgument(i).getText());
							} else {
								adjustedArgList.add(savedArgumentSet.get(i));
							}
						}
					}
					String argList = fr.formatArgumentList(adjustedArgList, initialArgumentListOffset);
					rw.replace(ctx.subroutineCall().LPAREN().getSymbol(), ctx.subroutineCall().RPAREN().getSymbol(), argList);					
				}
				if (ctx.getParent().getParent() instanceof LogicalIfStatementContext) {
					IfStatementContext ifsc = (IfStatementContext) ctx.getParent().getParent().getParent();
					String originalCall = fr.commentStatement(rw.getTokenStream().getText(ifsc), 
							ifsc.getStart().getCharPositionInLine());
					rw.insertBefore(ifsc.IF().getSymbol(), originalCall);
				} else {
					String originalCall = fr.commentStatement(rw.getTokenStream().getText(ctx), ctx.getStart().getCharPositionInLine());
					rw.insertBefore(ctx.CALL().getSymbol(), originalCall);
				}
			}
		}

		@Override
		public void exitUseStatement(UseStatementContext ctx) {	
			if (!scope.isEmpty()) return;

			String moduleName = ctx.identifier().getText();
			if (savedModuleVariables.containsKey(moduleName)) {
				// entire module included
				if ((ctx.onlyList() == null || ctx.onlyList().isEmpty()) && (ctx.renameList() == null || ctx.renameList().isEmpty())) {
					savedModuleVariables.removeAll(moduleName);
				} else if (ctx.onlyList() != null) {
					for (OnlyListItemContext olic : ctx.onlyList().onlyListItem()) {
						if (olic.identifier() != null) {
							savedModuleVariables.remove(moduleName, olic.identifier().getText());
						} else if (olic.bindingStatement() != null) {
							savedModuleVariables.remove(moduleName, olic.bindingStatement().expression1(1).identifier().getText());	
							try {
								savedArgumentSet.update(olic.bindingStatement().expression1(1).identifier().getText(),
										olic.bindingStatement().expression1(0).identifier().getText());
							} catch (DuplicateKeyException e) {
								useStatementErrors.add(e.getMessage());
							}
						} else {
							throw new IllegalStateException();
						}
					}
				} else if (ctx.renameList() != null) {
					for (BindingStatementContext bsc : ctx.renameList().bindingStatement()) {
						try {
							savedArgumentSet.update(bsc.expression1(1).identifier().getText(),
									bsc.expression1(0).identifier().getText());
						} catch (DuplicateKeyException e) {
							useStatementErrors.add(e.getMessage());
						}
					}
					savedModuleVariables.removeAll(moduleName);
				} else {
					throw new IllegalStateException();
				}
			}
			useStatementInsertionToken = ctx.getStop();
			useStatementOffset = ctx.getStart().getCharPositionInLine();
		}
	
		@Override
		public void exitSubroutineSubprogram(SubroutineSubprogramContext ctx) {
			onSubprogramExit();
		}
		
		@Override
		public void exitFunctionSubprogram(FunctionSubprogramContext ctx) {
			onSubprogramExit();
		}

		@Override
		public void exitTypeStatementNameList(TypeStatementNameListContext ctx) {
			for (TypeStatementNameContext tsnc : ctx.typeStatementName()) {
				if (tsnc.identifier()!= null && !tsnc.identifier().isEmpty()) {
					existingIdentifiers.add(tsnc.getText());
				} else if (tsnc.arrayDeclarator() != null && !tsnc.arrayDeclarator().isEmpty()) {
					existingIdentifiers.add(tsnc.arrayDeclarator().identifier().getText());
				}
			}
			
			for (AssignmentStatementContext asc : ctx.assignmentStatement()) {
				if (asc.lhsExpression().expression1().identifier() != null && !asc.lhsExpression().expression1().identifier().isEmpty()) {
					existingIdentifiers.add(asc.lhsExpression().expression1().identifier().getText());
				}
			}
		}

		private void onSubprogramExit() {
			if (!scope.isEmpty()) return;

			//check for name conflicts in added use statements
			SetView<String> sv = Sets.intersection(existingIdentifiers, savedModuleVariables.keySet());
			Set<String> ts = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
			if (!sv.isEmpty()) {
				useStatementErrors.add("Added module names have name conflicts: " + sv.toString());
			}
			ts.addAll(savedModuleVariables.values());
			sv = Sets.intersection(existingIdentifiers, ts);
			if (!sv.isEmpty()) {
				useStatementErrors.add("Added module variable names have name conflicts: " + sv.toString());
			}
			
			//and in common statements
			sv = Sets.intersection(existingIdentifiers, savedCommonStatements.keySet());
			if (!sv.isEmpty()) {
				commonStatementErrors.add("Added common names have name conflicts: " + sv.toString());
			}
			ts.clear();
			savedCommonStatements.values().forEach(x -> ts.addAll(x.nonNullValues()));
			sv = Sets.intersection(existingIdentifiers, ts);
			if (!sv.isEmpty()) {
				commonStatementErrors.add("Added common variable names have name conflicts: " + sv.toString());
			}
			
			//append use statements, if any
			StringBuilder output = new StringBuilder();
			for (String error : useStatementErrors) {
				output.append(fr.commentStatement(error, useStatementOffset));
			}
			String useStatements = fr.formatUseStatements(savedModuleVariables.asMap().entrySet(), useStatementOffset);
			rw.insertAfter(useStatementInsertionToken, output.append(useStatements).toString());
			
			//append common statements if any
			output = new StringBuilder();
			for (String error : commonStatementErrors) {
				output.append(fr.commentStatement(error, commonStatementOffset));
			}
			String commonStatements = fr.formatCommonStatements(savedCommonStatements, savedTypeStatements, commonStatementOffset);
			if (commonStatementInsertionToken != null) {
				rw.insertBefore(commonStatementInsertionToken, output.append(commonStatements).toString());				
			} else if (implicitToken != null) {
				rw.insertAfter(implicitToken, output.append(commonStatements).toString());
			} else {
				throw new IllegalStateException();
			}
		}

	}

	@Override
	public Collection<? extends String> getErrors() {
		List<String> list = new ArrayList<>();
		list.addAll(callerRewriter.useStatementErrors);
		list.addAll(callerRewriter.commonStatementErrors);
		return list;
	}

}
