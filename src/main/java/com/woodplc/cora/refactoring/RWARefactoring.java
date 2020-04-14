package com.woodplc.cora.refactoring;

import static java.util.stream.Collectors.partitioningBy;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStreamRewriter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.tree.Trees;

import com.google.common.base.Strings;
import com.google.common.io.MoreFiles;
import com.woodplc.cora.data.DefAndUseMapper;
import com.woodplc.cora.data.ModuleVariable;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.grammar.Fortran77Lexer;
import com.woodplc.cora.grammar.Fortran77Parser;
import com.woodplc.cora.grammar.Fortran77Parser.ArrayDeclaratorExtentContext;
import com.woodplc.cora.grammar.Fortran77Parser.AssignmentStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.CommonItemContext;
import com.woodplc.cora.grammar.Fortran77Parser.CommonStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.ExecutableStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.FunctionStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.FunctionSubprogramContext;
import com.woodplc.cora.grammar.Fortran77Parser.IdentifierContext;
import com.woodplc.cora.grammar.Fortran77Parser.IntentAttributeContext;
import com.woodplc.cora.grammar.Fortran77Parser.IntentStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.LhsExpressionContext;
import com.woodplc.cora.grammar.Fortran77Parser.NamelistContext;
import com.woodplc.cora.grammar.Fortran77Parser.OnlyListItemContext;
import com.woodplc.cora.grammar.Fortran77Parser.SubNameContext;
import com.woodplc.cora.grammar.Fortran77Parser.SubroutineStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.SubroutineSubprogramContext;
import com.woodplc.cora.grammar.Fortran77Parser.TypeStatementContext;
import com.woodplc.cora.grammar.Fortran77Parser.TypeStatementNameContext;
import com.woodplc.cora.grammar.Fortran77Parser.UseStatementContext;
import com.woodplc.cora.grammar.Fortran77ParserBaseListener;

class RWARefactoring implements Refactoring {
	
	private static final String NEW_LINE = "\n";
	private final Path path;
	private final Stream<String> originalSubprogram;
	private final SDGraph systemGraph;
	private final SDGraph cafGraph;

	public RWARefactoring(Path path, Stream<String> originalSubprogram, SDGraph systemGraph, SDGraph cafGraph) {
		this.path = path;
		this.originalSubprogram = originalSubprogram;
		this.systemGraph = systemGraph;
		this.cafGraph = cafGraph;
	}

	@Override
	public List<String> refactor() {
		Fortran77Lexer lexer = new Fortran77Lexer(CharStreams.fromString(originalSubprogram.collect(Collectors.joining(NEW_LINE))));
		if (MoreFiles.getFileExtension(path).toLowerCase().equals("for")) {
			lexer.fixedForm = true;
		} else {
			lexer.fixedForm = false;
		}
		CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
		TokenStreamRewriter rewriter = new TokenStreamRewriter(commonTokenStream);
		Fortran77Parser parser = new Fortran77Parser(commonTokenStream);
		ParseTree tree = parser.program();
		ParseTreeWalker ptw = new ParseTreeWalker();
		ptw.walk(new RWARewriter(rewriter, systemGraph, cafGraph, lexer.fixedForm), tree);
		return Arrays.asList(rewriter.getText().split(NEW_LINE));			
	}

	private static class RWARewriter extends Fortran77ParserBaseListener{

		private final TokenStreamRewriter rw;
		private final SDGraph systemGraph;
		private final SDGraph cafGraph;
		private final boolean isFixedForm;
		private final StringBuilder argumentList;
		private Set<String> argumentSet;
		private final Set<ArgumentVariable> variables;
		private final Set<String> commonItems;
		private final Set<String> existingIntentArguments;
		private Token argumentLastToken;
		private Token firstTypeToken = null;
		private Token lastTypeToken = null;
		private int typeOffset = 0;
		private int useStatementOffset = 0;
		private int currentArgumentListOffset = 0;
		private final List<String> parameterAllocationErrors;
		private final List<ArgumentVariable> localVariables;
		private final List<String> existingIntentStatements;
		
		private static final String MODULE_NOT_FOUND_COMMENT = "! Error: Module not found ";
		private static final String MODULE_VAR_NOT_FOUND_COMMENT = "! Error: Module variable not found ";
		private static final String PARAMETER_ALLOCATION_ERROR = "! Error resolved allocation parameter ";
		private static final int ARGUMENT_OFFSET = 80;
		
		private final DefAndUseMapper defAndUseMapper = new DefAndUseMapper();
		private ExecutableStatementContext currentExecutableStatement = null;
		private boolean isLhsExpression = false;
		
		private String functionName = null;
		private static final String fixedOffset = "     ";

		public RWARewriter(TokenStreamRewriter rewriter, SDGraph systemGraph, SDGraph cafGraph, boolean isFixedForm) {
			this.rw = rewriter; 
			this.systemGraph = systemGraph;
			this.cafGraph = cafGraph;
			this.isFixedForm = isFixedForm;
			argumentList = new StringBuilder();
			this.argumentSet = new HashSet<>();
			this.variables = new HashSet<>();
			this.commonItems = new HashSet<>();
			this.existingIntentArguments = new HashSet<>();
			this.existingIntentStatements = new ArrayList<>(); 
			this.parameterAllocationErrors = new ArrayList<>();
			this.localVariables = new ArrayList<>();
		}

		
		@Override
		public void exitIntentStatement(IntentStatementContext ctx) {
			rw.delete(ctx.getStart(), ctx.getStop());
			existingIntentStatements.add(rw.getTokenStream().getText(ctx.getStart(), ctx.getStop()));
			for (ArrayDeclaratorExtentContext adec : ctx.arrayDeclaratorExtents().arrayDeclaratorExtent()) {
				existingIntentArguments.add(adec.getText());
			}
		}


		@Override
		public void enterSubroutineStatement(SubroutineStatementContext ctx) {
			onEnterSubprogram(ctx.namelist(), ctx.LPAREN(), ctx.subName());
		}
		
		@Override
		public void enterExecutableStatement(ExecutableStatementContext ctx) {
			currentExecutableStatement = ctx;
		}


		@Override
		public void enterIdentifier(IdentifierContext ctx) {
			if (currentExecutableStatement != null && !isLhsExpression) {
				defAndUseMapper.addUse(ctx.getText(), ctx.getStart().getLine(), currentExecutableStatement);
			}
		}

		@Override
		public void exitExecutableStatement(ExecutableStatementContext ctx) {
			currentExecutableStatement = null;
		}


		@Override
		public void enterLhsExpression(LhsExpressionContext ctx) {
			isLhsExpression = true;
			defAndUseMapper.addDefinition(ctx.expression1().identifier().getText(), ctx.getStart().getLine(), currentExecutableStatement);
		}


		@Override
		public void exitLhsExpression(LhsExpressionContext ctx) {
			isLhsExpression = false;
		}

		@Override
		public void enterFunctionStatement(FunctionStatementContext ctx) {
			onEnterSubprogram(ctx.namelist(), ctx.LPAREN(), ctx.subName());
			functionName = ctx.subName().getText();
		}

		private void onEnterSubprogram(NamelistContext nlc, TerminalNode lparen, SubNameContext snc) {
			if (nlc == null || nlc.isEmpty()) {
				if (lparen == null) {
					argumentLastToken = snc.getStop();
					argumentList.append('(');
				} else {
					argumentLastToken = lparen.getSymbol();
				}
			} else {
				argumentLastToken = nlc.getStop();			
				argumentSet = nlc.identifier().stream().map(IdentifierContext::getText).collect(Collectors.toSet());
			}
			currentArgumentListOffset = argumentLastToken.getCharPositionInLine() + argumentLastToken.getText().length();
		}
		
		@Override
		public void exitSubroutineSubprogram(SubroutineSubprogramContext ctx) {
			onSubprogramExit();
		}
		
		@Override
		public void exitFunctionSubprogram(FunctionSubprogramContext ctx) {
			onSubprogramExit();
		}

		private void onSubprogramExit() {
			//System.out.print(defAndUseMapper);
			StringBuilder output = new StringBuilder();
			
			// add common variables to argument list
			for (Iterator<ArgumentVariable> iterator = localVariables.iterator(); iterator.hasNext();) {
				ArgumentVariable av = (ArgumentVariable) iterator.next();
				if (commonItems.contains(av.getName())) {
					if (!defAndUseMapper.isGlobalVariableUnused(av.getName())) {
						variables.add(av);						
						appendArgumentAndAdjustOffset(av.getName());
					}
					iterator.remove();
				}
			}
						
			if (argumentList.charAt(0) == '(') {
				argumentList.append(")");
			}
			rw.insertAfter(argumentLastToken, argumentList.toString());
			String tab = Strings.repeat(" ", typeOffset);
			
			for (String error : parameterAllocationErrors) {
				if (isFixedForm) {
					output.append("C ").append(error).append("\n").append(fixedOffset);
				} else {
					output.append("! ").append(error).append("\n").append(tab);
				}
			}
			
			Map<Boolean, List<ArgumentVariable>> varMap = variables.stream()
				.filter(x -> !defAndUseMapper.isGlobalVariableUnused(x.getName()))
				.sorted(Comparator.comparing(ArgumentVariable::getName))
				.collect(partitioningBy(ArgumentVariable::isScalar));
			for (ArgumentVariable av : varMap.get(true)) {
				appendOutput(av, output, tab);
			}
			
			for (ArgumentVariable av : varMap.get(false)) {
				appendOutput(av, output, tab);
			}
			// Append existing intent
			
			for (String string : existingIntentStatements) {
				if (isFixedForm) {
					output.append("\n").append(fixedOffset + " ").append(string);
				} else {
					output.append("\n").append(tab).append(string);
				}
			}
			
			if (isFixedForm) {
				output.append("\n").append("C ").append("Local variables").append("\n").append(fixedOffset + " ");
			} else {
				output.append("\n").append(tab).append("!Local variables").append("\n").append(tab);
			}
			for (ArgumentVariable av : localVariables) {
				if (defAndUseMapper.isLocalVariableUnused(av.getName())) {
					for (Map.Entry<Integer, ParserRuleContext> entry : defAndUseMapper.getDefinitions(av.getName()).entrySet()) {
						ParserRuleContext ctx = entry.getValue();
						rw.delete(ctx.getStart(), ctx.getStop());
					}
				} else {
					output.append(av.getDeclaration()).append(av.getNameWithDeclaration()).append("\n").append(tab);
				}
			}
			
			if (firstTypeToken == null || lastTypeToken == null) {
				throw new IllegalStateException();
			}
			rw.insertBefore(firstTypeToken, output.toString());
			rw.delete(firstTypeToken, lastTypeToken);
		}

		@Override
		public void exitTypeStatement(TypeStatementContext ctx) {
			if (firstTypeToken == null) {
				firstTypeToken = ctx.getStart();
				typeOffset = firstTypeToken.getCharPositionInLine();
			}
			
			String contextOriginal = rw.getTokenStream().getText(ctx.getStart(), ctx.getStop());
			String nameListOriginal = rw.getTokenStream().getText(ctx.typeStatementNameList().getStart(), ctx.typeStatementNameList().getStop());
			String declaration = contextOriginal.substring(0, contextOriginal.lastIndexOf(nameListOriginal));
			for (TypeStatementNameContext tsnc: ctx.typeStatementNameList().typeStatementName()) {
				String varName;
				String oldStyleArrayDeclaration = "";
				if (tsnc.identifier() != null) {
					varName = tsnc.identifier().getText();
				} else if (tsnc.arrayDeclarator() != null) {
					varName = tsnc.arrayDeclarator().identifier().getText();
					oldStyleArrayDeclaration = "(" + tsnc.arrayDeclarator().arrayDeclaratorExtents().getText() + ")";
				} else {
					throw new IllegalArgumentException();
				}
				if (argumentSet.contains(varName) || (functionName != null && functionName.equalsIgnoreCase(varName))) {
					if (ctx.dimensionStatement().isEmpty() && tsnc.arrayDeclarator() == null) {
						variables.add(ArgumentVariable.ofScalarType(varName, declaration, ctx.intentAttribute()));
					} else if (!oldStyleArrayDeclaration.isEmpty()) {
						variables.add(ArgumentVariable.ofOldStyleArrayType(varName, oldStyleArrayDeclaration, declaration, ctx.intentAttribute()));					
						List<String> allocationParameters = Trees.findAllTokenNodes(tsnc.arrayDeclarator().arrayDeclaratorExtents(), Fortran77Parser.NAME)
								.stream()
								.map(x -> x.getText())
								.collect(Collectors.toList());
						generateAllocationParameters(allocationParameters, ctx);
					} else {
						variables.add(ArgumentVariable.ofArrayType(varName, declaration, ctx.intentAttribute()));
					}					
				} else {
					if (ctx.dimensionStatement().isEmpty() && tsnc.arrayDeclarator() == null) {
						localVariables.add(ArgumentVariable.ofScalarType(varName, declaration, ctx.intentAttribute()));
					} else if (!oldStyleArrayDeclaration.isEmpty()) {
						localVariables.add(ArgumentVariable.ofOldStyleArrayType(varName, oldStyleArrayDeclaration, declaration, ctx.intentAttribute()));
						List<String> allocationParameters = Trees.findAllTokenNodes(tsnc.arrayDeclarator().arrayDeclaratorExtents(), Fortran77Parser.NAME)
								.stream()
								.map(x -> x.getText())
								.collect(Collectors.toList());
						generateAllocationParameters(allocationParameters, ctx);
					} else {
						localVariables.add(ArgumentVariable.ofArrayType(varName, declaration, ctx.intentAttribute()));
					}		
				}
			}
			
			for (AssignmentStatementContext asc : ctx.typeStatementNameList().assignmentStatement()) {
				String varName = asc.lhsExpression().expression1().getText();
				if (argumentSet.contains(varName)) {
					if (ctx.dimensionStatement().isEmpty()) {
						variables.add(ArgumentVariable.ofScalarType(varName, declaration, ctx.intentAttribute()));
					} else {
						variables.add(ArgumentVariable.ofArrayType(varName, declaration, ctx.intentAttribute()));
					}
				} else {
					if (ctx.dimensionStatement().isEmpty()) {
						localVariables.add(ArgumentVariable.ofScalarType(varName, declaration, ctx.intentAttribute()));
					} else {
						localVariables.add(ArgumentVariable.ofArrayType(varName, declaration, ctx.intentAttribute()));
					}		
				}
			}
			lastTypeToken = ctx.getStop();
		}

		@Override
		public void exitUseStatement(UseStatementContext ctx) {
			useStatementOffset = ctx.getStart().getCharPositionInLine();
			String tab = Strings.repeat(" ", useStatementOffset);
			StringBuilder partiallyRefactoredModule = new StringBuilder();
			StringBuilder localErrors = new StringBuilder();
			String moduleName = ctx.identifier().getText();
			// Check module is not part of CAF
			if (!cafGraph.modules().contains(moduleName)) {
				if (systemGraph.modules().contains(moduleName)) {
					for (OnlyListItemContext olic : ctx.onlyList().onlyListItem()) {
						String variableName = olic.identifier().getText();
						ModuleVariable mv = systemGraph.getModuleVariable(moduleName, variableName);
						if (mv != null) {
							variables.add(ArgumentVariable.fromModule(variableName, mv.getType(), mv.getAllocation()));
							if (!mv.isScalar()) generateAllocationParameters(mv.getAllocationParameters(), ctx);
							appendArgumentAndAdjustOffset(variableName);
						} else {
							localErrors.append(MODULE_VAR_NOT_FOUND_COMMENT).append(moduleName)
								.append(" :: ").append(variableName).append("\n").append(tab);
							if (partiallyRefactoredModule.length() == 0) {
								partiallyRefactoredModule.append("use ")
									.append(moduleName)
									.append(", only: ")
									.append(variableName);
							} else {
								partiallyRefactoredModule.append(", ").append(variableName);
							}
						}						
					}
					if (partiallyRefactoredModule.length() != 0) {
						rw.insertBefore(ctx.getStart(), partiallyRefactoredModule.append("\n").append(tab).toString());
						rw.insertBefore(ctx.getStart(), localErrors.toString());
					}
					rw.delete(ctx.getStart(), ctx.getStop());

				} else {
					rw.insertBefore(ctx.getStart(), MODULE_NOT_FOUND_COMMENT + moduleName + "\n" + tab);
				}
			}
		}
		
		
		
		@Override
		public void exitCommonStatement(CommonStatementContext ctx) {
			for (CommonItemContext cic : ctx.commonBlock(0).commonItems().commonItem()) {
				commonItems.add(cic.getText());
			}
			rw.delete(ctx.getStart(), ctx.getStop());
		}


		private void appendArgumentAndAdjustOffset(String variableName) {
			if (!argumentSet.contains(variableName)) {
				if (currentArgumentListOffset + variableName.length() + 3 > ARGUMENT_OFFSET) {
					if (argumentList.toString().equals("(") || (argumentList.length() == 0 && argumentLastToken.getType() == Fortran77Lexer.LPAREN)) {
						if (isFixedForm) {
							argumentList.append("\n").append(fixedOffset).append("& ").append(variableName);
						} else {
							argumentList.append(" &\n\t& ").append(variableName);
						}
						
					} else {
						if (isFixedForm) {
							argumentList.append(",\n").append(fixedOffset).append("& ").append(variableName);
						} else {
							argumentList.append(", &\n\t& ").append(variableName);
						}
					}
					currentArgumentListOffset = 6;
				} else {
					if (argumentList.toString().equals("(") || (argumentList.length() == 0 && argumentLastToken.getType() == Fortran77Lexer.LPAREN)) {
						argumentList.append(variableName);
					} else {
						argumentList.append(", ").append(variableName);	
					}
					currentArgumentListOffset += variableName.length() + 2;
				}	
				argumentSet.add(variableName);
			}
		}


		private void generateAllocationParameters(List<String> allocationParameters, ParserRuleContext ctx) {
			for (String ap : allocationParameters) {
				Map<String, ModuleVariable> mVar = systemGraph.getAllVariableModules(ap);
				if (mVar.size() != 1) {
					parameterAllocationErrors.add(PARAMETER_ALLOCATION_ERROR + ap + " " + mVar.keySet());
				} else {
					mVar.forEach((k,v) -> {
						if (v.isScalar()) {
							variables.add(ArgumentVariable.fromModule(ap, v.getType(), v.getAllocation()));
							defAndUseMapper.addUse(ap, ctx.getStart().getLine(), ctx);
							appendArgumentAndAdjustOffset(ap);
						} else {
							generateAllocationParameters(v.getAllocationParameters(), ctx);
						}
					});
				}
			}
			
		}

		private boolean isVariableFunctionName(String varName) {
			return functionName != null && functionName.equals(varName) ? true : false;
		}

		private StringBuilder appendOutput(ArgumentVariable av, StringBuilder output, String tab) {
			if (av.isExisting()) {
				if (!av.hasIntent && !existingIntentArguments.contains(av.getName())
						&& !isVariableFunctionName(av.getName())) {
					output
					.append(av.getDeclaration().replace("::", "").stripTrailing());
					generateIntent(av, output);
					output.append(" :: ");
				} else {
					output.append(av.getDeclaration());
				}
				output.append(av.getNameWithDeclaration())
					.append("\n").append(tab);
			} else {
				if (av.isScalar()) {
					output
						.append(av.getType());
					generateIntent(av, output);
					output.append(" :: ")
						.append(av.getName())
						.append("\n").append(tab);
				} else {
					output
						.append(av.getType());
					generateIntent(av, output);	
					output.append(", dimension(")
						.append(av.getAllocation())
						.append(") :: ")
						.append(av.getName())
						.append("\n").append(tab);
				}
			}
			return output;
		}
		
		private void generateIntent(ArgumentVariable av, StringBuilder output) {
			if (!av.getType().equals("external")) {
				output.append(", intent(")
				.append(defAndUseMapper.getGlobalVariableIntent(av.getName()))
				.append(")");	
			}
		}
	}
	
	
	
	private final static class ArgumentVariable {
		private final String name;
		private final String type;
		private final String allocation;
		private final String declaration;
		private final String oldStyleArrayDeclaration;
		private final boolean hasIntent;
		
		private ArgumentVariable(String name, String type, String allocation, String declaration, String oldStyleArrayDeclaration, boolean hasIntent) {
			this.name = name;
			this.type = type;
			this.allocation = allocation;
			this.declaration = declaration;
			this.oldStyleArrayDeclaration = oldStyleArrayDeclaration;
			this.hasIntent = hasIntent;
		}

		public static ArgumentVariable fromModule(String variableName, String type, String allocation) {
			return new ArgumentVariable(variableName, type, allocation, "", "", false);
		}

		public static ArgumentVariable ofOldStyleArrayType(String varName, String oldStyleArrayDeclaration,
				String declaration, List<IntentAttributeContext> list) {
			return new ArgumentVariable(varName, "", " ", declaration, oldStyleArrayDeclaration, list != null && !list.isEmpty()? true : false);
		}

		public static ArgumentVariable ofArrayType(String varName, String declaration, List<IntentAttributeContext> list) {
			return new ArgumentVariable(varName, "", " ", declaration, "", list != null && !list.isEmpty()? true : false);
		}

		public static ArgumentVariable ofScalarType(String varName, String declaration, List<IntentAttributeContext> list) {
			return new ArgumentVariable(varName, "", "", declaration, "", list != null && !list.isEmpty()? true : false);
		}

		public boolean isExisting() {
			return !declaration.isEmpty();
		}

		public String getName() {
			return name;
		}
		
		public String getNameWithDeclaration() {
			return name + oldStyleArrayDeclaration;
		}

		public String getType() {
			return type;
		}

		public String getAllocation() {
			return allocation;
		}

		public boolean isScalar() {return allocation.isEmpty();}
		
		public String getDeclaration() {
			return declaration;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((allocation == null) ? 0 : allocation.hashCode());
			result = prime * result + ((declaration == null) ? 0 : declaration.hashCode());
			result = prime * result + (hasIntent ? 1231 : 1237);
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			result = prime * result + ((oldStyleArrayDeclaration == null) ? 0 : oldStyleArrayDeclaration.hashCode());
			result = prime * result + ((type == null) ? 0 : type.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			ArgumentVariable other = (ArgumentVariable) obj;
			if (allocation == null) {
				if (other.allocation != null)
					return false;
			} else if (!allocation.equals(other.allocation))
				return false;
			if (declaration == null) {
				if (other.declaration != null)
					return false;
			} else if (!declaration.equals(other.declaration))
				return false;
			if (hasIntent != other.hasIntent)
				return false;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			if (oldStyleArrayDeclaration == null) {
				if (other.oldStyleArrayDeclaration != null)
					return false;
			} else if (!oldStyleArrayDeclaration.equals(other.oldStyleArrayDeclaration))
				return false;
			if (type == null) {
				if (other.type != null)
					return false;
			} else if (!type.equals(other.type))
				return false;
			return true;
		}

		@Override
		public String toString() {
			return "ArgumentVariable [name=" + name + ", type=" + type + ", allocation=" + allocation + ", declaration="
					+ declaration + ", oldStyleArrayDeclaration=" + oldStyleArrayDeclaration + ", hasIntent="
					+ hasIntent + "]";
		}

		
	}
}
