package com.woodplc.cora.refactoring;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import com.google.common.io.MoreFiles;
import com.woodplc.cora.grammar.Fortran77Lexer;
import com.woodplc.cora.grammar.Fortran77Parser;
import com.woodplc.cora.grammar.Fortran77Parser.CallStatementContext;
import com.woodplc.cora.grammar.Fortran77ParserBaseListener;

class CallerRefactoring extends AbstractFortran77Refactoring implements Refactoring {
	
	private final Path path;
	private final Stream<String> originalSubprogram;
	private final RWARefactoring refactoring;
	
	CallerRefactoring(Path path, Stream<String> content, RWARefactoring refactoring) {
		this.path = path;
		this.originalSubprogram = content;
		this.refactoring = refactoring;
	}

	@Override
	public List<String> refactor() {
		FortranFormatter fr = null;
		Fortran77Lexer lexer = new Fortran77Lexer(CharStreams.fromString(originalSubprogram.collect(Collectors.joining(NEW_LINE))));
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
		ptw.walk(new CallerRewriter(rewriter, refactoring, fr), tree);
		return Arrays.asList(rewriter.getText().split(NEW_LINE));	
	}
	
	private static class CallerRewriter extends Fortran77ParserBaseListener {

		private final TokenStreamRewriter rw;
		private final RWARefactoring refactoring;
		private final FortranFormatter fr;
		
		public CallerRewriter(TokenStreamRewriter rewriter, RWARefactoring refactoring, FortranFormatter fr) {
			this.rw = rewriter;
			this.refactoring = refactoring;
			this.fr = fr;
		}

		@Override
		public void exitCallStatement(CallStatementContext ctx) {
			if (ctx.subroutineCall().identifier().getText().equalsIgnoreCase(refactoring.subprogramName()) && !refactoring.isArgListEmpty()) {
				int initialArgumentListOffset = ctx.subroutineCall().identifier().getStart().getCharPositionInLine() + ctx.subroutineCall().identifier().getText().length();
				if (ctx.subroutineCall().callArgumentList() == null) {
					String argList = fr.formatArgumentList(refactoring.getArgumentListNonNull(), initialArgumentListOffset);
					rw.insertAfter(ctx.subroutineCall().identifier().getStart(), argList);
				} else {
					List<String> adjustedArgList = new ArrayList<>();
					for (int i = 0; i < refactoring.getArgumentListSize(); i++) {
						if (refactoring.getArgument(i) != null) {
							if (ctx.subroutineCall().callArgumentList().callArgument().size() > i) {
								adjustedArgList.add(ctx.subroutineCall().callArgumentList().callArgument(i).getText());
							} else {
								adjustedArgList.add(refactoring.getArgument(i));
							}
						}
					}
					String argList = fr.formatArgumentList(adjustedArgList, initialArgumentListOffset);
					rw.replace(ctx.subroutineCall().LPAREN().getSymbol(), ctx.subroutineCall().RPAREN().getSymbol(), argList);					
				}
				String originalCall = fr.commentStatement(rw.getTokenStream().getText(ctx), ctx.getStart().getCharPositionInLine());
				rw.insertBefore(ctx.CALL().getSymbol(), originalCall);
			}
		}
	
	
	}

}
