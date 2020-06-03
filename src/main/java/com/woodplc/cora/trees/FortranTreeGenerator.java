package com.woodplc.cora.trees;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Path;
import java.util.Objects;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.tree.ParseTree;

import com.github.gumtreediff.gen.antlr4.AbstractAntlr4TreeGenerator;
import com.google.common.io.MoreFiles;
import com.woodplc.cora.grammar.Fortran77LexerV1;
import com.woodplc.cora.grammar.Fortran77ParserV1;

public final class FortranTreeGenerator extends AbstractAntlr4TreeGenerator {

	private Fortran77LexerV1 lexer;
	private Fortran77ParserV1 parser;
	private final Path path;
	
	@Override
	protected String[] getRuleNames() {
		return parser.getRuleNames();
	}

	public FortranTreeGenerator(Path path) {
		super();
		Objects.requireNonNull(path);
		this.path = path;
	}

	@Override
	protected ParseTree getStartSymbol(Reader reader) throws RecognitionException, IOException {
		Objects.requireNonNull(reader);
		lexer = new Fortran77LexerV1(new ANTLRInputStream(reader));
		if (MoreFiles.getFileExtension(path).toLowerCase().equals("for")) {
			lexer.fixedForm = true;
		} else {
			lexer.fixedForm = false;
		}
		tokens = new CommonTokenStream(lexer);
        parser = new Fortran77ParserV1(tokens);
        return parser.program();
	}

	@Override
	protected String[] getTokenNames() {
		return lexer.getTokenNames();
	}
	
	public CommonTokenStream getTokenStream() {return this.tokens;}

}
