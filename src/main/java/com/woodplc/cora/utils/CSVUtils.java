package com.woodplc.cora.utils;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Set;

import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.utils.Utils.RegEx;

public class CSVUtils {

	public static void exportFeatureToCsv(Path exportPath, 
			Set<SubProgram> subprogramsSystemA,
			Set<SubProgram> subprogramsSystemB, 
			Set<SubProgram> subprogramsSystemC) throws IOException 
	{
		Objects.requireNonNull(exportPath);
		Objects.requireNonNull(subprogramsSystemA);
		Objects.requireNonNull(subprogramsSystemB);
		Objects.requireNonNull(subprogramsSystemC);
		
		if (Files.isDirectory(exportPath)) throw new IllegalArgumentException();
		
		try(PrintWriter pw = new PrintWriter(Files.newBufferedWriter(exportPath))) {
			pw.println("System A subprograms");
			printSubprograms(pw, subprogramsSystemA);
			pw.println("System B subprograms");
			printSubprograms(pw, subprogramsSystemB);
			pw.println("System C subprograms");
			printSubprograms(pw, subprogramsSystemC);
		}
	}
	
	private static void printSubprograms(PrintWriter pw, Set<SubProgram> subs) {
		subs.forEach(x -> pw.println(x.module() + RegEx.COMMA.regex() +
				x.name() + RegEx.COMMA.regex() + 
				x.startLine() + RegEx.COMMA.regex() + 
				x.endLine() + RegEx.COMMA.regex() +
				x.path()
		));
	}

}
