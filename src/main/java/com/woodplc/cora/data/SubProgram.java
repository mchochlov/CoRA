package com.woodplc.cora.data;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

import com.woodplc.cora.utils.Utils;

public abstract class SubProgram {

	private final String module;
	private final String subname;
	private final int startLine;
	private final int endLine;
	private final Path path;	
	
	public SubProgram(String module, String subname, int startLine, int endLine, Path path) {
		Objects.requireNonNull(module);
		Objects.requireNonNull(subname, "Subprogram name cannot be null!");
		Objects.requireNonNull(path, "File path cannot be null!");
		if (subname.isEmpty() ||
				startLine <= 0 ||
				endLine <= 0 ||
				startLine > endLine ||
				path.getNameCount() == 0) {
			throw new IllegalArgumentException();
		}

		this.module = module;
		this.subname = subname;
		this.startLine = startLine;
		this.endLine = endLine;
		this.path = path;
	}
	
	public static SubProgram fromString(String str) {
		Objects.requireNonNull(str);
		
		String[] entry = str.split(Utils.RegEx.COMMA.regex());
		if (entry.length != 6) {
			throw new IllegalArgumentException();
		}

		return ofType(entry[5], entry[0],
				entry[1], 
				Integer.parseInt(entry[2]), 
				Integer.parseInt(entry[3]),
				Paths.get(entry[4]));
	}
	
	public static SubProgram ofType(String type, String module, String name, 
			int startLine, int endLine, Path fname) {
		if (type.equalsIgnoreCase("program")) 
			return new Program(module,
				name, startLine, endLine, fname);
		else if (type.equalsIgnoreCase("subroutine"))
			return new Subroutine(module,
				name, startLine, endLine, fname);
		else if (type.equalsIgnoreCase("function"))
			return new Function(module,
				name, startLine, endLine, fname);
		else throw new IllegalArgumentException();
	}
	
	public String module() {return this.module;}
	
	public String name() {return this.subname;}
	
	public int startLine() {return this.startLine;}

	public int endLine() {return this.endLine;}
	
	public Path path() {return this.path;}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof SubProgram)) return false;
		SubProgram s = (SubProgram) o;
		return module.equals(s.module) &&
				subname.equals(s.subname) &&
				//startLine == s.startLine &&
				//endLine == s.endLine &&
				path.getFileName().equals(s.path.getFileName());
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(module, subname, path.getFileName());
	}

	@Override
	public String toString() {
		return "SubProgram [module=" + module + ", subname=" + subname + ", startLine=" + startLine + ", endLine=" + endLine + ", path=" + path
				+ "]";
	}

}
