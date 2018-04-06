package com.woodplc.cora.data;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

import com.woodplc.cora.utils.Utils;

public final class SubProgram {

	private final String subname;
	private final int startLine;
	private final int endLine;
	private final Path path;	
	
	public SubProgram(String subname, int startLine, int endLine, Path path) {
		Objects.requireNonNull(subname, "Subprogram name cannot be null!");
		Objects.requireNonNull(path, "File path cannot be null!");
		if (subname.isEmpty() ||
				startLine <= 0 ||
				endLine <= 0 ||
				startLine > endLine ||
				path.getNameCount() == 0) {
			throw new IllegalArgumentException();
		}

		this.subname = subname;
		this.startLine = startLine;
		this.endLine = endLine;
		this.path = path;
	}
	
	public static SubProgram fromString(String str) {
		Objects.requireNonNull(str);
		
		String[] entry = str.split(Utils.RegEx.COMMA.regex());
		if (entry.length != 4) {
			throw new IllegalArgumentException();
		}
		
		return new SubProgram(entry[0], 
				Integer.parseInt(entry[1]), 
				Integer.parseInt(entry[2]),
				Paths.get(entry[3]));
	}
	
	public String name() {return this.subname;}
	
	public int startLine() {return this.startLine;}

	public int endLine() {return this.endLine;}
	
	public Path path() {return this.path;}


	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof SubProgram)) return false;
		SubProgram s = (SubProgram) o;
		return subname.equals(s.subname) &&
				startLine == s.startLine &&
				endLine == s.endLine &&
				path.getFileName().equals(s.path.getFileName());
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(subname, startLine, endLine, path.getFileName());
	}

	@Override
	public String toString() {
		return "SubProgram [subname=" + subname + ", startLine=" + startLine + ", endLine=" + endLine + ", path=" + path
				+ "]";
	}

}
