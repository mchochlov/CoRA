package com.woodplc.cora.data;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

public class SubProgram {

	private static final String COMMA_SEPARATOR = ",";

	private final String subname;
	private final int startLine;
	private final int endLine;
	private final Path path;	
	
	private SubProgram(String subname, int startLine, int endLine, Path path) {
		this.subname = subname;
		this.startLine = startLine;
		this.endLine = endLine;
		this.path = path;
	}
	
	public static SubProgram fromString(String str) {
		Objects.requireNonNull(str);
		
		String[] entry = str.split(COMMA_SEPARATOR);
		if (entry.length != 4) {
			throw new IllegalArgumentException();
		}
		
		return fromValues(entry[0], 
				Integer.parseInt(entry[1]), 
				Integer.parseInt(entry[2]),
				Paths.get(entry[3]));
	}

	public static SubProgram fromValues(String subname, int startLine, int endLine, Path path) {
		Objects.requireNonNull(subname, "Subprogram name cannot be null!");
		Objects.requireNonNull(path, "File path cannot be null!");
		if (subname.length() == 0 ||
				startLine <= 0 ||
				endLine <= 0 ||
				path.getNameCount() == 0) {
			throw new IllegalArgumentException();
		}
		return new SubProgram(subname, startLine, endLine, path);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof SubProgram)) return false;
		SubProgram s = (SubProgram) o;
		return subname.equals(s.subname) &&
				startLine == s.startLine &&
				endLine == s.endLine &&
				path.equals(s.path);
	}

	@Override
	public int hashCode() {
		return Objects.hash(subname, startLine, endLine, path);
	}

}
