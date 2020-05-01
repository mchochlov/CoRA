package com.woodplc.cora.data;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

public final class ModuleContainer {
	private Path path;
	private String checkSum;
	private ImmutableModule module;
	
	private ModuleContainer(Path path, String checkSum, ImmutableModule module) {
		this.path = path;
		this.checkSum = checkSum;
		this.module = module;
	}

	public Path getPath() {	return path;}
	public String getCheckSum() {return this.checkSum;}
	public ImmutableModule getModule() {return module;}
	
	public void setPath(Path path) {
		this.path = Objects.requireNonNull(path);
		this.checkSum = null;
		this.module = null;
	}

	public void setCheckSum(String checkSum) {
		if (checkSum == null || checkSum.isEmpty()) throw new IllegalArgumentException();
		if (this.path == null) throw new IllegalStateException();
		this.checkSum = checkSum;
	}
	
	public void setModule(ImmutableModule module) {
		Objects.requireNonNull(module);
		if (this.path == null || this.checkSum == null) throw new IllegalStateException();
		this.module = module;
	}
	
	public static ModuleContainer empty() {
		return new ModuleContainer(null, null, null);
	}
	
	public static ModuleContainer fromValues(String path, String checkSum) {
		if (path == null && checkSum != null) throw new IllegalArgumentException();
		
		Path fpath = path == null ? null : Paths.get(path);
		return new ModuleContainer(fpath, checkSum, null);
	}

	public String updateCheckSum(String checkSum) {
		String old = this.checkSum;
		this.checkSum = checkSum;
		return old;
	}	
}
