package com.woodplc.cora.ir;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

final class LuceneIREngineWrapper implements IREngine {
	
	private final Path path;

	public LuceneIREngineWrapper(Path path) {
		this.path = Objects.requireNonNull(path);
	}

	@Override
	public void index(String subname, String textData) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public List<String> search(String query) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void close() {
		// TODO Auto-generated method stub
		
	}

	
}
