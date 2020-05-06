package com.woodplc.cora.ir;

import java.nio.file.Path;

public final class IREngines {
	
	private IREngines() {}
	
	public static IREngine newReadOnlyInstance(Path path) {
		return new LuceneIREngineWrapper(path, true);
	}
	
	public static IREngine newWriteableInstance(Path path) {
		return new LuceneIREngineWrapper(path, false);
	}

	public static IREngine existingWriteableInstance(Path path) {
		return new UpdateableLuceneEngineWrapper(path, false);
	}
}
	