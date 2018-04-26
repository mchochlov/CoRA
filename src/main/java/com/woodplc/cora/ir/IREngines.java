package com.woodplc.cora.ir;

import java.nio.file.Path;
import java.util.concurrent.ConcurrentHashMap;

public final class IREngines {
	
	private static final ConcurrentHashMap<Path, IREngine> ENGINE_INSTANCES = new ConcurrentHashMap<>();
	private static final long MAX_PARALLELISM = 1;
	
	private IREngines() {}

	public static IREngine getLuceneEngineInstance(Path path) {
		return ENGINE_INSTANCES.computeIfAbsent(path, LuceneIREngineWrapper::new);
	}
	
	public static void closeAll() {
		ENGINE_INSTANCES.forEachValue(MAX_PARALLELISM, IREngine::close);
	}
}
	