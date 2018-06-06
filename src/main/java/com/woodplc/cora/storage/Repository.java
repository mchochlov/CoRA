package com.woodplc.cora.storage;

import java.nio.file.Path;
import java.util.function.BiConsumer;

import com.woodplc.cora.data.ImmutableModule;

public interface Repository {
	
	ImmutableModule retrieve(String checkSum, Path path, BiConsumer<Long, Long> consumer) throws Exception;
	
	void closeAndSync();

}
