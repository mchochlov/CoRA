package com.woodplc.cora.storage;

import java.nio.file.Path;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;

import com.woodplc.cora.data.ImmutableModule;
import com.woodplc.cora.data.ModuleContainer;

public interface Repository {
	
	ImmutableModule retrieve(String checkSum, Path path, BiConsumer<Long, Long> consumer) throws Exception;
	
	void closeAndSync();

	ImmutableModule updateOrRetrieve(ModuleContainer module, String oldCheckSum, Path path) throws ExecutionException;

}
