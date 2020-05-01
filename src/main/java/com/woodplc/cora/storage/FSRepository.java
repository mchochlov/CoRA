package com.woodplc.cora.storage;

import static java.util.stream.Collectors.toSet;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.RemovalListener;
import com.google.common.cache.RemovalListeners;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.ImmutableModule;
import com.woodplc.cora.data.ModuleContainer;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.ir.IREngine;
import com.woodplc.cora.ir.IREngines;
import com.woodplc.cora.parser.Parser;
import com.woodplc.cora.parser.Parsers;

final class FSRepository implements Repository {

	final Logger logger = LoggerFactory.getLogger(FSRepository.class);
	
	private final static long SECONDS_TO_WAIT = 5;
	private final static int N_SYSTEMS = 3;
	private final static int CACHE_SIZE = N_SYSTEMS * 2;
	private final static int N_THREADS = Runtime.getRuntime().availableProcessors();
	
	private final ExecutorService executor = Executors.newFixedThreadPool(N_THREADS);
	
	private final RemovalListener<String, ImmutableModule> remListener = RemovalListeners
			.asynchronous(removal -> {
					if (removal.getValue().isPersistent()) {return;}
					try {
						persistToRepository(removal.getKey(), removal.getValue());
					} catch (EntryWriteException e) {
						e.printStackTrace();
					}	
			}, executor);
	
	private final Cache<String, ImmutableModule> modules = CacheBuilder.newBuilder()
			.maximumSize(CACHE_SIZE)
			.removalListener(remListener)
			.build();

	@Override
	public ImmutableModule retrieve(String checkSum, Path path, BiConsumer<Long, Long> consumer) throws Exception {
		Objects.requireNonNull(checkSum);

		if (checkSum.isEmpty()) throw new IllegalArgumentException();
		
		if (modules.getIfPresent(checkSum) != null) {
			logger.info("Loading {} from cache with checksum {}", path.getFileName(), checkSum);
		}
		
		return modules.get(checkSum, () -> {
			Path entryPath = Repositories.pathForCheckSum(checkSum);
			if (Files.isDirectory(entryPath)) {
				try {
					logger.info("Loading {} from repository with checksum {}", path.getFileName(), checkSum);
					return loadFromRepository(entryPath);
				} catch (EntryReadException e) {
					e.printStackTrace();
					logger.error("Error loading {} with checksum {}: recalculating", path.getFileName(), checkSum);
					return calculateNew(checkSum, path, consumer);
				}
			} else {
				logger.info("Creating new repository for {} with checksum {}", path.getFileName(), checkSum);
				return calculateNew(checkSum, path, consumer);
			}
		});
	}

	@Override
	public void closeAndSync() {
		if (modules.size() == 0) return;
		
		modules.invalidateAll();
		executor.shutdown();
		try {
		    if (!executor.awaitTermination(SECONDS_TO_WAIT, TimeUnit.SECONDS)) {
		    	executor.shutdownNow();
		    	if (!executor.awaitTermination(SECONDS_TO_WAIT, TimeUnit.SECONDS)) {
		           System.err.println("Executor didn't shutdown");
		    	}
		    }
		} catch (InterruptedException ie) {
		   	executor.shutdownNow();
		   	Thread.currentThread().interrupt();
		}
	}

	private static ImmutableModule loadFromRepository(Path entryPath) throws EntryReadException {
		Objects.requireNonNull(entryPath);
		try {
			SDGraph graph = JSONUtils.graphFromJson(entryPath);
			IREngine engine = IREngines.newReadOnlyInstance(entryPath);
			
			return ImmutableModule.persistent(graph, engine);
		} catch (Exception e) {
			throw new EntryReadException();
		}
	}
		
	private static void persistToRepository(String checkSum, ImmutableModule module) throws EntryWriteException {
		Objects.requireNonNull(checkSum);
		Objects.requireNonNull(module);
		IREngine engine = module.getEngine();
		Objects.requireNonNull(engine);
		Path entryPath = Repositories.pathForCheckSum(checkSum);
		boolean indexExists = engine.indexExists().orElseThrow(EntryWriteException::new);
		
		if (!indexExists) throw new EntryWriteException();
		
		try {
			JSONUtils.graphToJson(entryPath, module.getGraph());
			engine.close();
		} catch(Exception e) {
			throw new EntryWriteException();
		}
	}
	
	private static ImmutableModule calculateNew(String checkSum, Path path, BiConsumer<Long, Long> consumer) throws IOException {
		Objects.requireNonNull(path);
		Objects.requireNonNull(consumer);
		
		IREngine engine = IREngines.newWriteableInstance(Repositories.pathForCheckSum(checkSum));
		SDGraph graph = Graphs.newInstance();
		Parser parser = Parsers.indexableFortranParser(engine);
		try (Stream<Path> stream = Files.walk(path)) 
		{
			long parsedFiles = 0;
			Set<Path> paths = stream
				.filter(Parsers::isFortranFile)
				.collect(toSet());
			final long totalFiles = paths.size();
											
			for (Path entry: paths) {
				graph.merge(parser.parse(entry));
				consumer.accept(++parsedFiles, totalFiles);
			}
			engine.save();
			
			return ImmutableModule.nonPersistent(graph, engine);
	    }
	}

	@Override
	public ImmutableModule updateOrRetrieve(ModuleContainer module, String oldCheckSum, Path path) throws ExecutionException {
		if (modules.getIfPresent(module.getCheckSum()) != null) {
			logger.info("Loading {} from cache with checksum {}", path.getFileName(), module.getCheckSum());
		}
		return modules.get(module.getCheckSum(), () -> {
			Path entryPath = Repositories.pathForCheckSum(module.getCheckSum());
			if (Files.isDirectory(entryPath)) {
				try {
					logger.info("Update: {} with checksum {} already exists - loading from repository", path.getFileName(), module.getCheckSum());
					return loadFromRepository(entryPath);
				} catch (EntryReadException e) {
					e.printStackTrace();
					logger.error("Update error: cannot load {} with checksum {}", path.getFileName(), module.getCheckSum());
					return null;//calculateNew(checkSum, path, consumer);
				}
			} else {
				logger.info("Update: generating new repository for {} with checksum {}", path.getFileName(), module.getCheckSum());
				return updateAndReturn(module, oldCheckSum, path);
			}
		});
	}

	private ImmutableModule updateAndReturn(ModuleContainer module, String oldCheckSum, Path path) throws IOException {
		Objects.requireNonNull(module.getPath());
		//copy lucene dir
		FileUtils.copyDirectory(Repositories.pathForCheckSum(oldCheckSum).toFile(), 
				Repositories.pathForCheckSum(module.getCheckSum()).toFile(), false);
		IREngine engine = IREngines.existingWriteableInstance(Repositories.pathForCheckSum(module.getCheckSum()));
		Parser parser = Parsers.indexableFortranParser(engine);
		SDGraph graph = Graphs.copyOf(module.getModule().getGraph());
		graph.updateSubprograms(parser.parse(path).subprograms());
		engine.save();
			
		return ImmutableModule.nonPersistent(graph, engine);
	}
	
}
