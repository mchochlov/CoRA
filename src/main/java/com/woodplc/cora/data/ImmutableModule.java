package com.woodplc.cora.data;

import java.util.Objects;

import com.woodplc.cora.ir.IREngine;

public final class ImmutableModule {

	private final SDGraph graph;
	private final IREngine engine;
	private final boolean persistent;
	
	private ImmutableModule(SDGraph graph, IREngine engine, boolean persistent) {
		this.graph = Objects.requireNonNull(graph);
		this.engine = Objects.requireNonNull(engine);
		this.persistent = persistent;
	}

	public SDGraph getGraph() {return graph;}

	public boolean isPersistent() {return persistent;}
	
	public IREngine getEngine() {return engine;}

	public static ImmutableModule persistent(SDGraph graph, IREngine engine) {
		return new ImmutableModule(graph, engine, true);
	}

	public static ImmutableModule nonPersistent(SDGraph graph, IREngine engine) {
		return new ImmutableModule(graph, engine, false);
	}

}