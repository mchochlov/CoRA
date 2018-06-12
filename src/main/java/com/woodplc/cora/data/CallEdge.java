package com.woodplc.cora.data;

import java.util.Objects;

public final class CallEdge {

	private final String source;
	private final String target;
	
	public CallEdge(String source, String target) {
		Objects.requireNonNull(source);
		Objects.requireNonNull(target);
		
		if (source.isEmpty() || target.isEmpty()) throw new IllegalArgumentException();
		this.source = source;
		this.target = target;
	}
	
	public String source() {return this.source;}
	public String target() {return this.target;}
}
