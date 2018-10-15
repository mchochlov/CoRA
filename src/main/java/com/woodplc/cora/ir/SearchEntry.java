package com.woodplc.cora.ir;

public class SearchEntry {
	
	private final float score;
	private final String name;

	public SearchEntry(float score, String name) {
		if (score < 0 || name == null || name.isEmpty()) throw new IllegalArgumentException();
		this.score = score;
		this.name = name;
	}
	
	public float getScore() {return this.score;}
	public String getName() {return this.name;}
}
