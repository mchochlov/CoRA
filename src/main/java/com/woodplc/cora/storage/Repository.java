package com.woodplc.cora.storage;

import com.woodplc.cora.data.ImmutableModule;

public interface Repository {
	
	void add(ImmutableModule module);
	
	ImmutableModule retrieve(String checkSum);
	
	void closeAndSync();

}
