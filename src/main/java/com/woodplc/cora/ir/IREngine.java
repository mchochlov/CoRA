package com.woodplc.cora.ir;

import java.util.List;

public interface IREngine {

	void index(String subname, String textData);
	
	void save();

	List<String> search(String query);
	
	void close();

}
