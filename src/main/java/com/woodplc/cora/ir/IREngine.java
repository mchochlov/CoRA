package com.woodplc.cora.ir;

import java.util.List;

public interface IREngine {

	List<String> getDocumentTermVector(String subname);
	
	void index(String subname, String textData);
	
	List<String> moreLikeThis(List<String> termVector, String query);
	
	void save();

	List<String> search(String query);
	
	void close();

}
