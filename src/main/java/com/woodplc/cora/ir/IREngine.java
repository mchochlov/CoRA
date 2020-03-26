package com.woodplc.cora.ir;

import java.util.List;
import java.util.Optional;

public interface IREngine {
	
	List<String> getDocumentTermVector(String subname);

	Optional<Boolean> indexExists();
	
	void index(String subname, String textData);
	
	List<SearchEntry> moreLikeThis(List<String> termVector, String query);
	
	void save();

	List<SearchEntry> search(String query);
	
	void close();

}
