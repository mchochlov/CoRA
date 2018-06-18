package com.woodplc.cora.ir;

import java.util.List;
import java.util.Optional;

public interface IREngine {
	
	List<String> getDocumentTermVector(String subname);

	Optional<Boolean> indexExists();
	
	void index(String subname, String textData);
	
	List<String> moreLikeThis(List<String> termVector, String query);
	
	void save();

	List<String> search(String query);
	
	void close();

}
