package com.woodplc.cora.ir;

import java.io.IOException;
import java.nio.file.Path;

import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.index.Term;

final class UpdateableLuceneEngineWrapper extends LuceneIREngineWrapper {

	public UpdateableLuceneEngineWrapper(Path path, boolean readOnly) {
		super(path, readOnly, OpenMode.CREATE_OR_APPEND);
	}

	@Override
	public void index(String subname, String textData) {
		if (readOnly) {
			throw new IllegalStateException();
		}
		
		try {
			writer.updateDocument(new Term(Fields.NAME.name(), subname), prepareDocument(subname, textData));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
