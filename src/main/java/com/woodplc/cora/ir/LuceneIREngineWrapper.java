package com.woodplc.cora.ir;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.custom.CustomAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import com.woodplc.cora.app.Main.Resource;

final class LuceneIREngineWrapper implements IREngine {
	
	private final static String INDEX_ROOT = "index";
	private final Path indexPath;
	private final Analyzer fortranAnalyzer;
	private final IndexWriter writer;
	
	private enum Fields{
		NAME,
		DATA;
	}

	public LuceneIREngineWrapper(Path path) {
		this.indexPath = Paths.get(INDEX_ROOT, Objects.requireNonNull(path).getFileName().toString());
		
		try {
			this.fortranAnalyzer = CustomAnalyzer.builder()
					.addCharFilter("patternreplace", "pattern", "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])", "replacement", " ")
					.withTokenizer("lowercase")
		
					.addTokenFilter("stop", "ignoreCase", "true", "words", Resource.FORTRAN_KEYWORDS.path(), "format", "snowball")
					.addTokenFilter("porterstem")
					.addTokenFilter("stop", "ignoreCase", "true")//, "words", "stopwords.txt", "format", "wordset")
					//.addTokenFilter("length", "min", "3", "max", "255")
					.build();
			
			Directory dir = FSDirectory.open(indexPath);
			IndexWriterConfig iwc = new IndexWriterConfig(fortranAnalyzer).setOpenMode(OpenMode.CREATE_OR_APPEND);
			this.writer = new IndexWriter(dir, iwc);

		} catch (IOException e) {
			throw new IllegalStateException(e.getMessage());
		}
	}

	@Override
	public void index(String subname, String textData) {
		if ((subname == null || subname.isEmpty()) ||
				textData == null || textData.isEmpty()) {
			throw new IllegalArgumentException();
		}
		
		Document doc = new Document();
		doc.add(new StringField(Fields.NAME.name(), subname, Field.Store.YES));
		doc.add(new TextField(Fields.DATA.name(), textData, Field.Store.NO));
		try {
			writer.addDocument(doc);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public void save() {
		if (writer != null && writer.hasUncommittedChanges()) {
			try {
				writer.commit();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public List<String> search(String query) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void close() {
		if (writer != null && writer.isOpen()) {
			try {
				writer.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
}
