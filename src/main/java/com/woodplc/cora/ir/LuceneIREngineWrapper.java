package com.woodplc.cora.ir;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.custom.CustomAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.FieldType;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.IndexOptions;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import com.woodplc.cora.app.Main.Resource;

final class LuceneIREngineWrapper implements IREngine {
	
	private final static String INDEX_ROOT = "index";
	private final static int MAX_HITS = Integer.MAX_VALUE;
	private final Path indexPath;
	private final Analyzer fortranAnalyzer;
	private final IndexWriter writer;
	private final SearcherManager searchManager;
	private final QueryParser dataFieldQueryParser;
	
	private enum Fields{
		NAME,
		DATA;
	}

	public LuceneIREngineWrapper(Path path) {
		this.indexPath = Paths.get(INDEX_ROOT, Objects.requireNonNull(path).getFileName().toString());
		
		try {
			this.fortranAnalyzer = CustomAnalyzer.builder()
					//.addCharFilter("patternreplace", "pattern", "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])", "replacement", " ")
					.withTokenizer("lowercase")
					.addTokenFilter("stop", "ignoreCase", "true", "words", Resource.FORTRAN_KEYWORDS.path(), "format", "snowball")
					.addTokenFilter("porterstem")
					.addTokenFilter("stop", "ignoreCase", "true")//, "words", "stopwords.txt", "format", "wordset")
					//.addTokenFilter("length", "min", "3", "max", "255")
					.build();
			
			Directory dir = FSDirectory.open(indexPath);
			IndexWriterConfig iwc = new IndexWriterConfig(fortranAnalyzer).setOpenMode(OpenMode.CREATE);
			this.writer = new IndexWriter(dir, iwc);
			this.searchManager = new SearcherManager(writer, true, true, null);
			this.dataFieldQueryParser = new QueryParser(Fields.DATA.name(), fortranAnalyzer);

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
		FieldType customType = new FieldType();
		customType.setIndexOptions(IndexOptions.DOCS_AND_FREQS_AND_POSITIONS);
		customType.setTokenized(true);
		customType.setStoreTermVectors(true);
		customType.freeze();
		Field textField = new Field(Fields.DATA.name(), textData, customType);
		doc.add(textField);
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
				searchManager.maybeRefreshBlocking();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public List<String> search(String queryString) {
		if (queryString == null || queryString.isEmpty()) {
			throw new IllegalArgumentException();
		}
		
		IndexSearcher searcher = null;
		try {
			searcher = searchManager.acquire();
			Query query = dataFieldQueryParser.parse(queryString);
			
			ScoreDoc[] hits = searcher.search(query, MAX_HITS).scoreDocs;
			
			List<String> matchingSubprograms = new ArrayList<>();
			for (ScoreDoc hit : hits) {
				matchingSubprograms.add(searcher.doc(hit.doc).get(Fields.NAME.name()));
			}
			return matchingSubprograms;
			
		} catch (IOException | ParseException e) {
			e.printStackTrace();
			return Collections.emptyList();
		} finally {
			try {
				searchManager.release(searcher);
				searcher = null;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	@Override
	public void close() {
		try {
			if (writer != null && writer.isOpen()) {writer.close();}	
			if (searchManager != null) {searchManager.close();}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
