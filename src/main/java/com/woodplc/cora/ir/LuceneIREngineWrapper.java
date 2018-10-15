package com.woodplc.cora.ir;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.custom.CustomAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.FieldType;
import org.apache.lucene.document.StringField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexOptions;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.index.Term;
import org.apache.lucene.index.Terms;
import org.apache.lucene.index.TermsEnum;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;

import com.woodplc.cora.app.Main.Resource;

final class LuceneIREngineWrapper implements IREngine {
	
	private final static String INDEX_ROOT = "index";
	private final static int MAX_HITS = Integer.MAX_VALUE;
	private final Path indexPath;
	private final Analyzer fortranAnalyzer;
	private final Directory dir;
	private final IndexWriter writer;
	private final SearcherManager searchManager;
	private final QueryParser dataFieldQueryParser;
	private final boolean readOnly;
	
	private enum Fields{
		NAME,
		DATA;
	}

	public LuceneIREngineWrapper(Path path, boolean readOnly) {
		this.indexPath = Paths.get(Objects.requireNonNull(path).toString(), INDEX_ROOT);
		this.readOnly = readOnly;
		
		try {
			this.fortranAnalyzer = CustomAnalyzer.builder()
					//.addCharFilter("patternreplace", "pattern", "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])", "replacement", " ")
					.withTokenizer("lowercase")
					.addTokenFilter("stop", "ignoreCase", "true", "words", Resource.FORTRAN_KEYWORDS.path(), "format", "snowball")
					.addTokenFilter("porterstem")
					.addTokenFilter("stop", "ignoreCase", "true")//, "words", "stopwords.txt", "format", "wordset")
					//.addTokenFilter("length", "min", "3", "max", "255")
					.build();
			
			this.dir = FSDirectory.open(indexPath);
			
			if (readOnly) {
				this.writer = null;
				this.searchManager = new SearcherManager(dir, null);
			} else {
				IndexWriterConfig iwc = new IndexWriterConfig(fortranAnalyzer).setOpenMode(OpenMode.CREATE);
				this.writer = new IndexWriter(dir, iwc);
				this.searchManager = new SearcherManager(writer, true, true, null);
			}
			this.dataFieldQueryParser = new QueryParser(Fields.DATA.name(), fortranAnalyzer);

		} catch (IOException e) {
			throw new IllegalStateException(e.getMessage());
		}
	}

	@Override
	public void index(String subname, String textData) {
		if (readOnly) {
			throw new IllegalStateException();
		}
		
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
		if (readOnly) {
			throw new IllegalStateException();
		}
		
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
	public List<SearchEntry> search(String queryString) {
		if (queryString == null || queryString.isEmpty()) {
			throw new IllegalArgumentException();
		}
		
		IndexSearcher searcher = null;
		try {
			searcher = searchManager.acquire();
			Query query = dataFieldQueryParser.parse(queryString);
			
			ScoreDoc[] hits = searcher.search(query, MAX_HITS).scoreDocs;
			
			List<SearchEntry> matchingSubprograms = new ArrayList<>();
			for (ScoreDoc hit : hits) {
				matchingSubprograms.add(new SearchEntry(hit.score, 
						searcher.doc(hit.doc).get(Fields.NAME.name())));
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

	@Override
	public List<String> getDocumentTermVector(String subname) {
		if (subname == null || subname.isEmpty()) {
			throw new IllegalArgumentException();
		}
		
		IndexSearcher searcher = null;
		try {
			searcher = searchManager.acquire();
			Query query = new TermQuery(new Term(Fields.NAME.name(), subname));
			
			ScoreDoc[] hits = searcher.search(query, MAX_HITS).scoreDocs;
			
			if (hits.length != 1) {
				throw new IllegalStateException();
			}
			
			Terms terms = searcher.getIndexReader().getTermVector(hits[0].doc, Fields.DATA.name());
			TermsEnum iter = terms.iterator();
			BytesRef ref = iter.next();
			List<String> termVector = new ArrayList<>();
			while(ref != null) {
				termVector.add(ref.utf8ToString());
				ref = iter.next();
			}
			return termVector;
		} catch (IOException e) {
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
	public List<SearchEntry> moreLikeThis(List<String> termVector, String query) {
		Objects.requireNonNull(termVector);
		if (termVector.isEmpty() && (query == null || query.isEmpty())) {
			throw new IllegalArgumentException();
		}
		
		IndexSearcher searcher = null;
		try {
			searcher = searchManager.acquire();
			
			BooleanQuery.Builder termQueryBuilder = new BooleanQuery.Builder();
			
			termVector.forEach(x -> termQueryBuilder.add(new TermQuery(new Term(Fields.DATA.name(), x)), Occur.SHOULD));
			
			Query optionalQuery = null;
			if (query != null && !query.isEmpty()) {
				optionalQuery = dataFieldQueryParser.parse(query);
				termQueryBuilder.add(optionalQuery, Occur.SHOULD);
			}
			
			BooleanQuery finalQuery = termQueryBuilder.build();
			
			ScoreDoc[] hits = searcher.search(finalQuery, MAX_HITS).scoreDocs;
			
			List<SearchEntry> matchingSubprograms = new ArrayList<>();
			for (ScoreDoc hit : hits) {
				matchingSubprograms.add(new SearchEntry(hit.score, searcher.doc(hit.doc).get(Fields.NAME.name())));
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
	public Optional<Boolean> indexExists() {
		try {
			return Optional.of(DirectoryReader.indexExists(dir));
		} catch (IOException e) {
			e.printStackTrace();
			return Optional.empty();
		}
	}

}
