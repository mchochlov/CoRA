package com.woodplc.cora.storage;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;
import com.woodplc.cora.data.CallEdge;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;

public final class JSONUtils {

	private static final String SDGRAPH_JSON_FILENAME = "sdgraph.json";
	private static final Gson SDGRAPH_TO_GSON = new GsonBuilder()
			.registerTypeHierarchyAdapter(SDGraph.class, new GraphAdapter())
			.create();

	private enum JsonFieldNames {
		SUBNAME("subname"),
		STARTLINE("start"),
		ENDLINE("end"),
		PATH("path"),
		SOURCE("u"),
		TARGET("v"),
		SUBPROGRAMS("subprograms"),
		EDGES("edges"),
		VARIABLES("variables");
		
		private final String fieldName;
		
		private JsonFieldNames(String fieldName) {this.fieldName = fieldName;}
	}
	
	private JSONUtils() {}

	public static SDGraph graphFromJson(Path entryPath) throws IOException {
		Objects.requireNonNull(entryPath);
		Path graphFilePath = Paths.get(entryPath.toString(), SDGRAPH_JSON_FILENAME);
		
		if (!Files.exists(graphFilePath)) throw new IllegalArgumentException();
		return SDGRAPH_TO_GSON.fromJson(Files.newBufferedReader(graphFilePath), SDGraph.class);
	}

	public static void graphToJson(Path entryPath, SDGraph graph) throws IOException {
		Objects.requireNonNull(entryPath);
		Objects.requireNonNull(graph);

		if (!Files.isDirectory(entryPath)) throw new IllegalArgumentException();

		Path graphFilePath = Paths.get(entryPath.toString(), SDGRAPH_JSON_FILENAME);
		String jsonString = SDGRAPH_TO_GSON.toJson(graph);

		Files.write(graphFilePath, jsonString.getBytes());
	}

	private static class GraphAdapter extends TypeAdapter<SDGraph> {

		@Override
		public SDGraph read(JsonReader reader) throws IOException {
			if (reader.peek() == JsonToken.NULL) {
		        reader.nextNull();
		        return null;
		    }
			
			reader.beginObject();
			Set<SubProgram> subprograms = readSubprograms(reader);
			Set<CallEdge> edges = readEdges(reader);
			Map<String, Collection<String>> variables = writeVariables(reader);
			reader.endObject();
			
			return Graphs.newInstanceFromValues(subprograms, edges, variables);
		}

		private Set<SubProgram> readSubprograms(JsonReader reader) throws IOException {
			if (reader.peek() == JsonToken.NULL) {
		        reader.nextNull();
		        return null;
		    }
			
			Set<SubProgram> subprograms = new HashSet<>();
			reader.nextName();
			reader.beginArray();
			while(reader.hasNext()) {
				reader.beginObject();
				String subname = null;
				int startLine = 0, endLine = 0;
				Path path = null;
			    while (reader.hasNext()) {
			    	String name = reader.nextName();
			    	if (name.equals(JsonFieldNames.SUBNAME.fieldName)) {
			    		subname = reader.nextString();
			    	} else if (name.equals(JsonFieldNames.STARTLINE.fieldName)) {
			    		startLine = reader.nextInt();
			    	} else if (name.equals(JsonFieldNames.ENDLINE.fieldName)) {
			    		endLine = reader.nextInt();
			    	} else if (name.equals(JsonFieldNames.PATH.fieldName)) {
			    		path = Paths.get(reader.nextString());
			    	} else {
			    		reader.skipValue();
			    	}
			    }
			    reader.endObject();
			    subprograms.add(new SubProgram(subname, startLine, endLine, path));				 
			}
			reader.endArray();
			
			return subprograms;
		}

		private Set<CallEdge> readEdges(JsonReader reader) throws IOException {
			if (reader.peek() == JsonToken.NULL) {
		        reader.nextNull();
		        return null;
		    }
			
			Set<CallEdge> edges = new HashSet<>();
			reader.nextName();
			reader.beginArray();
			while(reader.hasNext()) {
				reader.beginObject();
				String source = null, target = null;
			    while (reader.hasNext()) {
			    	String name = reader.nextName();
			    	if (name.equals(JsonFieldNames.SOURCE.fieldName)) {
			    		source = reader.nextString();
			    	} else if (name.equals(JsonFieldNames.TARGET.fieldName)) {
			    		target = reader.nextString();
			    	} else {
			    		reader.skipValue();
			    	}
			    }
			    reader.endObject();
			    edges.add(new CallEdge(source, target));
			}
			reader.endArray();
			return edges;
		}

		private Map<String, Collection<String>> writeVariables(JsonReader reader) throws IOException {
			if (reader.peek() == JsonToken.NULL) {
		        reader.nextNull();
		        return null;
		    }
			
			Map<String, Collection<String>> variables = new HashMap<>();
			reader.nextName();
			reader.beginArray();
			while(reader.hasNext()) {
				reader.beginObject();
				String key = reader.nextName();
				reader.beginArray();
				Collection<String> collection = new HashSet<>();
				while(reader.hasNext()) {
				    collection.add(reader.nextString());
				}
				reader.endArray();
				reader.endObject();
				variables.put(key, collection);
			}
			reader.endArray();
			
			return variables;
		}

		@Override
		public void write(JsonWriter writer, SDGraph graph) throws IOException {
			if (graph == null) {
				writer.nullValue();
				return;
			}

			writer.beginObject();
			writeSubprograms(writer, graph.subprograms());
			writeEdges(writer, graph.edges());
			writeVariables(writer, graph.variables());
			writer.endObject();
		}

		private void writeSubprograms(JsonWriter writer, Set<SubProgram> subprograms)
				throws IOException {
			if (subprograms == null) {
				writer.nullValue();
				return;
			}
			writer.name(JsonFieldNames.SUBPROGRAMS.fieldName);
			writer.beginArray();
			
			for (SubProgram subprogram : subprograms) {
				writer.beginObject();
				writer.name(JsonFieldNames.SUBNAME.fieldName).value(subprogram.name());
				writer.name(JsonFieldNames.STARTLINE.fieldName).value(subprogram.startLine());
				writer.name(JsonFieldNames.ENDLINE.fieldName).value(subprogram.endLine());
				writer.name(JsonFieldNames.PATH.fieldName).value(subprogram.path().toString());
				writer.endObject();
			}
			
			writer.endArray();
		}

		private void writeEdges(JsonWriter writer, Set<CallEdge> edges) throws IOException {
			if (edges == null) {
				writer.nullValue();
				return;
			}
			writer.name(JsonFieldNames.EDGES.fieldName);
			writer.beginArray();
			for (CallEdge edge : edges) {
				writer.beginObject();
				writer.name(JsonFieldNames.SOURCE.fieldName).value(edge.source());
				writer.name(JsonFieldNames.TARGET.fieldName).value(edge.target());
				writer.endObject();
			}
			writer.endArray();
		}

		private void writeVariables(JsonWriter writer, Map<String, Collection<String>> variables) throws IOException {
			if (variables == null) {
				writer.nullValue();
				return;
			}
			writer.name(JsonFieldNames.VARIABLES.fieldName);
			writer.beginArray();
			for (Map.Entry<String, Collection<String>> entry : variables.entrySet()) {
				writer.beginObject();
				writer.name(entry.getKey());
				writer.beginArray();
				for (String subprogram : entry.getValue()) {
					writer.value(subprogram);
				}
				writer.endArray();
				writer.endObject();
			}
			writer.endArray();
		}

	}
}
