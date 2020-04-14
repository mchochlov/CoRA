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
import com.woodplc.cora.data.ApplicationState;
import com.woodplc.cora.data.CallEdge;
import com.woodplc.cora.data.FeatureView;
import com.woodplc.cora.data.Graphs;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;

public final class JSONUtils {

	private static final Path STATE_JSON_FILENAME = Paths.get(Repositories.DATA_FOLDER, "last_state.json");
	private static final String SDGRAPH_JSON_FILENAME = "sdgraph.json";
	private static final Gson SDGRAPH_TO_GSON = new GsonBuilder()
			.registerTypeHierarchyAdapter(SDGraph.class, new GraphAdapter())
			.create();
	private static final Gson STATE_TO_GSON = new GsonBuilder()
			.serializeNulls()
			.create();
	private static final Gson FEATURE_TO_GSON = new GsonBuilder()
			.serializeNulls()
			//.registerTypeHierarchyAdapter(Path.class, new PathAdapter())
			.registerTypeHierarchyAdapter(SubProgram.class, new SubProgramAdapter())
			.setPrettyPrinting()
			.create();

	private enum JsonFieldNames {
		TYPE("type"),
		MODULE("module"),
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

	public static ApplicationState stateFromJson() throws IOException {
		if (Files.notExists(STATE_JSON_FILENAME)) throw new IllegalStateException();
		return STATE_TO_GSON.fromJson(Files.newBufferedReader(STATE_JSON_FILENAME), ApplicationState.class);
	}
	
	public static void stateToJson(ApplicationState state) throws IOException {
		Objects.requireNonNull(state);
		String jsonString = STATE_TO_GSON.toJson(state);

		Files.createDirectories(Paths.get(Repositories.DATA_FOLDER));
		Files.write(STATE_JSON_FILENAME, jsonString.getBytes());
	}
	
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
				String type = null;
				String module = null;
				String subname = null;
				int startLine = 0, endLine = 0;
				Path path = null;
			    while (reader.hasNext()) {
			    	String name = reader.nextName();
			    	if (name.equals(JsonFieldNames.TYPE.fieldName)) {
			    		type = reader.nextString();
			    	} else if (name.equals(JsonFieldNames.MODULE.fieldName)) {
			    		module = reader.nextString();
			    	} else if (name.equals(JsonFieldNames.SUBNAME.fieldName)) {
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
			    subprograms.add(SubProgram.ofType(type, module, subname, startLine, endLine, path));				 
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
				writer.name(JsonFieldNames.TYPE.fieldName).value(subprogram.getClass().getSimpleName().toLowerCase());
				writer.name(JsonFieldNames.MODULE.fieldName).value(subprogram.module());
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

	public static boolean stateFileExists() {
		return Files.exists(STATE_JSON_FILENAME) && !Files.isDirectory(STATE_JSON_FILENAME);
	}

	public static void exportFeatureToJson(Path exportPath, FeatureView emptyView) throws IOException {
		Objects.requireNonNull(exportPath);
		Objects.requireNonNull(emptyView);
		
		if (Files.isDirectory(exportPath)) throw new IllegalArgumentException();
		String jsonString = FEATURE_TO_GSON.toJson(emptyView);

		Files.write(exportPath, jsonString.getBytes());
	}

	public static FeatureView loadFeatureFromJson(Path exportPath) throws IOException {
		Objects.requireNonNull(exportPath);
		if (Files.notExists(exportPath)) throw new IllegalArgumentException();
		
		return FEATURE_TO_GSON.fromJson(Files.newBufferedReader(exportPath), FeatureView.class);
	}
	
	private static class PathAdapter extends TypeAdapter<Path> {

		@Override
		public void write(JsonWriter writer, Path path) throws IOException {
			if (path == null) {
				writer.nullValue();
				return;
			}
			writer.value(path.toString());
		}

		@Override
		public Path read(JsonReader reader) throws IOException {
			if (reader.peek() == JsonToken.NULL) {
		        reader.nextNull();
		        return null;
		    }
			return Paths.get(reader.nextString());
		}
		
	}
	
	private static class SubProgramAdapter extends TypeAdapter<SubProgram> {

		@Override
		public void write(JsonWriter writer, SubProgram subprogram) throws IOException {
			if (subprogram == null) {
				writer.nullValue();
				return;
			}
			writer.beginObject();
			writer.name(JsonFieldNames.TYPE.fieldName).value(subprogram.getClass().getSimpleName().toLowerCase());
			writer.name(JsonFieldNames.MODULE.fieldName).value(subprogram.module());
			writer.name(JsonFieldNames.SUBNAME.fieldName).value(subprogram.name());
			writer.name(JsonFieldNames.STARTLINE.fieldName).value(subprogram.startLine());
			writer.name(JsonFieldNames.ENDLINE.fieldName).value(subprogram.endLine());
			writer.name(JsonFieldNames.PATH.fieldName).value(subprogram.path().toString());
			writer.endObject();
		}

		@Override
		public SubProgram read(JsonReader reader) throws IOException {
			if (reader.peek() == JsonToken.NULL) {
		        reader.nextNull();
		        return null;
		    }
			reader.beginObject();
			String type = null;
			String module = null;
			String subname = null;
			int startLine = 0, endLine = 0;
			Path path = null;
		    while (reader.hasNext()) {
		    	String name = reader.nextName();
		    	if (name.equals(JsonFieldNames.TYPE.fieldName)) {
		    		type = reader.nextString();
		    	} else if (name.equals(JsonFieldNames.MODULE.fieldName)) {
		    		module = reader.nextString();
		    	} else if (name.equals(JsonFieldNames.SUBNAME.fieldName)) {
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
		    return SubProgram.ofType(type, module, subname, startLine, endLine, path);	
		}
		
	}
}
