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
		NODES("nodes"),
		EDGES("edges"),
		VARIABLES("variables");
		
		private final String fieldName;
		
		private JsonFieldNames(String fieldName) {this.fieldName = fieldName;}
	}
	
	private JSONUtils() {}

	public static SDGraph graphFromJson(Path entryPath) throws IOException {
		
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
		public void write(JsonWriter writer, SDGraph graph) throws IOException {
			if (graph == null) {
				writer.nullValue();
				return;
			}

			writer.beginObject();
			writeSubprograms(writer, graph.subprograms());
			writeNodes(writer, graph.nodes());
			writeEdges(writer, graph.edges());
			writeVariables(writer, graph.variables());
			writer.endObject();
		}

		private void writeSubprograms(JsonWriter writer, Map<String, Collection<SubProgram>> subprograms)
				throws IOException {
			if (subprograms == null) {
				writer.nullValue();
				return;
			}
			writer.name(JsonFieldNames.SUBPROGRAMS.fieldName);
			writer.beginArray();
			for (Map.Entry<String, Collection<SubProgram>> entry : subprograms.entrySet()) {
				writer.beginObject();
				writer.name(entry.getKey());
				writer.beginArray();
				for (SubProgram subprogram : entry.getValue()) {
					writer.beginObject();
					writer.name(JsonFieldNames.SUBNAME.fieldName).value(subprogram.name());
					writer.name(JsonFieldNames.STARTLINE.fieldName).value(subprogram.startLine());
					writer.name(JsonFieldNames.ENDLINE.fieldName).value(subprogram.endLine());
					writer.name(JsonFieldNames.PATH.fieldName).value(subprogram.path().toString());
					writer.endObject();
				}
				writer.endArray();
				writer.endObject();
			}
			writer.endArray();
		}

		private void writeNodes(JsonWriter writer, Set<String> nodes) throws IOException {
			if (nodes == null) {
				writer.nullValue();
				return;
			}
			writer.name(JsonFieldNames.NODES.fieldName);
			writer.beginArray();
			for (String node : nodes) {
				writer.value(node);
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
