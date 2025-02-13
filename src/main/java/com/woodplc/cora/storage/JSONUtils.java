package com.woodplc.cora.storage;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.google.common.collect.SetMultimap;
import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;
import com.google.common.graph.EndpointPair;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.InstanceCreator;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.woodplc.cora.data.FeatureView;
import com.woodplc.cora.data.GuavaBasedSDGraph;
import com.woodplc.cora.data.ModuleVariable;
import com.woodplc.cora.data.SDGraph;
import com.woodplc.cora.data.SubProgram;
import com.woodplc.cora.gui.controllers.CoRAMainController;
import com.woodplc.cora.gui.model.SearchEntryView;
import com.woodplc.cora.storage.gson.adapters.GsonFileAdapter;
import com.woodplc.cora.storage.gson.adapters.GsonPairFxAdapter;
import com.woodplc.cora.storage.gson.adapters.GsonPathAdapter;
import com.woodplc.cora.storage.gson.adapters.GsonSearchEntryViewAdapter;
import com.woodplc.cora.storage.gson.adapters.GsonSetMultimapAdapter;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.util.Pair;

public final class JSONUtils {

	private static final Path STATE_JSON_FILENAME = Paths.get(Repositories.DATA_FOLDER, "last_state.json");
	private static final String SDGRAPH_JSON_FILENAME = "sdgraph.json";
	private static final Gson SDGRAPH_TO_GSON = new GsonBuilder()
			.registerTypeAdapter(SetMultimap.class, new GsonSetMultimapAdapter<String,String>())
			.registerTypeAdapter(Table.class, new TableAdapter<String, String, ModuleVariable>())
			.registerTypeAdapter(MutableGraph.class, new GraphAdapter<String>())
			.registerTypeHierarchyAdapter(SubProgram.class, new SubProgramAdapter())
			.registerTypeHierarchyAdapter(Path.class, new GsonPathAdapter())
			.registerTypeAdapter(EndpointPair.class, new InstanceCreator<EndpointPair<String>>() {
				public EndpointPair<String> createInstance(Type type) {
				     return EndpointPair.ordered("", "");
				}
			})
			.create();

	private static final Gson FEATURE_TO_GSON = new GsonBuilder()
			.serializeNulls()
			.registerTypeHierarchyAdapter(SubProgram.class, new SubProgramAdapter())
			.setPrettyPrinting()
			.create();
	private static final Gson STATE_TO_GSON = new GsonBuilder()
			.serializeNulls()
			.excludeFieldsWithoutExposeAnnotation()
			.registerTypeAdapter(File.class, new GsonFileAdapter())
			.registerTypeHierarchyAdapter(Path.class, new GsonPathAdapter())
			.registerTypeAdapter(Pair.class, new GsonPairFxAdapter())
			.enableComplexMapKeySerialization()
			.registerTypeAdapter(SetMultimap.class, new GsonSetMultimapAdapter<Pair<String, String>,Pair<String, String>>())
			.registerTypeAdapter(SearchEntryView.class, new GsonSearchEntryViewAdapter())
			.registerTypeAdapter(ObservableList.class, new InstanceCreator<ObservableList>() {

				@Override
				public ObservableList createInstance(Type type) {
					return FXCollections.observableArrayList();
				}
				
			})
			.create();
	

	private static final class SubProgramAdapter implements JsonDeserializer<SubProgram>, JsonSerializer<SubProgram> {

		@Override
		public JsonElement serialize(SubProgram src, Type typeOfSrc, JsonSerializationContext context) {
		
			JsonObject jo = new JsonObject();
			jo.addProperty("type", src.getClass().getSimpleName().toLowerCase());
			jo.addProperty("module", src.module());
			jo.addProperty("subname", src.name());
			jo.addProperty("startLine", src.startLine());
			jo.addProperty("endLine", src.endLine());
			jo.addProperty("path", src.path().toString());
			return jo;
		}

		@Override
		public SubProgram deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
				throws JsonParseException {
			JsonObject jo = json.getAsJsonObject();
			return SubProgram.ofType(jo.get("type").getAsString(), jo.get("module").getAsString(), 
					jo.get("subname").getAsString(), jo.get("startLine").getAsInt(), 
					jo.get("endLine").getAsInt(), Paths.get(jo.get("path").getAsString()));
		}
		
	}
	
    private static final class GraphAdapter<K> implements JsonSerializer<MutableGraph<K>>, JsonDeserializer<MutableGraph<K>> {
        private static final Type asMapReturnType;
        static {
            try {
                asMapReturnType = MutableGraph.class.getMethod("edges").getGenericReturnType();
            } catch (NoSuchMethodException e) {
                throw new AssertionError(e);
            }
        }

        @Override
        public JsonElement serialize(MutableGraph<K> src, Type typeOfSrc, JsonSerializationContext context) {
            return context.serialize(src.edges(), asMapType(typeOfSrc));
        }
        
        @Override
        public MutableGraph<K> deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
                throws JsonParseException {
            Set<EndpointPair<K>> edges = context.deserialize(json, asMapType(typeOfT));
            MutableGraph<K> graph = GraphBuilder
        			.directed()
        			.allowsSelfLoops(true)
        			.build();
            for (EndpointPair<K> edge : edges) {
                graph.putEdge(edge.source(), edge.target());
            }
            return graph;
        }

        private static Type asMapType(Type multimapType) {
            return TypeToken.of(multimapType).resolveType(asMapReturnType).getType();
        }
    }
    
    private static final class TableAdapter<R,C,V> implements JsonSerializer<Table<R,C,V>>, JsonDeserializer<Table<R,C,V>> {
        private static final Type asMapReturnType;
        static {
            try {
                asMapReturnType = Table.class.getDeclaredMethod("rowMap").getGenericReturnType();
            } catch (NoSuchMethodException e) {
                throw new AssertionError(e);
            }
        }

        @Override
        public JsonElement serialize(Table<R,C,V> src, Type typeOfSrc, JsonSerializationContext context) {
            return context.serialize(src.rowMap(), asMapType(typeOfSrc));
        }
        
        @Override
        public Table<R,C,V> deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
                throws JsonParseException {
            Map<R, Map<C,V>> asMap = context.deserialize(json, asMapType(typeOfT));
            Table<R,C,V> table = (Table<R, C, V>) TreeBasedTable.create(String.CASE_INSENSITIVE_ORDER, String.CASE_INSENSITIVE_ORDER);
            for (Map.Entry<R, Map<C,V>> entry : asMap.entrySet()) {
            	for (Map.Entry<C, V> entry2 : entry.getValue().entrySet()) {
            		table.put(entry.getKey(), entry2.getKey(), entry2.getValue());
            	}
            }
            return table;
        }

        private static Type asMapType(Type multimapType) {
            return TypeToken.of(multimapType).resolveType(asMapReturnType).getType();
        }
    }
	
	private JSONUtils() {}
	
	public static SDGraph graphFromJson(Path entryPath) throws IOException {
		Objects.requireNonNull(entryPath);
		Path graphFilePath = Paths.get(entryPath.toString(), SDGRAPH_JSON_FILENAME);
		
		if (!Files.exists(graphFilePath)) throw new IllegalArgumentException();
		SDGraph graph = SDGRAPH_TO_GSON.fromJson(Files.newBufferedReader(graphFilePath), GuavaBasedSDGraph.class);
		graph.addGraphNodes(graph.subprograms());
		return graph;
	}

	public static void graphToJson(Path entryPath, SDGraph graph) throws IOException {
		Objects.requireNonNull(entryPath);
		Objects.requireNonNull(graph);

		if (!Files.isDirectory(entryPath)) throw new IllegalArgumentException();

		Path graphFilePath = Paths.get(entryPath.toString(), SDGRAPH_JSON_FILENAME);
		String jsonString = SDGRAPH_TO_GSON.toJson(graph);

		Files.write(graphFilePath, jsonString.getBytes());
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

	public static CoRAMainController stateFromJson(Path path) throws IOException {
		if (Files.notExists(path)) throw new IllegalStateException();
		return STATE_TO_GSON.fromJson(Files.newBufferedReader(path), CoRAMainController.class);
	}
	
	public static CoRAMainController stateFromJson() throws IOException {
		return stateFromJson(STATE_JSON_FILENAME);
	}
	
	public static void stateToJson(CoRAMainController controller) throws IOException {
		stateToJson(controller, STATE_JSON_FILENAME);
	}
	
	public static void stateToJson(CoRAMainController controller, Path path) throws IOException {
		Objects.requireNonNull(controller);
		String jsonString = STATE_TO_GSON.toJson(controller);

		Files.createDirectories(Paths.get(Repositories.DATA_FOLDER));
		Files.write(path, jsonString.getBytes());
	}
}
