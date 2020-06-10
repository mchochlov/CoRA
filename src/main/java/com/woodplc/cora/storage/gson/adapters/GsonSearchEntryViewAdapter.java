package com.woodplc.cora.storage.gson.adapters;

import java.lang.reflect.Type;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.woodplc.cora.gui.model.SearchEntryView;

public class GsonSearchEntryViewAdapter implements JsonDeserializer<SearchEntryView>, JsonSerializer<SearchEntryView> {

	@Override
	public JsonElement serialize(SearchEntryView src, Type typeOfSrc, JsonSerializationContext context) {
		JsonObject jo = new JsonObject();
		jo.addProperty("param", src.getParam());
		jo.addProperty("score", src.getScore());
		jo.addProperty("name", src.getName());
		return jo;
	}

	@Override
	public SearchEntryView deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
			throws JsonParseException {
		JsonObject jo = json.getAsJsonObject();
		return new SearchEntryView(jo.get("param").getAsInt(), jo.get("score").getAsFloat(), jo.get("name").getAsString());
	}

}
