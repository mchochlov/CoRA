package com.woodplc.cora.storage.gson.adapters;

import java.lang.reflect.Type;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import javafx.util.Pair;

public final class GsonPairFxAdapter implements JsonDeserializer<Pair>, JsonSerializer<Pair> {

	@Override
	public JsonElement serialize(Pair src, Type typeOfSrc, JsonSerializationContext context) {
		JsonObject jo = new JsonObject();
		jo.addProperty("key", (String)src.getKey());
		jo.addProperty("value", (String)src.getValue());
		return jo;
	}

	@Override
	public Pair deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
			throws JsonParseException {
		JsonObject jo = json.getAsJsonObject();
		return new Pair(jo.get("key").getAsString(), jo.get("value").getAsString());
	}

}
