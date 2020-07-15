package com.woodplc.cora.storage.gson.adapters;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;

import com.google.common.collect.SetMultimap;
import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.reflect.TypeToken;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

public final class GsonSetMultimapAdapter<K, V> implements JsonDeserializer<SetMultimap<K,V>>, JsonSerializer<SetMultimap<K,V>> {

	private static final Type asMapReturnType;
    static {
        try {
            asMapReturnType = SetMultimap.class.getDeclaredMethod("asMap").getGenericReturnType();
        } catch (NoSuchMethodException e) {
            throw new AssertionError(e);
        }
    }

    @Override
    public JsonElement serialize(SetMultimap<K, V> src, Type typeOfSrc, JsonSerializationContext context) {
        return context.serialize(src.asMap(), asMapType(typeOfSrc));
    }
    @Override
    public SetMultimap<K, V> deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
            throws JsonParseException {
        Map<K, Collection<V>> asMap = context.deserialize(json, asMapType(typeOfT));
        SetMultimap<K, V> multimap = SetMultimapBuilder
    			.hashKeys()
    			.hashSetValues()
    			.build();
        for (Map.Entry<K, Collection<V>> entry : asMap.entrySet()) {
            multimap.putAll(entry.getKey(), entry.getValue());
        }
        return multimap;
    }

    private static Type asMapType(Type multimapType) {
        return TypeToken.of(multimapType).resolveType(asMapReturnType).getType();
    }

}
