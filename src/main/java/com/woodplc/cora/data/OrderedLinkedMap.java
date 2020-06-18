package com.woodplc.cora.data;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public final class OrderedLinkedMap<K, V> extends LinkedHashMap<K, V> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private List<K> positions = new ArrayList<>();
	
	@Override
	public V put(K key, V value) {
		V prev = super.put(key, value);
		if (prev == null) {
			positions.add(key);
		}
		return prev;
	}
	
	public K getKey(int index) {
		if (index < 0 || index >= positions.size()) {
			throw new IndexOutOfBoundsException();
		}
		return positions.get(index);
	}
	
	public V getValue(int index) {
		if (index < 0 || index >= positions.size()) {
			throw new IndexOutOfBoundsException();
		}
		return super.get(positions.get(index));
	}

}
