package com.woodplc.cora.data;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.stream.Collectors;

public final class OrderedCaseInsensitiveSet<K> {

	private final Map<K, Integer> uniqueValues;
	private final List<K> orderedValues = new ArrayList<>();
	
	public OrderedCaseInsensitiveSet(Comparator<K> c) {
		 uniqueValues = new TreeMap<>(c);
	}
	
	public void add(K value) {
		if (!uniqueValues.containsKey(value)) {
			orderedValues.add(value);
			uniqueValues.put(value, orderedValues.size() - 1);			
		}
	}
	
	public void remove(K value) {
		int index = uniqueValues.remove(value);
		orderedValues.set(index, null);
	}
	
	public List<K> allValues() {
		return new ArrayList<>(orderedValues);
	}
	
	public List<K> nonNullValues() {
		return orderedValues.stream()
				.filter(v -> v != null)
				.collect(Collectors.toList());
	}

	public boolean contains(K value) {
		return uniqueValues.containsKey(value);
	}
	
	public boolean isEmpty() {
		return orderedValues.size() == 0;
	}

	public int size() {
		return orderedValues.size();
	}
	
	public K get(int index) {
		return orderedValues.get(index);
	}

	public static OrderedCaseInsensitiveSet<String> from(OrderedCaseInsensitiveSet<String> argumentSet) {
		OrderedCaseInsensitiveSet<String> set = new OrderedCaseInsensitiveSet<>(String.CASE_INSENSITIVE_ORDER);
		set.orderedValues.addAll(argumentSet.orderedValues);
		set.uniqueValues.putAll(argumentSet.uniqueValues);
		return set;
	}

	public void update(K oldValue, K newValue) throws DuplicateKeyException {
		Integer position = uniqueValues.remove(oldValue);
		if (position == null) {
			throw new IllegalStateException();
		}
		
		if (uniqueValues.put(newValue, position) != null) {
			//rollback
			uniqueValues.put(oldValue, position);
			throw new DuplicateKeyException("Error: key already exists " + newValue);
		}
		
		orderedValues.set(position, newValue);		
	}

	@Override
	public int hashCode() {
		return Objects.hash(orderedValues, uniqueValues);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		OrderedCaseInsensitiveSet<K> other = (OrderedCaseInsensitiveSet<K>) obj;
		return this.uniqueValues.equals(other.uniqueValues) &&
				this.orderedValues.equals(other.orderedValues);
	}
}
