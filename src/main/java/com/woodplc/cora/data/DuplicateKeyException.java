package com.woodplc.cora.data;

public class DuplicateKeyException extends Exception {

	public <K> DuplicateKeyException(K newValue) {
		super((String)newValue);
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = -8469966172594680306L;

}
