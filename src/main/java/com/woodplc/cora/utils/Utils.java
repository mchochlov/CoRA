package com.woodplc.cora.utils;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.MalformedInputException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

public final class Utils {
	
	private Utils() {}
	
	private static final List<Charset> STANDARD_CHARSETS = List.of(Charset.forName("windows-1252"), StandardCharsets.ISO_8859_1, StandardCharsets.US_ASCII, 
			StandardCharsets.UTF_8);
	
	public static enum RegEx{
		COMMA(","),
		WHITESPACE("\\s+");
		
		private final String regex;
		
		RegEx(String regex) {
			this.regex = regex;
		}
		
		public String regex() {return regex;}
	}

	public static List<String> readAllFromFile(Path path) throws IOException {
		Objects.requireNonNull(path);
		try {
			return Files.readAllLines(path);
		} catch (IOException e) {
			if (!(e instanceof MalformedInputException)) {
				throw e;
			}
			return tryWithOtherCharsets(path);
		}
	}

	private static List<String> tryWithOtherCharsets(Path path) throws IOException {
		for (Charset cs : STANDARD_CHARSETS) {
			try {
				//System.out.println(cs);
				return Files.readAllLines(path, cs);
			} catch (IOException e) {
				if (!(e instanceof MalformedInputException)) {
					throw e;
				}
			}
		}
		
		for (Charset cs : Charset.availableCharsets().values()) {
			try {
				//System.out.println(cs);
				return Files.readAllLines(path, cs);
			} catch (IOException e) {
				if (!(e instanceof MalformedInputException)) {
					throw e;
				}
			}
		}
		
		return null;
	}
}
