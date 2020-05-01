package com.woodplc.cora.utils;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.MalformedInputException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.woodplc.cora.data.SubProgram;

public final class Utils {
	
	final static Logger logger = LoggerFactory.getLogger(Utils.class);
	
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
	
	public static List<String> readFullLinesFromFile(Path path) throws IOException {
		Objects.requireNonNull(path);
		try {
			return readFullLines(path, StandardCharsets.UTF_8);
		} catch (IOException e) {
			if (!(e instanceof MalformedInputException)) {
				throw e;
			}
			return tryWithOtherCharsetsFull(path);
		}

	}

	private static List<String> tryWithOtherCharsetsFull(Path path) throws IOException {
		for (Charset cs : STANDARD_CHARSETS) {
			try {
				//System.out.println(cs);
				return readFullLines(path, cs);
			} catch (IOException e) {
				if (!(e instanceof MalformedInputException)) {
					throw e;
				}
			}
		}
		
		for (Charset cs : Charset.availableCharsets().values()) {
			try {
				//System.out.println(cs);
				return readFullLines(path, cs);
			} catch (IOException e) {
				if (!(e instanceof MalformedInputException)) {
					throw e;
				}
			}
		}
		
		return null;
	}

	private static List<String> readFullLines(Path path, Charset charset) throws IOException {
		try (BufferedReader br = Files.newBufferedReader(path, charset)) {
			List<String> content = new ArrayList<>();
			int ch;
			char prev = 0;
			StringBuilder sb = new StringBuilder();
			while ((ch = br.read()) != -1) {
				char c = (char) ch;
				// \r = 13 \n = 10
				if (c == '\n') {
					sb.append(c);
					content.add(sb.toString());
					sb.setLength(0);
				} else {
					if (prev == '\r') {
						content.add(sb.toString());
						sb.setLength(0);
					}
					sb.append(c);
				}
				prev = c;
			}
			if (sb.length() != 0) {
				content.add(sb.toString());
			}
			return content;
		}
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

	public static void updateSubprogramInFile(List<String> originalFile, List<String> autoRefactoredSubprogram,
			SubProgram subprogram) throws IOException {
		Path tmpFile = Files.createTempFile(null, null);
		try (BufferedWriter br = Files.newBufferedWriter(tmpFile, StandardOpenOption.APPEND)){
			for (int i = 0; i < subprogram.startLine() - 1; i++) {
				br.write(originalFile.get(i));
				//br.write("\r\n");
			}
			for (String s : autoRefactoredSubprogram) {
				br.write(s);
				br.write("\r\n");
			}
			for (int i = subprogram.endLine(); i < originalFile.size(); i++) {
				br.write(originalFile.get(i));
				//br.write("\r\n");
			}		
			//br.write(originalFile.get(originalFile.size() - 1));
		}
		try {
			Files.move(tmpFile, subprogram.path(), StandardCopyOption.ATOMIC_MOVE);
		} catch (AtomicMoveNotSupportedException ae) {
			logger.warn("Atomic move not supported: attempting non-atomic replacement.");
			Files.move(tmpFile, subprogram.path(), StandardCopyOption.REPLACE_EXISTING);
		}
		
	}
}
