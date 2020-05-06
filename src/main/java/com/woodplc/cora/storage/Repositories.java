package com.woodplc.cora.storage;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.hash.HashCode;
import com.google.common.hash.HashFunction;
import com.google.common.hash.Hasher;
import com.google.common.hash.Hashing;
import com.woodplc.cora.parser.Parsers;

public final class Repositories {

	public final static String DATA_FOLDER = "data";
	private final static String ROOT_FOLDER = ".cora";
	private final static Repository SINGLETON = new FSRepository();
	private final static HashFunction checksumFunction = Hashing.crc32();


	private Repositories() {}

	public static Repository getInstance() {return SINGLETON;}

	public static Path pathForCheckSum(String checkSum) {
		return Paths.get(DATA_FOLDER, ROOT_FOLDER, Objects.requireNonNull(checkSum));
	}
	
	public static String checkSumForPath(Path path) throws IOException {
		try (Stream<Path> stream = Files.walk(path)) {
			
			List<HashCode> codes = stream
				.filter(Parsers::isFortranFile)
				.sorted(Comparator.comparing(Path::toString))
				.map(p -> {
					try {
						return com.google.common.io.Files.asByteSource(p.toFile()).hash(checksumFunction);
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						return null;
					}
				})
				.collect(Collectors.toList());
			return Hashing.combineOrdered(codes).toString();
		}
	}
}
