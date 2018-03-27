package com.woodplc.cora.utils;

import java.nio.file.Path;
import java.nio.file.Paths;

public final class TestUtils {

	private static final Path flex3Path = Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\FlexcomAnalysis\\Flex3");
	private static final Path dprflex3Path = Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\DeepRiserAnalysis\\Dprflex3");
	private static final Path mamPath = Paths.get("C:\\Users\\muslim.chochlov\\Projects\\Source\\PipeLayAnalysis\\mam");
	
	private TestUtils() {}

	public static Path pathToFlex3() {
		return flex3Path;
	}
	
	public static Path pathToDprflex3() {
		return dprflex3Path;
	}
	
	public static Path pathToMam() {
		return mamPath;
	}
}
