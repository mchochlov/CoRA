package com.woodplc.cora.gui.controllers;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.utils.TestUtils;

class CoRAMainControllerTest {
	
	private static final long SLEEP_DURATION = 5000;

	@Test
	void testEachProgramParserRunsInOwnThread() {
		Thread currentThread = Thread.currentThread();
		Thread flex3Thread = new Thread(new CoRAMainController.ParseTask(TestUtils.pathToFlex3()));
		Thread dprflex3Thread = new Thread(new CoRAMainController.ParseTask(TestUtils.pathToDprflex3()));
		Thread mamThread = new Thread(new CoRAMainController.ParseTask(TestUtils.pathToMam()));
		
		flex3Thread.start();
		dprflex3Thread.start();
		mamThread.start();
		
		assertAll(
				() -> assertNotSame(currentThread, flex3Thread),
				() -> assertNotSame(currentThread, dprflex3Thread),
				() -> assertNotSame(currentThread, mamThread),
				() -> assertNotSame(flex3Thread, dprflex3Thread),
				() -> assertNotSame(flex3Thread, mamThread),
				() -> assertNotSame(dprflex3Thread, mamThread)
		);
	}

}
