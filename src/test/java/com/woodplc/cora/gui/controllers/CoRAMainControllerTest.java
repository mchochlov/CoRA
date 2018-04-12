package com.woodplc.cora.gui.controllers;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.utils.TestUtils;

class CoRAMainControllerTest {
	
	@Test
	void testEachProgramParserRunsInOwnThread() {
		Thread currentThread = Thread.currentThread();
		Thread flex3Thread = new Thread(new CoRAMainController.ParseTask(TestUtils.SoftwareSystem.FLEX3.path()));
		Thread dprflex3Thread = new Thread(new CoRAMainController.ParseTask(TestUtils.SoftwareSystem.DPRFLEX3.path()));
		Thread mamThread = new Thread(new CoRAMainController.ParseTask(TestUtils.SoftwareSystem.MAM.path()));
		
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
