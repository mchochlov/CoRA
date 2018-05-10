package com.woodplc.cora.gui.controllers;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

import org.junit.jupiter.api.Test;

import com.woodplc.cora.utils.TestUtils.SoftwareSystem;

class CoRAMainControllerTest {
	
	@Test
	void testEachProgramParserRunsInOwnThread() throws Exception {
		
		CoRAMainController.ParseTask flex3ParseTask = spy(new CoRAMainController.ParseTask(SoftwareSystem.FLEX3.path()));
		CoRAMainController.ParseTask dprflex3ParseTask = spy(new CoRAMainController.ParseTask(SoftwareSystem.DPRFLEX3.path()));
		CoRAMainController.ParseTask mamParseTask = spy(new CoRAMainController.ParseTask(SoftwareSystem.MAM.path()));
		
		doReturn(null).when(flex3ParseTask).call();
		doReturn(null).when(dprflex3ParseTask).call();
		doReturn(null).when(mamParseTask).call();
		
		Thread currentThread = Thread.currentThread();
		Thread flex3Thread = new Thread(flex3ParseTask);
		Thread dprflex3Thread = new Thread(dprflex3ParseTask);
		Thread mamThread = new Thread(mamParseTask);
		
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
