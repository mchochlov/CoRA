package com.woodplc.cora.data;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.woodplc.cora.gui.model.SearchEntryView;

import javafx.collections.ObservableList;

public final class ApplicationState {
	
	private final String lastKnownDir;
	private final String searchQuery;
	private final List<SimpleView> searchResults;
	private final ModuleState stateA;
	private final ModuleState stateB;
	private final ModuleState stateC;
	private final ModuleState stateCaf;
	private final FeatureState fState;
		
	private static class ModuleState {
		private final String path;
		private final String checkSum;
		
		private ModuleState(Path path, String checkSum) {
			this.path = path == null ? null : path.toString();
			this.checkSum = checkSum;
		}

		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof ModuleState)) return false;
			ModuleState ms = (ModuleState) o;
			return Objects.equals(this.path, ms.path)
					&& Objects.equals(this.checkSum, ms.checkSum);
		}

		@Override
		public int hashCode() {
			return Objects.hash(path, checkSum);
		}
	}
	
	private static class FeatureState {
		private final List<String> subprogramsA = new ArrayList<>();
		private final List<String> subprogramsB = new ArrayList<>();
		private final List<String> subprogramsC = new ArrayList<>();
		
		private FeatureState(List<String> subprogramsA,
				List<String> subprogramsB,
				List<String> subprogramsC) {
			Objects.requireNonNull(subprogramsA);
			Objects.requireNonNull(subprogramsB);
			Objects.requireNonNull(subprogramsC);
			if (!subprogramsA.isEmpty()) this.subprogramsA.addAll(subprogramsA);
			if (!subprogramsB.isEmpty()) this.subprogramsB.addAll(subprogramsB);
			if (!subprogramsC.isEmpty()) this.subprogramsC.addAll(subprogramsC);
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof FeatureState)) return false;
			FeatureState fs = (FeatureState) o;
			return this.subprogramsA.equals(fs.subprogramsA)
					&& this.subprogramsB.equals(fs.subprogramsB)
					&& this.subprogramsC.equals(fs.subprogramsC);
		}

		@Override
		public int hashCode() {
			return Objects.hash(subprogramsA, subprogramsB, subprogramsC);
		}
		
	}
	
	private static class SimpleView {
		private final int param;
		private final float score;
		private final String name;
		
		private SimpleView(int param, float score, String name) {
			if (param < 0 || score < 0 || name == null || name.isEmpty()) throw new IllegalArgumentException();
			
			this.param = param;
			this.score = score;
			this.name = name;
		}

		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof SimpleView)) return false;
			SimpleView sv = (SimpleView) o;
			return this.param == sv.param
					&& this.score == sv.score
					&& Objects.equals(this.name, sv.name);
		}

		@Override
		public int hashCode() {
			return Objects.hash(param, score, name);
		}
	}
		
	public ApplicationState(File lastKnownDir,
			String searchQuery,
			ObservableList<SearchEntryView> searchResults,
			ModuleContainer moduleA,
			ModuleContainer moduleB,
			ModuleContainer moduleC,
			ModuleContainer cafModule,
			Feature feature) {
		Objects.requireNonNull(searchResults);
		Objects.requireNonNull(moduleA);
		Objects.requireNonNull(moduleB);
		Objects.requireNonNull(moduleC);
		Objects.requireNonNull(cafModule);
		Objects.requireNonNull(feature);
		this.lastKnownDir = lastKnownDir == null ? null : lastKnownDir.toString();
		this.searchQuery = searchQuery;
		this.searchResults = searchResults.isEmpty() ? new ArrayList<>() : 
				searchResults.stream()
				.map(ev -> new SimpleView(ev.getParam(), ev.getScore(), ev.getName()))
				.collect(Collectors.toList());
		this.stateA = new ModuleState(moduleA.getPath(), moduleA.getCheckSum());
		this.stateB = new ModuleState(moduleB.getPath(), moduleB.getCheckSum());
		this.stateC = new ModuleState(moduleC.getPath(), moduleC.getCheckSum());
		this.stateCaf = new ModuleState(cafModule.getPath(), cafModule.getCheckSum());
		this.fState = new FeatureState(feature.readOnlySystemASubprograms(), 
				feature.readOnlySystemBSubprograms(), 
				feature.readOnlySystemCSubprograms());
	}

	public String getLastKnownDir() {return lastKnownDir;}

	public String getSearchQuery() {return searchQuery;}

	public List<SearchEntryView> getSearchResults() {
		return Collections.unmodifiableList(searchResults.stream()
				.map(sv -> new SearchEntryView(sv.param, sv.score, sv.name))
				.collect(Collectors.toList()));
	}

	public ModuleContainer getModuleA() {return ModuleContainer.fromValues(stateA.path, stateA.checkSum);}

	public ModuleContainer getModuleB() {return ModuleContainer.fromValues(stateB.path, stateB.checkSum);}

	public ModuleContainer getModuleC() {return ModuleContainer.fromValues(stateC.path, stateC.checkSum);}
	
	public ModuleContainer getCafModule() {return ModuleContainer.fromValues(stateCaf.path, stateCaf.checkSum);	}

	public Feature getFeature() {
		return new Feature(Collections.unmodifiableList(fState.subprogramsA), 
			Collections.unmodifiableList(fState.subprogramsB), 
			Collections.unmodifiableList(fState.subprogramsC));
	}
	
	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof ApplicationState)) return false;
		ApplicationState as = (ApplicationState) o;
		
		return Objects.equals(this.lastKnownDir, as.lastKnownDir)
				&& Objects.equals(this.searchQuery, as.searchQuery)
				&& this.searchResults.equals(as.searchResults)
				&& this.stateA.equals(as.stateA)
				&& this.stateB.equals(as.stateB)
				&& this.stateC.equals(as.stateC)
				&& this.stateCaf.equals(as.stateCaf)
				&& this.fState.equals(as.fState);
	}

	@Override
	public int hashCode() {
		return Objects.hash(lastKnownDir, searchQuery, 
				searchResults, stateA, stateB, stateC, stateCaf, fState);
	}

}
