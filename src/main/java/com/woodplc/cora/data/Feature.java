package com.woodplc.cora.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.woodplc.cora.gui.model.CallDependencyView;
import com.woodplc.cora.gui.model.RefactoringCaseView;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;

public final class Feature {
	
	private static final String FLEXCOM_MODULE = "flex3";
	private static final String DEEPRISER_MODULE = "dprflex3";
	private static final String PIPELAY_MODULE = "mam";

	private static final int MAX_NUMBER_OF_CLONES = 3;
	private final ObservableList<String> systemASubprograms = FXCollections.observableArrayList();
	private final ObservableList<String> systemBSubprograms = FXCollections.observableArrayList();
	private final ObservableList<String> systemCSubprograms = FXCollections.observableArrayList();
	private final ObservableList<RefactoringCaseView> refactoringCaseList = FXCollections.observableArrayList();
	
	public ObservableList<RefactoringCaseView> getRefactoringCaseList() {
		return refactoringCaseList;
	}

	private final Map<String, List<String>> flToCase = new HashMap<>();
	private final Map<String, List<String>> drToCase = new HashMap<>();
	private final Map<String, List<String>> plToCase = new HashMap<>();
	
	private final Map<String, RefactoringCaseView> flToCaseRef = new HashMap<>();
	private final Map<String, RefactoringCaseView> drToCaseRef = new HashMap<>();
	private final Map<String, RefactoringCaseView> plToCaseRef = new HashMap<>();

	
	public Feature() {}

	public Feature(List<String> subprogramsA, 
			List<String> subprogramsB, 
			List<String> subprogramsC) {
		Objects.requireNonNull(subprogramsA);
		Objects.requireNonNull(subprogramsB);
		Objects.requireNonNull(subprogramsC);
		if (!subprogramsA.isEmpty()) this.systemASubprograms.addAll(subprogramsA);
		if (!subprogramsB.isEmpty()) this.systemBSubprograms.addAll(subprogramsB);
		if (!subprogramsC.isEmpty()) this.systemCSubprograms.addAll(subprogramsC);
	}
	
	public boolean isEmpty() {
		return this.systemASubprograms.isEmpty()
				&& this.systemBSubprograms.isEmpty()
				&& this.systemCSubprograms.isEmpty();
	}
	
	public ObservableList<String> systemASubprograms() {return systemASubprograms;}

	public ObservableList<String> systemBSubprograms() {return systemBSubprograms;}
	
	public ObservableList<String> systemCSubprograms() {return systemCSubprograms;}
	
	public List<String> readOnlySystemASubprograms() {return Collections.unmodifiableList(systemASubprograms);}
	
	public List<String> readOnlySystemBSubprograms() {return Collections.unmodifiableList(systemBSubprograms);}
	
	public List<String> readOnlySystemCSubprograms() {return Collections.unmodifiableList(systemCSubprograms);}

	public void addRefactoringCasesFromSearch(String path, List<String> selectedItems) {
		if (selectedItems.isEmpty()) return;
		addRefactoringCases(path, selectedItems);
	}

	public void addRefactoringCasesFromAdj(String path, ObservableList<CallDependencyView> selectedItems) {
		if(selectedItems.isEmpty()) return;
		List<String> str = selectedItems.stream().map(CallDependencyView::getName).collect(Collectors.toList());
		addRefactoringCases(path, str);
	}
	
	public void addRefactoringCasesFromVar(String path, ObservableList<TreeItem<String>> selectedItems) {
		if(selectedItems.isEmpty()) return;
		List<String> str = selectedItems.stream().map(TreeItem::getValue).collect(Collectors.toList());
		addRefactoringCases(path, str);
	}
	
	private void addRefactoringCases(String path, List<String> selectedItems) {
		if (path.equalsIgnoreCase(FLEXCOM_MODULE)) {
			for (String searchEntryView : selectedItems) {
				if (!flToCase.containsKey(searchEntryView)) {
					List<String> entries = new ArrayList<>(MAX_NUMBER_OF_CLONES);
					entries.add(0, searchEntryView);
					entries.add(1, "");
					entries.add(2, "");
					flToCase.put(searchEntryView, entries);
					RefactoringCaseView rcv = new RefactoringCaseView(searchEntryView, entries.get(0), entries.get(1), entries.get(2));
					flToCaseRef.put(searchEntryView, rcv);
					refactoringCaseList.add(rcv);
				}
			}
		}
		
		if (path.equalsIgnoreCase(DEEPRISER_MODULE)) {
			for (String searchEntryView : selectedItems) {
				if (!drToCase.containsKey(searchEntryView)) {
					List<String> entries = new ArrayList<>(MAX_NUMBER_OF_CLONES);
					entries.add(0, "");
					entries.add(1, searchEntryView);
					entries.add(2, "");
					drToCase.put(searchEntryView, entries);
					RefactoringCaseView rcv = new RefactoringCaseView(searchEntryView, entries.get(0), entries.get(1), entries.get(2));
					drToCaseRef.put(searchEntryView, rcv);
					refactoringCaseList.add(rcv);

				}
			}
		}
		
		if (path.equalsIgnoreCase(PIPELAY_MODULE)) {
			for (String searchEntryView : selectedItems) {
				if (!plToCase.containsKey(searchEntryView)) {
					List<String> entries = new ArrayList<>(MAX_NUMBER_OF_CLONES);
					entries.add(0, "");
					entries.add(1, "");
					entries.add(2, searchEntryView);
					plToCase.put(searchEntryView, entries);
					RefactoringCaseView rcv = new RefactoringCaseView(searchEntryView, entries.get(0), entries.get(1), entries.get(2));
					plToCaseRef.put(searchEntryView, rcv);
					refactoringCaseList.add(rcv);
				}
			}
		}
	}

	public void removeRefactoringCases(String path, List<String> selectedItems) {
		if(selectedItems.isEmpty()) return;
		if (path.equalsIgnoreCase(FLEXCOM_MODULE)) {
			for (String searchEntryView : selectedItems) {
				if (flToCase.containsKey(searchEntryView)) {
					List<String> entries = flToCase.remove(searchEntryView);
					entries.set(0, "");
					RefactoringCaseView rcv = flToCaseRef.remove(searchEntryView);
					rcv.setFlName("");
					if (rcv.getDrName().equals("") && rcv.getPlName().equals("")) {
						refactoringCaseList.remove(rcv);						
					}
				} else {
					throw new IllegalStateException();
				}
			}
		}
		
		if (path.equalsIgnoreCase(DEEPRISER_MODULE)) {
			for (String searchEntryView : selectedItems) {
				if (drToCase.containsKey(searchEntryView)) {
					List<String> entries = drToCase.remove(searchEntryView);
					entries.set(1, "");
					RefactoringCaseView rcv = drToCaseRef.remove(searchEntryView);
					rcv.setDrName("");
					if (rcv.getFlName().equals("") && rcv.getPlName().equals("")) {
						refactoringCaseList.remove(rcv);						
					}
				} else {
					throw new IllegalStateException();
				}
			}
		}

		if (path.equalsIgnoreCase(PIPELAY_MODULE)) {
			for (String searchEntryView : selectedItems) {
				if (plToCase.containsKey(searchEntryView)) {
					List<String> entries = plToCase.remove(searchEntryView);
					entries.set(2, "");
					RefactoringCaseView rcv = plToCaseRef.remove(searchEntryView);
					rcv.setPlName("");
					if (rcv.getFlName().equals("") && rcv.getDrName().equals("")) {
						refactoringCaseList.remove(rcv);						
					}
				} else {
					throw new IllegalStateException();
				}
			}
		}

	}

	public void addRefactoringCasesFromClones(String pathA, String path, String subname, Set<String> clones) {
		if(clones.size() != 1) throw new UnsupportedOperationException();
		String clone = new ArrayList<>(clones).get(0);
		if (pathA.equalsIgnoreCase(FLEXCOM_MODULE)) {
			
			if (!flToCase.containsKey(subname)) throw new IllegalStateException();
			List<String> entries = flToCase.get(subname);
			if(path.equalsIgnoreCase(DEEPRISER_MODULE)) {
				entries.set(1, clone);
				drToCase.put(clone, entries);
				RefactoringCaseView rcv = flToCaseRef.get(subname);
				rcv.setDrName(clone);
				drToCaseRef.put(clone, rcv);
			} else if (path.equalsIgnoreCase(PIPELAY_MODULE)) {
				entries.set(2, clone);
				plToCase.put(clone, entries);				
				RefactoringCaseView rcv = flToCaseRef.get(subname);
				rcv.setPlName(clone);
				plToCaseRef.put(clone, rcv);
			}
			
		}
		if (pathA.equalsIgnoreCase(DEEPRISER_MODULE)) {
			
			if (!drToCase.containsKey(subname)) throw new IllegalStateException();
			List<String> entries = drToCase.get(subname);
			if(path.equalsIgnoreCase(FLEXCOM_MODULE)) {
				entries.set(0, clone);
				flToCase.put(clone, entries);
				RefactoringCaseView rcv = drToCaseRef.get(subname);
				rcv.setFlName(clone);
				flToCaseRef.put(clone, rcv);
			} else if (path.equalsIgnoreCase(PIPELAY_MODULE)) {
				entries.set(2, clone);
				plToCase.put(clone, entries);				
				RefactoringCaseView rcv = drToCaseRef.get(subname);
				rcv.setPlName(clone);
				plToCaseRef.put(clone, rcv);
			}
			
		}
		if (pathA.equalsIgnoreCase(PIPELAY_MODULE)) {
			
			if (!plToCase.containsKey(subname)) throw new IllegalStateException();
			List<String> entries = plToCase.get(subname);
			if(path.equalsIgnoreCase(FLEXCOM_MODULE)) {
				entries.set(0, clone);
				flToCase.put(clone, entries);
				RefactoringCaseView rcv = plToCaseRef.get(subname);
				rcv.setFlName(clone);
				flToCaseRef.put(clone, rcv);

			} else if (path.equalsIgnoreCase(DEEPRISER_MODULE)) {
				entries.set(1, clone);
				plToCase.put(clone, entries);
				RefactoringCaseView rcv = plToCaseRef.get(subname);
				rcv.setDrName(clone);
				drToCaseRef.put(clone, rcv);
			}
			
		}
		
	}


}