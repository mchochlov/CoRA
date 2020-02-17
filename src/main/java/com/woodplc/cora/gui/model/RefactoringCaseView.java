package com.woodplc.cora.gui.model;

import javafx.beans.property.SimpleStringProperty;

public class RefactoringCaseView {
	
	private final SimpleStringProperty name;
	private final SimpleStringProperty flName;
	private final SimpleStringProperty drName;
	private final SimpleStringProperty plName;
	
	public RefactoringCaseView(String name, String flName, String drName,
			String plName) {
		this.name = new SimpleStringProperty(name);
		this.flName = new SimpleStringProperty(flName);
		this.drName = new SimpleStringProperty(drName);
		this.plName = new SimpleStringProperty(plName);
	}

	public void setName(String name) {
		this.name.set(name);
	}

	public void setFlName(String flName) {
		this.flName.set(flName);
	}

	public void setDrName(String drName) {
		this.drName.set(drName);
	}

	public void setPlName(String plName) {
		this.plName.set(plName);
	}

	public String getName() {
		return name.get();
	}
	
	public String getFlName() {
		return flName.get();
	}

	public String getDrName() {
		return drName.get();
	}

	public String getPlName() {
		return plName.get();
	}

	public SimpleStringProperty nameProperty() {return this.name;}
	public SimpleStringProperty flNameProperty() {return this.flName;}
	public SimpleStringProperty drNameProperty() {return this.drName;}
	public SimpleStringProperty plNameProperty() {return this.plName;}
/*	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof RefactoringCaseView)) return false;
		RefactoringCaseView ev = (RefactoringCaseView) o;
		return this.name.getValue().equals(ev.name.getValue())
				&& this.flName.getValue().equals(ev.flName.getValue())
				&& this.drName.getValue().equals(ev.drName.getValue())
				&& this.plName.getValue().equals(ev.plName.getValue());
	}

	@Override
	public int hashCode() {
		return Objects.hash(name.getValue(), flName.getValue(), drName.getValue(), plName.getValue());
	}*/
}
