package com.woodplc.cora.gui.model;

import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;

public class EntityView {

	private final SimpleIntegerProperty param;
	private final SimpleStringProperty name;
	
	public EntityView(int param, String name) {
		if (param < 0 || name == null || name.isEmpty()) {
			throw new IllegalArgumentException();
		}
		this.param = new SimpleIntegerProperty(param);
		this.name = new SimpleStringProperty(name);
	}

	public int getParam() {return param.get();}

	public String getName() {return name.get();}
	
	public void setParam(int param) {this.param.set(param);}
	
	public void setName(String name) {this.name.set(name);}
}
