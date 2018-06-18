package com.woodplc.cora.gui.model;

import java.util.Objects;

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
	
	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof EntityView)) return false;
		EntityView ev = (EntityView) o;
		return this.param.intValue() == ev.param.intValue()
				&& this.name.getValue().equals(ev.name.getValue());
	}

	@Override
	public int hashCode() {
		return Objects.hash(param, name);
	}
}
