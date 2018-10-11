package com.woodplc.cora.gui.model;

import java.util.Objects;

import com.woodplc.cora.ir.SearchEntry;

import javafx.beans.property.SimpleFloatProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;

public class SearchEntryView {

	private final SimpleIntegerProperty param;
	private final SimpleFloatProperty score;
	private final SimpleStringProperty name;
	
	public SearchEntryView(int param, SearchEntry res) {
		this(param, res.getScore(), res.getName());
	}

	public SearchEntryView(int param, float score, String name) {
		if (param < 0 || score < 0 || name == null || name.isEmpty() ) {
			throw new IllegalArgumentException();
		}
		this.param = new SimpleIntegerProperty(param);
		this.score = new SimpleFloatProperty(score);
		this.name = new SimpleStringProperty(name);
	}
	
	public int getParam() {return param.get();}

	public float getScore() {return score.get();}
	public String getName() {return name.get();}
	
	public void setParam(int param) {this.param.set(param);}
	public void setScore(float score) {this.score.set(score);}
	public void setName(String name) {this.name.set(name);}
	
	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof SearchEntryView)) return false;
		SearchEntryView ev = (SearchEntryView) o;
		return this.param.intValue() == ev.param.intValue()
				&& this.score.floatValue() == ev.score.floatValue()
				&& this.name.getValue().equals(ev.name.getValue());
	}

	@Override
	public int hashCode() {
		return Objects.hash(param.intValue(), score.floatValue(), name.getValue());
	}
}
