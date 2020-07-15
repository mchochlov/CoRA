package com.woodplc.cora.data;

import java.util.Comparator;
import java.util.Map;
import java.util.Objects;

import org.antlr.v4.runtime.ParserRuleContext;

import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;

public class DefAndUseMapper {
	
	private final Table<String, Integer, ParserRuleContext> definitions;
	private final Table<String, Integer, ParserRuleContext> uses;
	
	public DefAndUseMapper() {
		definitions = TreeBasedTable.create(String.CASE_INSENSITIVE_ORDER, Comparator.naturalOrder());
		uses = TreeBasedTable.create(String.CASE_INSENSITIVE_ORDER, Comparator.naturalOrder());
	}
	
	public void addDefinition(String variable, int line, ParserRuleContext ctx) {
		Objects.requireNonNull(variable);
		Objects.requireNonNull(ctx);
		if (line <= 0) throw new IllegalArgumentException();
		if (definitions.put(variable, line, ctx) != null) throw new IllegalStateException();
	}
	
	public void addUse(String variable, int line, ParserRuleContext ctx) {
		Objects.requireNonNull(variable);
		Objects.requireNonNull(ctx);
		if (line <= 0) throw new IllegalArgumentException();
		uses.put(variable, line, ctx);
	}
	
	public String getGlobalVariableIntent(String variable) {
		Objects.requireNonNull(variable);
		if (definitions.containsRow(variable) && uses.containsRow(variable)) {
			return "inout";
		} else if (definitions.containsRow(variable)) {
			return "out";
		} else if (uses.containsRow(variable)) {
			return "in";
		} else {
			throw new IllegalStateException();
		}
	}
	
	public boolean isLocalVariableUnused(String variable) {
		Objects.requireNonNull(variable);
		if (!uses.containsRow(variable)) {
			return true;
		} else {
			return false;
		}
	}
	
	public boolean isGlobalVariableUnused(String variable) {
		Objects.requireNonNull(variable);
		if (!definitions.containsRow(variable) && !uses.containsRow(variable)) {
			return true;
		} else {
			return false;
		}
	}

	public Map<Integer, ParserRuleContext> getDefinitions(String variable) {
		return definitions.row(variable);
	}
	
	@Override
	public String toString() {
		return "DefAndUseMapper [definitions=" + definitions + ", uses=" + uses + "]";
	}
}
