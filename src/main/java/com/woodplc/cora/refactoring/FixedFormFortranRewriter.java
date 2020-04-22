package com.woodplc.cora.refactoring;

import static com.google.common.io.Files.append;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.woodplc.cora.data.OrderedCaseInsensitiveSet;

final class FixedFormFortranFormatter implements FortranFormatter {
	
	private static final String fixedOffset = "     ";
	private static final int ARGUMENT_OFFSET = 72;
	private String typeTab;

	@Override
	public String formatParameterAllocationErrors(List<String> parameterAllocationErrors) {
		StringBuilder output =  new StringBuilder();
		for (String error : parameterAllocationErrors) {
			output.append("C ").append(error).append("\n").append(fixedOffset);
		}
		return output.toString();
	}

	@Override
	public String formatIntentStatements(List<String> existingIntentStatements) {
		StringBuilder output =  new StringBuilder();
		for (String string : existingIntentStatements) {
			output.append("\n").append(fixedOffset + " ").append(string);
		}
		return output.toString();
	}

	@Override
	public String formatLocalVariableComment() {
		return new StringBuilder().append("\n").append("C ").append("Local variables")
				.append("\n").append(fixedOffset + " ")
				.toString();
	}

	@Override
	public String formatArgumentList(List<String> nonNullValues, int initialArgumentListOffset) {
		return new StringBuilder()
				.append('(')
				.append(formatVariableList(nonNullValues, initialArgumentListOffset + 1))
				.append(')')
				.toString();
	}

	@Override
	public void setTypeTab(String tab) {
		this.typeTab = tab;
	}

	@Override
	public String commentStatement(String text, int position) {
		String tab = getTab(position);
		return "\nc" + tab.substring(0, tab.length() - 1) + text + "\n" + tab;
	}

	@Override
	public String formatUseStatements(Set<Entry<String, Collection<String>>> entrySet, int useStatementOffset) {
		String tab = getTab(useStatementOffset);
		StringBuilder output =  new StringBuilder();
		for (Map.Entry<String, Collection<String>> entry : entrySet) {
			output.append("\n").append(tab).append("use ").append(entry.getKey()).append(", only : ")
			.append(formatVariableList(entry.getValue(), 13 + tab.length() + entry.getKey().length()));
		}
		return output.toString();
	}
	
	private String formatVariableList(Collection<String> list, int offset) {
		StringBuilder output = new StringBuilder();
		for (String arg : list) {
			if (offset + arg.length() + 2 > ARGUMENT_OFFSET) {			
				output.append("\n").append(fixedOffset).append("& ").append(arg).append(", ");
				offset = 9 + arg.length();
			} else {
				output.append(arg).append(", ");	
				offset += arg.length() + 2;
			}	
		}
		if (output.length() > 2) output.delete(output.length() - 2, output.length());
		return output.toString();
	}

	private String getTab(int offset) {
		return offset > 6 ? " ".repeat(offset) : fixedOffset + " ";		
	}

	@Override
	public String formatCommonStatements(Map<String, OrderedCaseInsensitiveSet<String>> savedCommonStatements,
			Map<String, String> savedTypeStatements, int commonStatementOffset) {
		String tab = getTab(commonStatementOffset);
		StringBuilder output =  new StringBuilder();
		for (Map.Entry<String, OrderedCaseInsensitiveSet<String>> entry : savedCommonStatements.entrySet()) {
			output.append("\n").append(tab).append("common /").append(entry.getKey()).append("/ ")
			.append(formatVariableList(entry.getValue().allValues(), 10 + tab.length() + entry.getKey().length()));
			for (String var : entry.getValue().allValues()) {
				if (!savedTypeStatements.containsKey(var)) {
					throw new IllegalStateException();
				}
				output.append("\n").append(tab).append(savedTypeStatements.get(var));
			}
		}
		if (output.length() > 0) {
			output.append("\n").append(tab);
		}
		return output.toString();
	}
}
