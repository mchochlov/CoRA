package com.woodplc.cora.refactoring;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.woodplc.cora.data.OrderedCaseInsensitiveSet;

final class FreeFormFortranFormatter implements FortranFormatter {

	private static final int ARGUMENT_OFFSET = 80;
	private String typeTab = "\t";

	@Override
	public String formatParameterAllocationErrors(List<String> parameterAllocationErrors) {
		StringBuilder output =  new StringBuilder();
		for (String error : parameterAllocationErrors) {
			output.append("! ").append(error).append("\n").append(typeTab);
		}
		return output.toString();
	}

	@Override
	public String formatIntentStatements(List<String> existingIntentStatements) {
		StringBuilder output =  new StringBuilder();
		for (String string : existingIntentStatements) {
			output.append("\n").append(typeTab).append(string);
		}
		return output.toString();
	}

	@Override
	public String formatLocalVariableComment() {
		return new StringBuilder().append("\n").append(typeTab).append("!Local variables")
				.append("\n").append(typeTab).toString();
	}

	@Override
	public String formatArgumentList(List<String> nonNullValues, int initialArgumentListOffset) {
		StringBuilder output =  new StringBuilder();
		output.append('(');
		initialArgumentListOffset += 1;
		for (String arg : nonNullValues) {
			if (initialArgumentListOffset + arg.length() + 2 > ARGUMENT_OFFSET) {			
				output.append(" &\n\t& ").append(arg).append(", ");
				initialArgumentListOffset = 8 + arg.length();
			} else {
				output.append(arg).append(", ");	
				initialArgumentListOffset += arg.length() + 2;
			}	
		}
		if (output.length() > 2) output.delete(output.length() - 2, output.length());
		output.append(')');
		return output.toString();
	}

	@Override
	public void setTypeTab(String tab) {
		this.typeTab = tab;
	}

	@Override
	public String commentStatement(String text, int position) {
		String tab = " ".repeat(position);
		return "! " + text + "\n" + tab;
	}

	@Override
	public String formatUseStatements(Set<Entry<String, Collection<String>>> entrySet, int useStatementOffset) {
		String tab = " ".repeat(useStatementOffset);
		StringBuilder output =  new StringBuilder();
		for (Map.Entry<String, Collection<String>> entry : entrySet) {
			output.append("\n").append(tab).append("use ").append(entry.getKey()).append(", only : ")
			.append(formatVariableList(entry.getValue(), 13 + tab.length() + entry.getKey().length()));
		}
		return output.toString();
	}

	private String formatVariableList(Collection<String> list, int offset) {
		StringBuilder output =  new StringBuilder();
		for (String arg : list) {
			if (offset + arg.length() + 2 > ARGUMENT_OFFSET) {			
				output.append(" &\n\t& ").append(arg).append(", ");
				offset = 8 + arg.length();
			} else {
				output.append(arg).append(", ");	
				offset += arg.length() + 2;
			}	
		}
		if (output.length() > 2) output.delete(output.length() - 2, output.length());
		return output.toString();
	}

	@Override
	public String formatCommonStatements(Map<String, OrderedCaseInsensitiveSet<String>> savedCommonStatements,
			Map<String, String> savedTypeStatements, int commonStatementOffset) {
		String tab = " ".repeat(commonStatementOffset);
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
