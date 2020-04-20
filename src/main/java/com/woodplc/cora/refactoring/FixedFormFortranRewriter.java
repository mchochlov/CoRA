package com.woodplc.cora.refactoring;

import java.util.List;

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
		StringBuilder output =  new StringBuilder();
		output.append('(');
		initialArgumentListOffset += 1;
		for (String arg : nonNullValues) {
			if (initialArgumentListOffset + arg.length() + 2 > ARGUMENT_OFFSET) {			
				output.append("\n").append(fixedOffset).append("& ").append(arg).append(", ");
				initialArgumentListOffset = 9 + arg.length();
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
		String tab = position > 6 ? " ".repeat(position) : fixedOffset + " ";
		return "\nc" + tab.substring(0, tab.length() - 1) + text + "\n" + tab;
	}

}
