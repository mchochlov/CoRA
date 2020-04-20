package com.woodplc.cora.refactoring;

import java.util.List;

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

}
