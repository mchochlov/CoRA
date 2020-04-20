package com.woodplc.cora.refactoring;

import java.util.List;

interface FortranFormatter {

	static FortranFormatter ofFixedForm() {
		return new FixedFormFortranFormatter();
	}

	static FortranFormatter ofFreeForm() {
		return new FreeFormFortranFormatter();
	}

	String formatParameterAllocationErrors(List<String> parameterAllocationErrors);

	String formatIntentStatements(List<String> existingIntentStatements);

	String formatLocalVariableComment();

	String formatArgumentList(List<String> nonNullValues, int initialArgumentListOffset);

	void setTypeTab(String tab);

	String commentStatement(String text, int position);

}
