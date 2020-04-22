package com.woodplc.cora.refactoring;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.woodplc.cora.data.OrderedCaseInsensitiveSet;

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

	String formatUseStatements(Set<Entry<String, Collection<String>>> entrySet, int useStatementOffset);

	String formatCommonStatements(Map<String, OrderedCaseInsensitiveSet<String>> savedCommonStatements,
			Map<String, String> savedTypeStatements, int commonStatementOffset);

}
