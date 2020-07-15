package com.woodplc.cora.refactoring;

import java.util.Collection;
import java.util.List;

public interface Refactoring {

	List<String> refactor();

	Collection<? extends String> getErrors();

}
