package com.woodplc.cora.data;

import java.util.Set;

public interface SDGraph {

	Set<SubProgram> getSubprograms();

	void add(FortranFileModel model);

}
