package com.woodplc.cora.data;

import java.util.Objects;
import java.util.Set;

public final class FeatureView {
	
	private final Set<SubProgram> subprogramsSystemA;
	private final Set<SubProgram> subprogramsSystemB;
	private final Set<SubProgram> subprogramsSystemC;
	
	public FeatureView(Set<SubProgram> subprogramsSystemA,
			Set<SubProgram> subprogramsSystemB,
			Set<SubProgram> subprogramsSystemC) 
	{
		this.subprogramsSystemA = Objects.requireNonNull(subprogramsSystemA);
		this.subprogramsSystemB = Objects.requireNonNull(subprogramsSystemB);
		this.subprogramsSystemC = Objects.requireNonNull(subprogramsSystemC);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof FeatureView)) return false;
		FeatureView fv = (FeatureView) o;
		return this.subprogramsSystemA.equals(fv.subprogramsSystemA)
				&& this.subprogramsSystemB.equals(fv.subprogramsSystemB)
				&& this.subprogramsSystemC.equals(fv.subprogramsSystemC);
	}

	@Override
	public int hashCode() {
		return Objects.hash(subprogramsSystemA, subprogramsSystemB, subprogramsSystemC);
	}

}
