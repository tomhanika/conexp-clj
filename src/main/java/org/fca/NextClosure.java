package org.fca;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

public class NextClosure {

    /**
     * Represents a single implication A -> B.
     * We use BitSets for efficient set operations.
     */
    public static class Implication {
        public final BitSet premise;
        public final BitSet conclusion;

        public Implication(BitSet premise, BitSet conclusion) {
            this.premise = premise;
            this.conclusion = conclusion;
        }
    }

    /**
     * Computes the closure of a set under a set of implications.
     * Naive iterative implementation (LinClosure is faster for large sets but this is simpler for now).
     * Repeat until stable:
     *   For each impl A->B:
     *     If A is subset of current set, add B.
     */
    public static BitSet computeClosure(BitSet input, List<Implication> implications) {
        // Clone input to avoid modifying the original
        BitSet closure = (BitSet) input.clone();
        boolean changed = true;
        while (changed) {
            changed = false;
            for (Implication impl : implications) {
                // If premise is subset of closure
                boolean isSubset = true;
                for (int i = impl.premise.nextSetBit(0); i >= 0; i = impl.premise.nextSetBit(i+1)) {
                    if (!closure.get(i)) {
                        isSubset = false;
                        break;
                    }
                }

                if (isSubset) {
                    for (int i = impl.conclusion.nextSetBit(0); i >= 0; i = impl.conclusion.nextSetBit(i+1)) {
                        if (!closure.get(i)) {
                            closure.set(i);
                            changed = true;
                        }
                    }
                }
            }
        }
        return closure;
    }

    /**
     * Compute next closed set using internal closure logic.
     * @param n Universe size
     * @param implications List of Java Implication objects
     * @param currentSet The current set (BitSet)
     * @return Next closed set (BitSet)
     */
    public static BitSet nextClosedSet(int n, List<Implication> implications, BitSet currentSet) {
        BitSet workingSet = (BitSet) currentSet.clone();

        for (int i = n - 1; i >= 0; i--) {
            if (workingSet.get(i)) {
                workingSet.clear(i);
            } else {
                workingSet.set(i);

                BitSet closure = computeClosure(workingSet, implications);

                boolean lectic = true;
                for (int bit = closure.nextSetBit(0); bit >= 0 && bit < i; bit = closure.nextSetBit(bit+1)) {
                    if (!workingSet.get(bit)) {
                        lectic = false;
                        break;
                    }
                }

                if (lectic) {
                    return closure;
                }

                workingSet.clear(i);
            }
        }
        return null;
    }
}
