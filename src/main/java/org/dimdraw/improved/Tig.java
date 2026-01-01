package org.dimdraw.improved;

import java.util.*;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.stream.IntStream;

public class Tig {

    public static boolean isCompatible(BitSet[] adjacency, int src1, int dest1, int src2, int dest2) {
        boolean cond1 = (dest2 == src1) || (adjacency[dest2] != null && adjacency[dest2].get(src1));
        if (!cond1) return true;

        boolean cond2 = (dest1 == src2) || (adjacency[dest1] != null && adjacency[dest1].get(src2));

        return !(cond1 && cond2);
    }

    public static Object[] computeTigEdges(BitSet[] adjacency, int n) {
        // 1. Find incomparable pairs
        List<int[]> pairsList = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i == j) continue;
                boolean hasEdge = (adjacency[i] != null && adjacency[i].get(j));
                boolean hasReverse = (adjacency[j] != null && adjacency[j].get(i));
                if (!hasEdge && !hasReverse) {
                    pairsList.add(new int[]{i, j});
                }
            }
        }

        int[][] pairs = pairsList.toArray(new int[0][]);
        int numPairs = pairs.length;

        // 2. Compute edges as pairs of indices
        ConcurrentLinkedQueue<int[]> edgesQueue = new ConcurrentLinkedQueue<>();

        IntStream.range(0, numPairs).parallel().forEach(i -> {
             for (int j = i + 1; j < numPairs; j++) {
                 int[] p1 = pairs[i];
                 int[] p2 = pairs[j];

                 if (!isCompatible(adjacency, p1[0], p1[1], p2[0], p2[1])) {
                     edgesQueue.add(new int[]{i, j});
                 }
             }
        });

        int[][] edges = edgesQueue.toArray(new int[0][]);

        return new Object[]{pairs, edges};
    }
}
