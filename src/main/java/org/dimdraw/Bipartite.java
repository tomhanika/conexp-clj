package org.dimdraw;

import java.util.*; 
import java.lang.*; 
import java.io.*; 

public class Bipartite {
    public static boolean isInOddCycle(Map<Object,Set> edges, Set subset, Object src) {
        //System.out.println(graph.toString());
        //System.out.println(subset.toString());
        //        System.out.println(o);
        //        subset.add(src);

        //        System.out.println(edges);
        //subset.add(src);
        HashMap<Object,Integer> colorArr = new HashMap<Object, Integer>(); 
        colorArr.put(src, 1);
  
        // Create a queue (FIFO) of vertex numbers  
        // and enqueue source vertex for BFS traversal 
        LinkedList<Object>q = new LinkedList<Object>(); 
        q.add(src); 
  
        // Run while there are vertices in queue (Similar to BFS) 
        while (q.size() != 0) 
        { 
            // Dequeue a vertex from queue 
            Object u = q.poll(); 
  
            // Find all non-colored adjacent vertices 
            for (Object v : edges.get(u)) 
            {
                if(subset.contains(v)){
                // An edge from u to v exists  
                // and destination v is not colored 
                if (!colorArr.containsKey(v)) 
                { 
                    // Assign alternate color to this adjacent v of u 
                    colorArr.put(v,1-colorArr.get(u)); 
                    q.add(v); 
                }  
                else if (colorArr.get(v)==colorArr.get(u)) 
                    return true;
                }
            } 
        } 
        // If we reach here, then all adjacent vertices can 
        // be colored with alternate color 
        return false; 
    }
}
