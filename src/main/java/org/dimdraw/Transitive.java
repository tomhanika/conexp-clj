package org.dimdraw;

import java.util.*; 
import java.lang.*; 
import java.io.*; 

public class Transitive {
    public static boolean[][] hull(boolean[][] graph) {
        for (int k = 0; k < graph.length; k++) 
        { 
            // Pick all vertices as source one by one 
            for (int i = 0; i < graph.length; i++) 
            {
                if(graph[i][k]){
                    for (int j = 0; j < graph.length; j++){
                        if(graph[k][j]){
                            graph[i][j]=true;
                        }
                    }
                }
            } 
        }
        return graph;
    }
}
