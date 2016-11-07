package org.latdraw.orderedset;


import org.latdraw.util.SimpleList;
import java.util.*;

/**
 * A class to hold an edge. Used for the hashtable of edge colors.
 */
public final class Edge {

  Object pt0;
  Object pt1;

  public Edge(Object pt0, Object pt1) {
    this.pt0 = pt0;
    this.pt1 = pt1;
  }

  public boolean equals(Object obj) {
    if (!(obj instanceof Edge)) return false;
    Edge e = (Edge)obj;
    return (this.pt0.equals(e.pt0) && this.pt1.equals(e.pt1));
  }

  public int hashCode() {
    return pt0.hashCode() + 31 * pt1.hashCode();
  }


}

