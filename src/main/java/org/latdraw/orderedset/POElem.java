package org.latdraw.orderedset;


import org.latdraw.util.SimpleList;
import java.util.*;

/**
 * this class represents elements of an ordered set.
 *
 * @version $Id: POElem.java,v 1.4 2010/03/29 04:00:39 ralph Exp $
 */
public class POElem implements Comparable<POElem> {

  private OrderedSet poset;
  private Object underlyingObject; // often just a string
  private double x, y;		// x and y coords for the diagram
  // All of these should be linked lists
  private List upperCovers;
  private List lowerCovers;
  private List ideal;
  private List filter;
  // Elements of poset incomparable with `this' and
  // which come after it in the linear order of poset.
  private List highIncomparables; 
  private int rank;

  /**
   * Construct an element based on an arbitrary object. Using an 
   * arbitrary object essentially as a label rather than a String 
   * allows the diagram to easily interface with other programs.
   */
  public POElem(Object underlyingObject, OrderedSet poset) {
    this.underlyingObject = underlyingObject;
    this.poset = poset;
  }

  public Object getUnderlyingObject() { return underlyingObject; }

  public String label() {
    return underlyingObject.toString();
  }

  public String toString() {
    return underlyingObject.toString();
  }

  public List upperCovers() {
    return upperCovers;
  }

  public void setUpperCovers(List v) { upperCovers = v; }

  public List lowerCovers() {
    return lowerCovers;
  }

  public void setLowerCovers(List v) { lowerCovers = v; }

  public double xCoord() {
    return x;
  }

  public double yCoord() {
    return y;
  }

  public OrderedSet orderedSet() {
    return poset;
  }

  public List filter() {
    return filter;
  }

  public void setFilter(List v) { filter = v; }

  public List ideal() {
    return ideal;
  }

  /**
   * The index in the linear extension.
   */
  public int index() {
    return poset.elemOrder(this);
  }

  public void setIdeal(List v) { ideal = v; }

  public List highIncomparables() {
    return highIncomparables;
  }

  /**
   * Returns true iff it has exactly one lower cover.
   * So this is really a test for complete join irreducibility.
   */
  public boolean isJoinIrreducible() {
    return lowerCovers().size() == 1;
  }

  /**
   * Returns true iff it has exactly one upper cover.
   * So this is really a test for complete meet irreducibility.
   */
  public boolean isMeetIrreducible() {
    return upperCovers().size() == 1;
  }

  public void setHighIncomparables(List v) { highIncomparables = v; }

  public int rank() {
    return rank;
  }

  public void setRank(int v) { rank = v; }
  
  public int compareTo(POElem elt) {
    return rank() - elt.rank();
  }

}

