package org.latdraw.orderedset;


import java.util.List;

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
  private List<POElem> upperCovers;
  private List<POElem> lowerCovers;
  private List<POElem> ideal;
  private List<POElem> filter;
  // Elements of poset incomparable with `this' and
  // which come after it in the linear order of poset.
  private List<POElem> highIncomparables;
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

  @Override
  public String toString() {
    return underlyingObject.toString();
  }

  public List<POElem> upperCovers() {
    return upperCovers;
  }

  public void setUpperCovers(List<POElem> v) { upperCovers = v; }

  public List<POElem> lowerCovers() {
    return lowerCovers;
  }

  public void setLowerCovers(List<POElem> v) { lowerCovers = v; }

  public double xCoord() {
    return x;
  }

  public double yCoord() {
    return y;
  }

  public OrderedSet orderedSet() {
    return poset;
  }

  public List<POElem> filter() {
    return filter;
  }

  public void setFilter(List<POElem> v) { filter = v; }

  public List<POElem> ideal() {
    return ideal;
  }

  /**
   * The index in the linear extension.
   */
  public int index() {
    return poset.elemOrder(this);
  }

  public void setIdeal(List<POElem> v) { ideal = v; }

  public List<POElem> highIncomparables() {
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

  public void setHighIncomparables(List<POElem> v) { highIncomparables = v; }

  public int rank() {
    return rank;
  }

  public void setRank(int v) { rank = v; }
  
  @Override
  public int compareTo(POElem elt) {
    return rank() - elt.rank();
  }

}

