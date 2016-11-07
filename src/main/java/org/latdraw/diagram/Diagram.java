package org.latdraw.diagram;

/* Diagram.java  96/30/6


*/


import java.util.*;
import java.io.*;
import org.latdraw.orderedset.*;
import org.latdraw.util.*;

// just for testing
import org.latdraw.beans.*;
import javax.swing.*;
//import java.awt.*;
import java.awt.Dimension;
import java.awt.Toolkit;


/**
 * a class to represent diagrams of ordered sets.
 *
 *
 * @version $Id: Diagram.java,v 1.25 2008/08/04 18:56:26 ralph Exp $
 */
public class Diagram {

  final static int[] PRIMES = {3, 5, 7, 11, 13, 17, 19, 23, 29, 31};
  final static double ATTRACTION_CONSTANT = 0.1;
  final static double REPULSION_CONSTANT = 1.0;
  public static final int ITERATIONS = 30;

  double attraction_I, attraction_II, attraction_III;
  double repulsion_I, repulsion_II, repulsion_III;
  int repititions_I, repititions_II, repititions_III;


  OrderedSet poset;
  Vertex[] vertices;
  int size;
  HashMap edgeColors;
  int primePointer;
  //static double REPULSION_CONSTANT = 0.8;
  double attractionFactor;
  double repulsionFactor;
  //static int ITERATIONS = 40;

  private boolean paintLabels;

  double rotationAngle = 0.0;

  /**
   * When normalized, this will be false if the z coordinate of the top 1.
   */
  boolean horizontal;

  /**
   * The normalized coordinates are obtained by dividing by this.
   */
  double scaleFactor;

  public double getScaleFactor() { return scaleFactor; }
  public void setScaleFactor(double v) { scaleFactor = v; }

  public double getRotationAngle() { return rotationAngle; }
  public void setRotationAngle(double angle) { rotationAngle = angle; }

  public double getAttractionFactor() { return attractionFactor; }
  public double getRepulsionFactor() { return repulsionFactor; }

  /**
   * This globally controls if the labels are displayed. If it is false
   * then they are not displayed even for vertices with labelPainted
   * true.
   */
  public boolean isPaintLabels() { return paintLabels; }

  /**
   * This globally controls if the labels are displayed. If it is false
   * then they are not displayed even for vertices with labelPainted
   * true.
   */
  public void setPaintLabels(boolean v) { paintLabels = v; }

  // was this diagram constructed as an interval of another diagram.
  private boolean interval;

  private int improvementCount = 0;

  public void setImprovementCount(int v) { improvementCount = v; }

  public int getImprovementCount() { return improvementCount; }

  /**
   * Was this diagram constructed as an interval of another diagram.
   */
  public boolean isInterval() { return interval; }

  public void setInterval(boolean v) { interval = v; }

  private String name;

  public String getName() { return name; }

  public void setName(String s) { name = s; }

  // Constructors (and methods to help)

  /**
   * Construct a Diagram with name <tt>n</tt> from a List of Objects and
   * a list of covers for each object. An ordered set is constructed
   * from the name, labels and ucs.
   *
   *
   *
   * @param n       the name.
   * @param labels  the list of Objects representing the elements
   *                of the ordered set. Note any Object is allowed.
   * @param ucs     a list of lists so the <t>k</i><sup>th</sup> list
   *                is the list of covers of the <t>k</i><sup>th</sup>
   *                element of <tt>labels</tt>. It valid to include
   *                elements that are not upper covers as long as they are
   *                greater than the element.
   *
   */
  public Diagram(String n, List labels, List ucs) 
                                      throws NonOrderedSetException {
    this(n, labels, ucs, null);
  }

  public Diagram(String n, List labels, List ucs, HashMap edgeColors) 
                                              throws NonOrderedSetException {
    poset = new OrderedSet(n, labels, ucs);
    setupDiagram();
    this.edgeColors = edgeColors;
  }

  public Diagram(InputLattice in) throws NonOrderedSetException {
    this(in.name, in.labels, in.upperCoversList);
    if (in.edgeColors != null && in.edgeColors.size() > 0) {
      edgeColors = in.edgeColors;
    }
  }

  public Diagram(OrderedSet set) throws NonOrderedSetException {
    poset = set;
    setupDiagram();
  }

  void setupDiagram() {
    // AS: adding <name>
    name = poset.getName();

    primePointer = -1;
    size = poset.card();
    attractionFactor = ATTRACTION_CONSTANT / Math.sqrt(size);
    repulsionFactor = REPULSION_CONSTANT / Math.sqrt(size);
    attraction_I = 0.5 * attractionFactor;
    attraction_II = 3.0 * attractionFactor;  // was 3.0
    attraction_III = 0.75 * attractionFactor;  // was 1.0
    repulsion_I = 3.0 * repulsionFactor;
    repulsion_II = 0.5 * repulsionFactor;  // was 0.5
    repulsion_III = 1.5 * repulsionFactor;  // was 1.0
    repititions_I = ITERATIONS + size + 30;
    repititions_II = ITERATIONS + size;
    repititions_III = ITERATIONS + size + 20;

    vertices = new Vertex[size];
    List elems = poset.univ();
    for(int i=0; i < size; i++) {
      vertices[i] = new Vertex((POElem)elems.get(i));
    }
    Vertex pt;
    int rank;
    for(int i=0; i < size; ) {
      
      pt = vertices[i];
      rank = ((POElem)elems.get(i)).rank();
      // j will be the nunber of vertices with the same rank as the ith one.
      int j;
      for(j=1; j < size - i; j++) {
        if (rank != ((POElem)elems.get(i+j)).rank()) break;
      }
      double angle = 2*Math.PI/j;
      for (int k=0; k < j; k++) {
        //int prime = nextPrime();
        pt = vertices[i+k];
        pt.z = rank;
        pt.x = j*Math.cos(k*angle + Math.PI/nextPrime());
        pt.y = j*Math.sin(k*angle + Math.PI/nextPrime());
      }
      i += j;
    }
  }
      
  /**
   * Silently improve the diagram through all three stages.
   */
  public void improve() {
    multipleUpdates(repititions_I, attraction_I, repulsion_I);
    multipleUpdates(repititions_II, attraction_II, repulsion_II);
    multipleUpdates(repititions_III, attraction_III, repulsion_III);
    improvementCount += repititions_I + repititions_II + repititions_III;
    normalizeCoords();
  }

  /**
   * Form a new Diagram representing the interval from botton to top.
   * If bottom is null then use 0 (or the first element in the topological
   * sort and similar for top.
   */
  public Diagram interval(Object top, Object bottom) 
                                          throws NonOrderedSetException {
    POElem topE = null;
    if (top != null) topE = poset.getElement(top);
    POElem bottomE = null;
    if (bottom != null) bottomE = poset.getElement(bottom);
    if (topE == null) topE = poset.one();
    if (bottomE == null) bottomE = poset.zero();
    if (!poset.leq(bottomE, topE)) {
      throw new NonOrderedSetException(NonOrderedSetException.EMPTY_ERROR);
    }
    List filter = bottomE.filter();
    List elems = new ArrayList();
    List ucs = new ArrayList();
    for (Iterator it = filter.iterator(); it.hasNext(); ) {
      POElem x = (POElem)it.next();
      if (poset.leq(x, topE)) {
        elems.add(x.label());
        List upperCovs = x.upperCovers();
        List covers = new ArrayList();
        for (Iterator it2 = upperCovs.iterator(); it.hasNext(); ) {
          POElem y = (POElem)it2.next();
          if (poset.leq(y, topE)) covers.add(y.label());
        }
        ucs.add(covers);
      }
    }
    String intName;
    if (name != null) {
      intName = name + "[" + bottom + "," + top + "]";
    }
    else {
      intName = "[" + bottom + "," + top + "]";
    }
    return new Diagram(intName, elems, ucs, edgeColors);
  }

  public OrderedSet getOrderedSet() {
    return poset;
  }

  public int size() {
    return size;
  }

  public Vertex[] getVertices() { return vertices; }

  /**
   * Find the Vertex whose associated POElem is <tt>elt</tt>.
   *
   *
   */
  public Vertex vertexForPOElem(POElem elt) {
    Object o = elt.getUnderlyingObject();
    for (int i = 0; i < vertices.length; i++) {
      // there actually is no equals method presently (except ==)
      // in fact it failed on going to POElem level; not sure why
      if (vertices[i].getUnderlyingObject().equals(o)) return vertices[i];
    }
    return null;
  }

  /**
   * Sets the Color of all vertices to null and filled to false.
   */
  public void resetVertices() {
    for (int i = 0; i < vertices.length; i++) {
      vertices[i].reset();
    }
  }

  public HashMap getEdgeColors() { return edgeColors; }

  public void setEdgeColors(HashMap ht) { edgeColors = ht; }

  public boolean isHorizontal() { return horizontal; }

  int nextPrime() {
    primePointer++;
    if (primePointer == PRIMES.length) primePointer = 0;
    return PRIMES[primePointer];
  }

// /////////  MOVE BOTH OF THESE TO Vertex ///////////////////////
  /**
   * This finds the attraction between two points 
   * and updates their currentForce.
   */
  void attraction(Vertex pt1, Vertex pt2, double att_fac) {
    double dx = att_fac * (pt2.x - pt1.x); 
    double dy = att_fac * (pt2.y - pt1.y); 
    pt1.adjustForce(dx, dy);
    pt2.adjustForce(-dx, -dy);
  }

  /**
   * This finds the repulsion between two points 
   * and updates their currentForce.
   */
  void repulsion(Vertex pt1,  Vertex pt2, double repulsion_fac) {
    double dx = pt1.x - pt2.x;
    double dy = pt1.y - pt2.y;
    double   dz = pt1.z - pt2.z;
    double inv_d_cubed;
    if (dz==0 && -0.2<dx && dx<0.2 && -0.2<dy && dy<0.2) {
      inv_d_cubed = 37.0;
    } else {
      inv_d_cubed = 1.0 / (Math.pow(Math.abs(dx),3) + 
          Math.pow(Math.abs(dy),3) + 
          Math.pow(Math.abs(dz),3));
    }
    dx *= inv_d_cubed * repulsion_fac;
    dy *= inv_d_cubed * repulsion_fac;
    pt1.adjustForce(dx, dy);
    pt2.adjustForce(-dx, -dy);
  }

  public void multipleUpdates(int k, double att, double repulsion) {
    for(int i=0;i<k;i++) update(att, repulsion);
  }

  /**
   * This does a single update using <tt>att</tt> and <tt>repulsion</tt>,
   * improving the diagram.
   */
  public synchronized void update(double att, double repulsion) {
    improvementCount++;
    Iterator list;
    for (int i = 0; i < size - 1; i++) {
      POElem x = (POElem)poset.univ().get(i);
      list = x.filter().iterator();
      list.next();    // Skip the first element which is x.
      while (list.hasNext()) {
        attraction(vertices[i], 
                   vertices[poset.elemOrder((POElem)list.next())], att);
      }
      list = x.highIncomparables().iterator();
      while (list.hasNext()) {
        repulsion(vertices[i], 
                  vertices[poset.elemOrder((POElem)list.next())], repulsion);
      }
    }
    for (int i = 0; i < size; i++) {
      vertices[i].update();
    }
  }

  /**
  * This does a translation in the x-y plane so that 0 is at the origin.
  * It returns a scale factor which can be used to get the coords in [0,1].
  */
  private double center() {
    Vertex p = vertices[0];
    double x0 = p.x;
    double y0 = p.y;
    double distSq;
    double maxSq = 0.0;
    for (int i=0; i < size; i++) {
      p = vertices[i];
      p.x -= x0;
      p.y -= y0;
      distSq = p.x * p.x + p.y * p.y;
      if (distSq > maxSq) maxSq = distSq;
    }
    double maxZ = vertices[size-1].z;
    if (maxZ * maxZ > 4 * maxSq) {
      horizontal = false;
      scaleFactor = maxZ;
    }
    else {
      horizontal = true;
      scaleFactor = 2 * Math.sqrt(maxSq);
    }
    return scaleFactor;
  }


  /**
   * 
   *
   */
  public void normalizeCoords() {
    double scale = center();
    if (scale != 0) {
      for (int i=0; i < size; i++) vertices[i].setNormalizedCoords(scale);
    }
  }

  public synchronized void project2d(double angle) {
    //double half = 0.5;
    final double cos = Math.cos(angle);
    final double sin = Math.sin(angle);
    Vertex p;
    for (int i = 0; i < size; i++) {
      p = vertices[i];
      p.project2d(cos, sin);
      //p.getProjection().x += half;
    }
  }

  public void writeRSFDiagram(File f) throws IOException {
    PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(f)));
    try {
      final String sp = " ";
      final String open = "[";
      final String close = "]";
      final Vertex[] vertices = getVertices();
      final int r = vertices[vertices.length - 1].getUnderlyingElem().rank();
      final int scaling = 15; // make this an argument?
      final int maxY = scaling * r;
      out.println("%!PS-Adobe-3.0 EPSF-3.0");
      out.println("%%Creator: LatDraw (Ralph Freese)");
      out.println("%%CreationDate: " + new Date());
      out.println("%%BoundingBox: -1 -1 1 " + maxY);
      out.println("");
      out.println("/scaling " + scaling + " def");
      out.println("0.5 setlinewidth\n");
      out.println(open);
      for (int i = 0; i < vertices.length; i++) {
        final OrderedSet poset = getOrderedSet();
        Vertex v = vertices[i];
        out.print(sp + sp + open + open + format(v.getProjectedX(), r) + sp 
                         + format(v.getProjectedY(), r) + close + sp + open);
        for (Iterator covs = v.getUnderlyingElem().upperCovers().iterator(); 
                                                           covs.hasNext(); ) {
          Vertex v2 = vertices[poset.elemOrder((POElem)covs.next())];
          out.print(open + format(v2.getProjectedX(), r) 
                           + sp + format(v2.getProjectedY(), r) + close + sp);
        }
        out.println(close + close);
      }
      out.println("] drawpicture");
    }
    finally {
      if (out != null) out.close();
    }
  }

  private double format(double d, int scale) {
    return Math.round(100 * d * scale) / 100.0;
  }

  /**
   * Hide the all vertex labels.
   */
  public void hideLabels() {
    for (int i = 0; i < vertices.length; i++) {
      vertices[i].setLabelPainted(false);
    }
  }

  /**
   * Set all vertex labels to be shown but if paintLabels is false they 
   * still will not be painted.
   */
  public void showLabels() {
    for (int i = 0; i < vertices.length; i++) {
      vertices[i].setLabelPainted(true);
    }
  }

  /**
   * Sets all the labels to the empty string which prevents them
   * from being drawn.
   */
  public void clearLabels() {
    final String es = "";
    for (int i = 0; i < vertices.length; i++) {
      vertices[i].setLabel(es);
    }
  }

  public boolean leq(Vertex v0, Vertex v1) {
    return getOrderedSet().leq(v0.getUnderlyingElem(), 
                               v1.getUnderlyingElem());
  }

  public boolean lt(Vertex v0, Vertex v1) {
    return getOrderedSet().lt(v0.getUnderlyingElem(),
                               v1.getUnderlyingElem());
  }

  public boolean geq(Vertex v0, Vertex v1) {
    return getOrderedSet().geq(v0.getUnderlyingElem(),
                               v1.getUnderlyingElem());
  }

  public boolean gt(Vertex v0, Vertex v1) {
    return getOrderedSet().gt(v0.getUnderlyingElem(),
                               v1.getUnderlyingElem());
  }

  /**
   * A convenience method to get the vertices in the filter.
   */
  public List<Vertex> filter(Vertex v) {
    List<Vertex> ans = new ArrayList<Vertex>();
    ans.add(v);
    final Vertex[] verts = getVertices();
    for (int i = v.index() + 1 ; i < verts.length; i++) {
      if (leq(v, verts[i])) ans.add(verts[i]);
    }
    return ans;
  }

  /**
   * A convenience method to get the vertices in the ideal.
   */
  public List<Vertex> ideal(Vertex v) {
    List<Vertex> ans = new ArrayList<Vertex>();
    ans.add(v);
    final Vertex[] verts = getVertices();
    for (int i = 0; i < v.index() ;  i++) {
      if (leq(verts[i], v)) ans.add(verts[i]);
    }
    return ans;
  }

  public static void main(String[] args) 
          throws FileNotFoundException, IOException,NonOrderedSetException {
    OrderedSet test;
    if (args.length == 0) {
      //System.out.println("I need the name of a lattice file");
      //System.exit(1);
      // String str = "( (0 (b c)) (a (1)) (b (1)) (c (a)) (1 ()))"; // test = new OrderedSet(new InputLattice(str,false));
      // a test lattice
      String n = "Andrew";
      List l = new ArrayList();
      l.add("0"); 
      l.add("a");
      l.add("b");
      l.add("c");
      l.add("topelt bigname");

      List c = new ArrayList();
      List tmp;

      // covers of 0
      tmp = new ArrayList();
      tmp.add("b");
      tmp.add("c");
      c.add(tmp);

      // covers of a
      tmp = new ArrayList();
      tmp.add("topelt bigname");
      c.add(tmp);

      // covers of b
      tmp = new ArrayList();
      tmp.add("topelt bigname");
      c.add(tmp);


      // covers of c
      tmp = new ArrayList();
      tmp.add("a");
      c.add(tmp);

      // covers of 1
      tmp = new ArrayList();
      c.add(tmp);

      InputLattice i = new InputLattice(n, l, c);
      test = new OrderedSet(i);
    } 
    else {
      test = new OrderedSet(new InputLattice(args[0]));
    }
    ChainDecomposition chainDec = new ChainDecomposition(test);
    for (Iterator it = test.univ().iterator(); it.hasNext(); ) {
      System.out.print(" " + ((POElem)it.next()).label());
    }
    Diagram testDiagram = new Diagram(test);
    System.out.println("AS:Test diagram's name:"+testDiagram.getName()+"\n");
    System.out.println("\nveritces: ");
    int size = test.card();
    for(int i = 0; i < size; i++) {
      System.out.println("  " + (Vertex)testDiagram.vertices[i]);
    }
    System.out.println("att and rep fac are " + testDiagram.attractionFactor + 
      " " +  testDiagram.repulsionFactor);

/*
    for (int i = 0; i < testDiagram.size + ITERATIONS; i++) {
      testDiagram.update(0.5 * testDiagram.attractionFactor, 
      3.0 * testDiagram.repulsionFactor);
      //System.out.println("\nveritces after " + i + ": ");
      //for (int j = 0; j < size; j++) {
      //System.out.println("  " + (Vertex)testDiagram.vertices[j]);
      //}
    }
    for (int i = 0; i < testDiagram.size + ITERATIONS; i++) {
      testDiagram.update(3.0 * testDiagram.attractionFactor, 
      0.5 * testDiagram.repulsionFactor);
      //System.out.println("\nveritces after " + i + ": ");
      //System.out.println("  " + (Vertex)testDiagram.vertices[0]);
    }

    for (int i = 0; i < testDiagram.size + 4*ITERATIONS; i++) {
      testDiagram.update(testDiagram.attractionFactor, 
      testDiagram.repulsionFactor);
      //System.out.println("\nveritces after " + i + ": ");
      //System.out.println("  " + (Vertex)testDiagram.vertices[0]);
      //for(int k = 0; k < size; k++) {
      //System.out.println("  " + (Vertex)testDiagram.vertices[k]);
      //}
    }
    testDiagram.normalizeCoords();
*/
    testDiagram.improve();
    testDiagram.project2d(0);

    for(int k = 0; k < size; k++) {
      System.out.println("  " + (Vertex)testDiagram.vertices[k]);
    }  

    JFrame frame = new JFrame("testing");
    DrawPanel dp = new DrawPanel(testDiagram);
    frame.setContentPane(dp);
    dp.startRotation();
    //frame.pack();
    //frame.validate();
    //frame.show();

    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    int width = (screenSize.width * 8) / 10;
    int height = (screenSize.height * 8) / 10;
    frame.setLocation((screenSize.width - width) / 2,
                      (screenSize.height - height) / 2);
    frame.setSize(width, height);
    frame.setVisible(true);
    frame.isDefaultLookAndFeelDecorated();

  }
}

