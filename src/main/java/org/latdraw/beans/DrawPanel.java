/* DrawPanel.java (c) Ralph Freese 2003/07/06 */

package org.latdraw.beans;

import org.latdraw.orderedset.*;
import org.latdraw.diagram.*;
import org.latdraw.util.*;
import java.io.*;
import java.util.*;
//import java.net.URL;
import java.awt.*;
import java.awt.print.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.awt.font.*;
import javax.swing.*;
import javax.swing.event.*;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

//import java.awt.print.*;


public class DrawPanel extends JPanel 
                  implements MouseListener, MouseMotionListener, Printable {

  // public constant for outside property change listeners
  public static final String ROTATION = "rotation";
  public static final String STARTED = "started";
  public static final String STOPPED = "stopped";

  private Set<Vertex> allowedVertices = null;

  /**
   * We use this to let outside listeners know about certain events to
   * avoid some problems with the old stye java events used here.
   */
  ChangeSupport changeSupport = new ChangeSupport(this);
  
  HashMap highlightedEdges = new HashMap();

  // This is a map from the vertex labels to Colors (if defined at all)
  HashMap filledVertices = new HashMap();

  // This is a map from the vertex labels to a List whose
  // first component is a Color and second is a Boolean indicating if
  // the stoke should be thick. If a vertex is not on this table it
  // is just drawn in the usual black.
  HashMap drawnVertices = new HashMap();

  private final View view = new View();

  static int reps = Diagram.ITERATIONS;

  Diagram diagram; 
  OrderedSet poset;
  int size;
  int pointRadius = 5;
  int pointDiameter = 2 * pointRadius;
  int highlightPointRadius = 6;
  int highlightPointDiameter = 2 * highlightPointRadius;
  int phase1, phase2, phase3;
  boolean improving;
  //boolean erase = false;    // used to unpaint
  double attFactor;
  double repFactor;
  double att_I;        // attraction in Phase I, etc.
  double att_II;
  double att_III;
  double att_user;      // user set attraction.
  double rep_I;
  double rep_II;
  double rep_III;
  double rep_user;      // user set repulsion.

  /* Values for converting mouseEvents into vertex coordinates */
  int wd, ht; //store the nomimal dimensions of the panel
  int xorig, yorig; 

  Color backgroundColor = new Color(160, 160, 160);

  // mouse stuff
  private Vertex currentVertex = null;
  private Vertex origVertex = null;
  private double origX;  // original x coord of a pressed in screen coords
  private double origY;  // original y coord of a pressed in screen coords
  private double[] yRange = new double[2]; // allowed y range in screen coords
  private Vertex[] currentEdge = null;
  private boolean draggingAllowed = true;
  private boolean draggingHorizontal = false;
  private boolean mouseDragged = false;
  private Point draggedMousePoint;

  // point labels
  String ptLabel = null;
  //Graphics tg = null;
  boolean justChanged = false; // from in to out or out to in
  //int currentMouseX = 0;
  //int currentMouseY = 0;

  // rotation stuff
  private boolean rotating = false;
  private double rotAngleDelta = Math.PI/90;
  private double rotationAngle;
  private javax.swing.Timer rotationTimer;
  //private int rotationDelay = 75;
  private int rotationDelay = 50;
  private int improveDelay = 100;
  private boolean useImproveDelay = true;

  // text label stuff; can be overridden by individual vertices
  private Color labelForegroundColor;
  private Color labelBackgroundColor = Color.YELLOW;
  private Color highlightColor = new Color(204, 0, 102);
  private Font labelFont;

  public DrawPanel() {
    //setBackground(Color.white);
    setBackground(backgroundColor);
    addMouseListener(this);
    addMouseMotionListener(this);
    changeSupport = new ChangeSupport(this);
    labelForegroundColor = getForeground();
  }

  public DrawPanel(Diagram d) {
    this();
    diagram = d;
    setup(d);
    //addMouseListener(this);
    //addMouseMotionListener(this);
  }

  /**
   *  If allowedVertices is not null, only those vertices (and edges)
   *  are drawn.
   */
  public void setAllowedVertices(Set<Vertex> verts) {
    allowedVertices = verts;
  }

  public Set<Vertex> getAllowedVertices() {
    return allowedVertices;
  }

  public void setUseImproveDelay(boolean v) { useImproveDelay =v ; }

  public void setImproveDelay(int delay) {
    if (delay < 5) improveDelay = 5;
    if (delay > 500) improveDelay = 500;
    improveDelay = delay;
  }

  public Diagram getDiagram() { return diagram; }

  /**
   * Set and setup the diagram and repaint.
   */
  public void setDiagram(Diagram d) {
    diagram = d;
    setup(d);
    repaint();
  }

/*
  public Diagram stringToDiagram(String s) {
    InputLattice in;
    Diagram d;

    try 
    {
      in = new InputLattice(s, false);
    } 
    catch (FileNotFoundException e) 
    {
      throw new InternalError(e.toString());
    } 
    catch (IOException e) 
    {
      throw new InternalError(e.toString());
    }


    // AS: Convert the InputLattice to a Diagram
    try 
    {
      d = new Diagram(in);
    } 
    catch (NonOrderedSetException e) 
    {    
      // can't occur now; change later
      throw new InternalError(e.toString());
    }
    return d;
  }
*/

/*
  public void setup (String s) {
    setup(stringToDiagram(s));
  }
*/

  /**
   * Setup the diagram in the drawing panel. 
   */
  public void setup (Diagram d) {
    improving = false;
    diagram = d;
    if (diagram == null) return;
    diagram.normalizeCoords();
    diagram.project2d(diagram.getRotationAngle());
    poset = d.getOrderedSet();
    // fire a PropertyChange event
    //Vector v = new Vector();
    //v.addElement(d.isInterval() ? Boolean.TRUE : Boolean.FALSE);
    //v.addElement(poset.zero().label());
    //v.addElement(poset.one().label());
    //v.addElement(isNew ? Boolean.TRUE : Boolean.FALSE);
    //changeSupport.firePropertyChange(latDrawWin.DIAGRAM_CHANGED, null, v);
    size = d.size();
    rotationAngle = diagram.getRotationAngle();
    phase1 = reps + size + 30;
    phase2 = phase1 + reps + size;
    phase3 = phase2 + reps + size + 20;
    attFactor = d.getAttractionFactor();
    repFactor = d.getRepulsionFactor();
    att_I = 0.5 * attFactor;
    att_II = 3.0 * attFactor;  // was 3.0
    att_III = 0.75 * attFactor;  // was 1.0
    rep_I = 3.0 * repFactor;
    rep_II = 0.5 * repFactor;  // was 0.5
    rep_III = 1.5 * repFactor;  // was 1.0
    att_user = att_III;  // for now same as stage 3.
    rep_user = rep_III;

    this.repaint();
  }

  public void increaseAttraction() { att_user = att_user * 1.1; }

  public void increaseReplusion() { rep_user = rep_user * 1.1; }

  public void decreaseAttraction() { att_user = att_user / 1.1; }

  public void decreaseReplusion() { rep_user = rep_user / 1.1; }

  /**
   * Each vertex can override this.
   */
  public Color getLabelForegroundColor() { return labelForegroundColor; }

  /**
   * Each vertex can override this.
   */
  public void getLabelForegroundColor(Color v) { labelForegroundColor = v; }

  /**
   * Each vertex can override this.
   */
  public Color getLabelBackgroundColor() { return labelBackgroundColor; }

  /**
   * Each vertex can override this.
   */
  public void getLabelBackgroundColor(Color v) { labelBackgroundColor = v; }

  /**
   * Each vertex can override this.
   */
  public Font getLabelFont() { return labelFont; }

  /**
   * Each vertex can override this.
   */
  public void getLabelFont(Font v) { labelFont = v; }

  public boolean isDraggingAllowed() { return draggingAllowed; }
  public void setDraggingAllowed(boolean v) { draggingAllowed = v; }

  public boolean isDraggingHorizontal() { return draggingHorizontal; }
  public void setDraggingHorizontal(boolean v) { draggingHorizontal = v; }

  public PropertyChangeSupport getChangeSupport() { return changeSupport; }

  public void paintBackground(Graphics2D g2) {
    g2.setColor(getBackground());
    g2.fillRect(0,0,getWidth(), getHeight());
  }

  public synchronized void paint(Graphics g) {
    paint(g, false);
  }

  public synchronized void paint(Graphics g, boolean noWhite) {
    Graphics2D g2 = (Graphics2D)g;
    paintBackground(g2);
    if (diagram == null) return;
    g2.setColor(getForeground());
    paintCovers(g2, noWhite);
    paintElems(g2, false);
    if (justChanged) {
      //paintLabel(g, wd, ht);
    }

  }

// ****************  Printing Stuff  *************

  public int print(Graphics g, PageFormat pf, int pageIndex) {
    // Tell the PrinterJob that there is only one page
    if (pageIndex != 0) return NO_SUCH_PAGE;
    Graphics2D g2 = (Graphics2D)g;
    Color bg = getBackground();
    setBackground(Color.white);
    g2.translate(pf.getImageableX(), pf.getImageableY());
    double pageWidth = pf.getImageableWidth();
    double pageHeight = pf.getImageableHeight();
    double width = getWidth();
    double height = getHeight();
    // Scale the example if needed
    double scalex = 1.0, scaley = 1.0;
    if (width > pageWidth) scalex = pageWidth/width;
    if (height > pageHeight) scaley = pageHeight/height;
    double scalefactor = Math.min(scalex, scaley);
    if (scalefactor != 1) g2.scale(scalefactor, scalefactor);
    paint(g2, true);
    setBackground(bg);
    // Tell the PrinterJob that we successfully printed the page
    return PAGE_EXISTS;
  }

  public boolean printDialog() {
    PrinterJob job = PrinterJob.getPrinterJob();
    job.setPrintable((Printable)this);
    // Display the print dialog to the user
    //System.out.println("# of print services: " 
    //              + PrinterJob.lookupPrintServices().length);
    if (job.printDialog()) {
    // If they didn't cancel it, then tell the job to start printing
      try {
        job.print();
      }
      catch(PrinterException e) {
        System.out.println("Couldn't print: " + e.getMessage());
      }
      return true;
    }
    return false;
  }


// *****************  Mouse Stuff  ****************

  /**
   * Let the "changeSupport" know something was clicked.
   */
  public void mousePressed(MouseEvent me) {
    if (diagram == null) return;
    String edgePropName = ChangeSupport.EDGE_PRESSED;
    String vertexPropName = ChangeSupport.VERTEX_PRESSED;
    if (me.isShiftDown()) vertexPropName = ChangeSupport.VERTEX_PRESSED_SHIFT;
    else if (me.isControlDown()) {
      vertexPropName = ChangeSupport.VERTEX_PRESSED_CTRL;
    }
    else if (me.isAltDown()) {
      vertexPropName = ChangeSupport.VERTEX_PRESSED_ALT;
    }

    if ((me.getModifiersEx() & InputEvent.BUTTON3_DOWN_MASK)
                                     == InputEvent.BUTTON3_DOWN_MASK) {
      edgePropName = ChangeSupport.EDGE_RIGHT_PRESSED;
      vertexPropName = ChangeSupport.VERTEX_RIGHT_PRESSED;
    }
    if ((me.getModifiersEx() & InputEvent.BUTTON2_DOWN_MASK)
                                     == InputEvent.BUTTON2_DOWN_MASK) {
      edgePropName = ChangeSupport.EDGE_MIDDLE_PRESSED;
      vertexPropName = ChangeSupport.VERTEX_MIDDLE_PRESSED;
    }
    currentVertex = vertexAt(me.getPoint());
    if (currentVertex != null) {
      origVertex = (Vertex)currentVertex.clone();
      origX = me.getPoint().getX();
      origY = me.getPoint().getY();
      if (draggingAllowed && !draggingHorizontal) setYRange();
      changeSupport.firePropertyChange(vertexPropName, null, currentVertex);
      return;
    }
    currentEdge = edgeAt(me.getPoint());
    if (currentEdge != null) {
      changeSupport.firePropertyChange(edgePropName, null, currentEdge);
      return;
    }
    if (!me.isPopupTrigger()) changeSupport.firePropertyChange(
                                 ChangeSupport.NOTHING_PRESSED, null, null);
  }
    

/*
System.out.println("raw " + e.getPoint());
System.out.println("lat " + 
 view.getScreenToLat().transform(e.getPoint(), null));
System.out.println("vertex is " + vertexAt(e.getPoint()));
Vertex[] edge = edgeAt(e.getPoint());
if (edge != null)
System.out.println("edge is [" + edge[0] + ", " + edge[1] + "]");
else System.out.println("edge is null");
*/


  public void mouseClicked(MouseEvent me) {}

  public void mouseMoved(MouseEvent me) {}
                                                                                
  public void mouseDragged(MouseEvent me) {
    if (diagram == null) return;
    if (currentVertex == null || !draggingAllowed) return;
    mouseDragged = true;
    // this modifies the project (but not the 3d coords) of the current vertex
    final Point2D proj = currentVertex.getProjection();
    draggedMousePoint = me.getPoint();
    // for unrestricted dragging use screen coords to keep y in the right range
    if (!draggingHorizontal) {
      if (me.getY() > yRange[1] - 2 * pointDiameter) {
        draggedMousePoint.setLocation(me.getX(), yRange[1] - 2 * pointDiameter);
      }
      else if (me.getY() < yRange[0] + 2 * pointDiameter) {
        draggedMousePoint.setLocation(me.getX(), yRange[0] + 2 * pointDiameter);
      }
    }
    view.getScreenToLat().transform(draggedMousePoint, proj);
    if (draggingHorizontal) {
      proj.setLocation(proj.getX(), origVertex.getProjectedY());
    }
    repaint();
  }
                                                                                
  public void mouseReleased(MouseEvent me) {
    if (diagram == null) return;
    if (mouseDragged) {
      mouseDragged = false;
      if (5 < me.getX() && me.getX() < getWidth() - 5 && 
          5 < me.getY() && me.getY() < getHeight() - 5) {
      // restore origVertex if move is not sane:
      // else, add origVertex to undo stack
        if (draggingHorizontal) {
          currentVertex.setHorizontalLocationFromDrag(
                     (me.getX() - origX) / view.getLatToScreen().getScaleX(),
                     diagram.getRotationAngle());
        }
        else {
          currentVertex.setLocationFromDrag(
                     (draggedMousePoint.getX() - origX) 
                                 / view.getLatToScreen().getScaleX(),
                     (draggedMousePoint.getY() - origY) 
                                / view.getLatToScreen().getScaleY(),
                     diagram.getRotationAngle());
        }
      }
      else {
        currentVertex.project2d(diagram.getRotationAngle());
      }
      repaint();
    }
  }
                                                                                
  public void mouseEntered(MouseEvent me) {
    requestFocus();
  }
                                                                                
  public void mouseExited(MouseEvent me) {}
                                                                                
  private void setYRange() {
    POElem elem = (POElem)poset.univ().get(
                       poset.elemOrder(currentVertex.getUnderlyingElem()));
    double maxY = 1.0;
    for (Iterator it = elem.upperCovers().iterator(); it.hasNext(); ) {
      Vertex pt2 = diagram.getVertices()[poset.elemOrder((POElem)it.next())];
      if (pt2.getProjectedY() < maxY) maxY = pt2.getProjectedY();
    }
    double minY = 0.0;
    for (Iterator it = elem.lowerCovers().iterator(); it.hasNext(); ) {
      Vertex pt2 = diagram.getVertices()[poset.elemOrder((POElem)it.next())];
      if (pt2.getProjectedY() > minY) minY = pt2.getProjectedY();
    }
    Point2D pt = new Point2D.Double(0.0, maxY);
    view.getLatToScreen().transform(pt, pt);
    maxY = pt.getY();
    pt = new Point2D.Double(0.0, minY);
    view.getLatToScreen().transform(pt, pt);
    minY = pt.getY();
    // now maxY and minY are inscreen coords so actually maxY < minY
    yRange[0] = maxY;
    yRange[1] = minY;
  }





/*
  public boolean mouseMove(Event e, int x, int y)
  {
    Vertex tmpv = vertexAt(x - xorig, y - yorig);
    // if we were on no vertex or are changing vertex...
    if (tmpv != null && ((currentVertex == null) || 
          !(new String(currentVertex.getLabel()).equals(tmpv.getLabel()) )))
    {
      currentVertex = tmpv;
      currentMouseX = x;
      currentMouseY = y;
      justChanged = true;
      repaint();
    } 
    // if we go from some vertex to no vertex ...
    else if ((tmpv == null) && (currentVertex != null))
    {
      currentVertex = null;
      justChanged = true;
      repaint();
    }
    else
    {
      justChanged = false;
    }
    return true;
  }
*/

  // this needs serious checking

  public void paintLabel(Graphics2D g, Vertex p,  int x, int y) {
    String label = p.getLabel();
    if (label == null || label.length() == 0) return;
    // can I do this ???
    Graphics2D tg = (Graphics2D)g.create();
    Shape clip = tg.getClip();
    if (p.getLabelFont() != null) tg.setFont(p.getLabelFont());
    else if (labelFont != null) tg.setFont(labelFont);
    // other wise just use what's there ??
    Rectangle2D rect = 
       tg.getFont().getStringBounds(label, tg.getFontRenderContext());
    rect = new Rectangle2D.Double(x, y, rect.getWidth(), rect.getHeight());
    //tg.clip(rect);
    Color bg = getBackground();
    if (p.getLabelBackgroundColor() != null) bg = p.getLabelBackgroundColor();
    else if (getLabelBackgroundColor() != null) bg = getLabelBackgroundColor();
    Color fg = getForeground();
    if (p.getLabelForegroundColor() != null) fg = p.getLabelForegroundColor();
    else if (getLabelForegroundColor() != null) fg = getLabelForegroundColor();
    tg.setColor(bg);
    tg.fill(rect);
    tg.setColor(fg);
    tg.drawString(label, x, y + (float)rect.getHeight() - 2);
    tg.dispose();
  }

/*
  public void paintLabel(Graphics2D g, int wd, int ht) 
  {
    Rectangle r = this.bounds();
    if (currentVertex != null)
    {
      int textwd = 
        g.getFontMetrics().getMaxAdvance()*currentVertex.getLabel().length();
      int textht = getFont().getSize();

      g.setColor(getForeground());
      if (tg != null)
        tg.dispose();
      tg = g.create(Math.min(currentMouseX,r.width-textwd),Math.max(currentMouseY - 20,0), 
        (int) Math.round(textwd*1.2)+5, (int) Math.round(textht*1.2));
      tg.setColor(Color.yellow);
      tg.fillRect(0,0, textwd+5, (int) Math.round(textht*1.1));
      tg.setColor(getForeground());
      tg.drawRect(0,0, textwd+5, (int) Math.round(textht*1.1));
      tg.setPaintMode();
      tg.drawString(currentVertex.getLabel(), 5,textht);
    }
    else
    {
      g.setColor(getBackground());
      if (tg != null)
        tg.dispose();
      g.setColor(getForeground());
    }
  }
*/

  public void paintCovers(Graphics2D g) {
    paintCovers(g, false);
  }

  // the boolean noWhite is for printing
  // note I am hard coding lightGray as the alt color for white
  public void paintCovers(Graphics2D g, boolean noWhite) {
    POElem elem;
    Vertex[] vertices = diagram.getVertices();
    Vertex pt, pt2;
    Point2D p = new Point2D.Double(); 
    Point2D p2 = new Point2D.Double();  // reusable storage
    java.util.List elems = poset.univ();
    HashMap edgeColors = diagram.getEdgeColors();
    AffineTransform l2s = view.getLatToScreen(getWidth(), getHeight());
    for (int i = 0; i < size; i++) {
      elem = (POElem)elems.get(i);
      pt = vertices[i];
      if (allowedVertices != null && !allowedVertices.contains(pt)) continue;
      for (Iterator covs = elem.upperCovers().iterator(); covs.hasNext(); ) {
        pt2 = vertices[poset.elemOrder((POElem)covs.next())];
        if (allowedVertices != null && !allowedVertices.contains(pt2)) continue;

        // this is an extremely subtle error: under Java 2, Vector
        // implements hashCode but under 1.1 it just inherits it
        // from Object so the lookup fails under 1.1.
        // we fixed this by making an Edge class. 
        // we should also do this with highlightedEdges
        // but since the applet doesn't use that, I'll do it later.

        if (edgeColors != null) {
          //String colorStr = (String)edgeColors.get(v);
          Edge e = new Edge(pt.getUnderlyingObjectlabel(), 
                            pt2.getUnderlyingObjectlabel());
          String colorStr = (String)edgeColors.get(e);
          if (colorStr != null) {
            Color c = getEdgeColor(colorStr);
            if (noWhite && c.equals(Color.white)) c = Color.lightGray;
            g.setColor(c);
          }
        }
        l2s.transform(pt.getProjection(), p);
        l2s.transform(pt2.getProjection(), p2);
        g.draw(new Line2D.Double(p, p2));
        // do something with the Stroke to draw a thick line
        //if (highlightedEdges.containsKey(v)) {
        //}
        g.setColor(getForeground());
      }
    }
  }

  HashMap edgeColorMap;

  // also add a way to import one
  public void setupEdgeColorMap() {
    edgeColorMap = new HashMap();
    edgeColorMap.put("1", new Color(255,0,0));
    edgeColorMap.put("2", new Color(255,255,0));
    edgeColorMap.put("3", new Color(0,255,255));
    edgeColorMap.put("4", new Color(0,0,255));
    edgeColorMap.put("5", new Color(0,0,0));
  }

  public Color getEdgeColor(String str) {
    if (edgeColorMap == null) setupEdgeColorMap();
    Color c = (Color)edgeColorMap.get(str);
    if (c != null) return c;
    return getForeground();
  }

  public void paintElems(Graphics2D g, boolean erase) {
    AffineTransform l2s = view.getLatToScreen(getWidth(), getHeight());
    final int n = size;
    Vertex[] vertices = diagram.getVertices();
    for (int i = 0; i < n; i++) {
      if (erase) eraseElem(g, vertices[i], l2s);
      else paintElem(g, vertices[i], l2s);
    }
  }

  /**
   * Return the edge at <i>x</i>, <i>y</i> for the raw mouse 
   * <i>x</i> and <i>y</i>.
   */
  public Vertex[] edgeAt(Point2D mousePt) {
    double x = mousePt.getX();
    double y = mousePt.getY();
    final double epsilon = 7;  // 7 pixels; maybe pointRadius*2 or something
    POElem elem;
    Vertex pt, pt2;
    Vertex[] vertices = diagram.getVertices();
    java.util.List elems = poset.univ();
    double x1,y1,x2,y2;
    for (int i = 0; i < size; i++) {
      elem = (POElem)elems.get(i);
      pt = vertices[i];
      Point2D screenPt = 
                 view.getLatToScreen().transform(pt.getProjection(), null);
      y1 = screenPt.getY();
      if (y >  y1 - epsilon) continue;  // these y's are down based
      x1 = screenPt.getX();
      Iterator covs = ((java.util.List)elem.upperCovers()).iterator();
      while (covs.hasNext()) {
        pt2 = vertices[poset.elemOrder((POElem)covs.next())];
        Point2D screenPt2 = 
                 view.getLatToScreen().transform(pt2.getProjection(), null);
        y2 = screenPt2.getY();
        if (y <  y2 + epsilon) continue;  // these y's are down based
        x2 = screenPt2.getX();
        double dx = x2 - x1;
        double dy = y2 - y1;
        double u = dx * (y - y1) - dy * (x - x1);
        if (u * u < 25 * (dx * dx + dy * dy)) {
          return new Vertex[] {pt, pt2};
        }
      }
    }
    return null;
  }

  /**
   * Return the edge at <i>x</i>, <i>y</i> for the raw mouse 
   * <i>x</i> and <i>y</i>.
   */
  public Vertex[] getEdgeAt(int xc, int yc) {
    long x = (long)(xc - xorig);
    long y = (long)(yc - yorig);
    final long epsilon = 7;  // 7 pixels; maybe pointRadius*2 or something
    POElem elem;
    Vertex pt, pt2;
    Vertex[] vertices = diagram.getVertices();
    java.util.List elems = poset.univ();
    long x1,y1,x2,y2;
    for (int i = 0; i < size; i++) {
      elem = (POElem)elems.get(i);
      pt = vertices[i];
      y1 = (long)converty(pt.getProjectedY(), ht);
      if (y >  y1 - epsilon) continue;  // these y's are down based
      x1 = (long)convertx(pt.getProjectedX(), wd);
      Iterator covs = ((java.util.List)elem.upperCovers()).iterator();
      while (covs.hasNext()) {
        pt2 = vertices[poset.elemOrder((POElem)covs.next())];
        y2 = (long)converty(pt2.getProjectedY(), ht);
        if (y <  y2 + epsilon) continue;  // these y's are down based
        x2 = (long)convertx(pt2.getProjectedX(), wd);
        long dx = x2 - x1;
        long dy = y2 - y1;
        long u = dx * (y - y1) - dy * (x - x1);
        if (u * u < 25 * (dx * dx + dy * dy)) {
          return new Vertex[] {pt, pt2};
        }
      }
    }
    return null;
  }

  protected Shape bigRect(Point2D p) {
    final int side = 10;
    final int halfSide = side/2;
    return new Rectangle2D.Double(p.getX() - halfSide,
                                  p.getY() - halfSide, side, side);
  }

  /**
   * Find the vertex at <i>pt</i> for the mouse point in screen coordinates.
   */
  protected Vertex vertexAt(Point2D pt) {
    final Shape rect = bigRect(pt);
    final Vertex[] verts = diagram.getVertices();
    final int n = verts.length;
    for (int i = 0; i < n; i++) {
      Vertex v = verts[i];
      final AffineTransform tr = view.getLatToScreen();
      if (rect.contains(tr.transform(v.getProjection(), null))) return v;
    }
    return null;
  }
  
  private static final Arc2D circle = new Arc2D.Double();

  public void paintElem(Graphics2D g, Vertex p, AffineTransform l2s) {
    if (allowedVertices != null && !allowedVertices.contains(p)) return;
    Point2D screenPt = new Point2D.Double();
    l2s.transform(p.getProjection(), screenPt);
    int x = (int)screenPt.getX();
    int y = (int)screenPt.getY();
    circle.setArc(x - pointRadius, 
                       y - pointRadius,
                       pointDiameter, 
                       pointDiameter, 0, 360, Arc2D.OPEN);
    g.setColor(this.getBackground());
    g.fill(circle);
    Color color = p.getColor();
    if (color != null) g.setColor(color);
    else g.setColor(this.getForeground());
    Stroke origStroke = null;
    if (p.isFilled()) g.fill(circle);
    else g.draw(circle);
    if (p.isHighlighted()) {
      circle.setArc(x - highlightPointRadius, y - highlightPointRadius,
                    highlightPointDiameter, highlightPointDiameter, 
                    0, 360, Arc2D.OPEN);
      origStroke = g.getStroke();
      g.setStroke(new BasicStroke(3f));
      g.setColor(highlightColor);
      g.draw(circle);
      g.setStroke(origStroke);
    }
    //if (p.isLabelPainted() && diagram.isPaintLabels()) paintLabel(g, p, x, y);
    if (diagram.isPaintLabels()) paintLabel(g, p, x, y);
  }

  public void setHighlightColor(Color v) { highlightColor = v; }
  public Color getHighlightColor() { return highlightColor; }

  public void eraseElem(Graphics2D g, Vertex p, AffineTransform l2s) {
    Point2D screenPt = new Point2D.Double();
    l2s.transform(p.getProjection(), screenPt);
    int x = (int)screenPt.getX();
    int y = (int)screenPt.getY();
    g.setColor(this.getBackground());
    g.fillOval(x - pointRadius, y - pointRadius, pointDiameter, pointDiameter);
  }

  int convertx(double x, int wd) {
    return (int)Math.round(x * wd);
  } 

  int converty(double y, int ht) {
    return (int)Math.round((1 - y) * ht);
  } 

  public boolean isRotating() { return rotating; }

  public void rotateOnce () {
    rotationAngle += rotAngleDelta;
    if (rotationAngle > 2*Math.PI) rotationAngle -= 2*Math.PI;
    diagram.setRotationAngle(rotationAngle);
    diagram.project2d(rotationAngle);
    repaint();
  }

  public void rotateLeft () {
    rotationAngle -= rotAngleDelta;
    if (rotationAngle < -2*Math.PI) rotationAngle += 2*Math.PI;
    diagram.setRotationAngle(rotationAngle);
    diagram.project2d(rotationAngle);
    repaint();
  }

  public javax.swing.Timer getRotationTimer() {
    if (rotationTimer == null) {
      ActionListener rotate = new ActionListener() {
          public void actionPerformed(ActionEvent evt) {
            rotateOnce();
          }
      };
      rotationTimer = new javax.swing.Timer(rotationDelay, rotate);
    }
    return rotationTimer;
  }

  public void startRotation () {
    getRotationTimer().start();
    rotating = true;
    changeSupport.firePropertyChange(ROTATION, STOPPED, STARTED); 
  }

  public void stopRotation () {
    getRotationTimer().stop();
    rotating = false;
    changeSupport.firePropertyChange(ROTATION, STARTED, STOPPED);
  }

  // /////////   Improvement stuff  /////////////////

  public void improveWithoutDelay(final int count) {
    for (int i = 0; i < count; i++) {
      improveOnce();
    }
    diagram.normalizeCoords();
    diagram.project2d(rotationAngle);
    repaint();
  }

  public void improveWithDelay(final int count) {
    final javax.swing.Timer timer = 
                              new javax.swing.Timer(improveDelay, null);
    timer.setCoalesce(false);
    ActionListener improver = new ActionListener() {
        int k = 0;
        public void actionPerformed(ActionEvent evt) {
          if (k < count) {
            k++;
            improveOnce();
          }
          else {
            timer.stop();
          }
        }
    };
    timer.addActionListener(improver);
    timer.start();
  }

  public void improveOnce () {
    improveMany(1, true);
  }

  public void improve() {
    int count = diagram.getImprovementCount();
    int n = phase1 + phase2 + phase3;
    if (count >= n) {
      if (useImproveDelay) improveWithDelay(20);
      else improveWithoutDelay(20);
    }
    else {
       if (useImproveDelay) improveWithDelay(n - count);
       else improveWithoutDelay(n - count);
    }
  }

  public void improveMany(int k, boolean repaint) {
    for (int i = 0; i < k; i++) {
      int count = diagram.getImprovementCount();
      if (count < phase1) {
        diagram.update(att_I, rep_I);
      } else if (phase1 <= count && count < phase2) {
        diagram.update(att_II, rep_II);
      } else if (phase2 <= count && count < phase3) {
        diagram.update(att_III, rep_III);
      } else {
        diagram.update(att_user, rep_user);
      }
    }
    if (repaint) {
      //if  (latDrawWin.showSteps) countDown.repaint();
      diagram.normalizeCoords();
      diagram.project2d(rotationAngle);
      repaint();
    }
  }

  public boolean writeRSFDiagram(String file) {
    return writeRSFDiagram(new File(file));
  }

  public boolean  writeRSFDiagram(File file) {
    try {
      diagram.writeRSFDiagram(file);
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(this, 
           "Could not write outx.ps", 
           "IO Exception", JOptionPane.ERROR_MESSAGE);
      e.printStackTrace();
      return false;
    }
    return true;
  }

}

