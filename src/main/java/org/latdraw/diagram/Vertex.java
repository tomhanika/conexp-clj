/* Vertex.java  (c) Ralph Freese 97/01/07 */


package org.latdraw.diagram;

import org.latdraw.orderedset.*;
import java.awt.geom.*;
import java.awt.*;
import java.util.*;


/* Vertex.java  (c) Ralph Freese 97/01/07

 * I am adding code which saves the last force calculation and takes the 
 * dot product with the new force. If this is positive the movement is 
 * increased but if it is negative it is decreased. This should prevent
 * wild oscillations. The formula I am using is to multiply v_new
 * (the new force) by
 *
 *                v_new \cdot v_old  
 *        1 + 0.5 -----------------
 *                |v_new| * |v_old|
 * 
 * 0.5 is the constant named factor; there is nothing scientific about it;
 * it just seems to work well.
 * <p> 
 * @version $Id: Vertex.java,v 1.9 2008/07/18 23:38:44 ralph Exp $
 */
 
/**
 * This holds the position of an element in 3 formats: 3d raw, 
 * 3d normalized, and a 2d projection. In the 3d normalized form
 * the z coordinate lies in [0, 1] and the projection to the x-y plane 
 * lies in the circle of radius 1/2. Also various force imformation
 * is stored here.
 */ 
public class Vertex implements Cloneable {

  /**
   * Label positionsr.
   */
  public static final int AUTOMATIC = 0;
  public static final int LEFT = 1;
  public static final int RIGHT = 2;
  public static final int TOP = 3;
  public static final int BOTTOM = 4;
  public static final int CENTER = 5;

  double x;
  double y;
  double z;
  private POElem underlyingElem;
  private Point2D currentForce;
  private Point2D previousForce;
  private static final double FACTOR = 0.5;
  Point2D proj2d;
  double normalizedX;  // These will be x, y, z scaled so that they are  
  double normalizedY;  // between 0 and 1.
  double normalizedZ;
  private String label = null;
  private int labelPosition = RIGHT;
  private Color color;
  private boolean filled = false;
  private boolean highlighted = false;
  private boolean labelPainted = false;
  private Color labelForegroundColor;
  private Color labelBackgroundColor;
  private Font labelFont;
  private boolean useOrderForLabel = false;

  private double scaleFactor;

  public Vertex(POElem e) {
    currentForce = new Point2D.Double();
    previousForce = null;
    proj2d = new Point2D.Double();
    underlyingElem = e;
  }

  public POElem getUnderlyingElem() { return underlyingElem; }  

  /**
   * The index in the linear extension of the ordered set.
   */
  public int index() {
    return getUnderlyingElem().index();
  }

  public void setUseOrderForLabel(boolean v) {
    useOrderForLabel = v;
  }

  public boolean isJoinIrreducible() {
    return getUnderlyingElem().isJoinIrreducible();
  }

  public boolean isMeetIrreducible() {
    return getUnderlyingElem().isMeetIrreducible();
  }

  //public List<Vertex> lowerCovers() {
  //  final List<Vertex> ans = new ArrayList<Vertex>();
  // 
  //  List<POElem> lc = (List<POElem>)getUnderlyingElem().lowerCovers();
  //  for (elt : lc) {
  //    
  //  return ans;
  //}

  public String getLabel() {
    if (useOrderForLabel) return Integer.toString(index());
    if (label != null) return label;
    return  getUnderlyingObjectlabel();
  }

  public String getUnderlyingObjectlabel() {
    return underlyingElem.getUnderlyingObject().toString();
  }

  public void setLabel(String v) { label = v; }

  /**
   * Get the label position which is one of the constants
   * AUTOMATIC, LEFT, RIGHT, TOP, BOTTOM, or CENTER.
   */
  public int getLabelPosition() { return labelPosition; }

  /**
   * Set the label position to be one of the constants
   * AUTOMATIC, LEFT, RIGHT, TOP, BOTTOM, or CENTER.
   */
  public void setLabelPosition(int v) { labelPosition = v; }

  public Color getColor() { return color; }
  public void setColor(Color c) { color = c; }

  public boolean isFilled() { return filled; }
  public void setFilled(boolean v) { filled = v; }

  public boolean isHighlighted() { return highlighted; }
  public void setHighlighted(boolean v) { highlighted = v; }

  public boolean isLabelPainted() { return labelPainted; }
  public void setLabelPainted(boolean v) { labelPainted = v; }

  /**
   * May return null in which case the default should be used.
   */
  public Font getLabelFont() { return labelFont; }
  public void setLabelFont(Font v) { labelFont = v; }

  public Color getLabelForegroundColor() { return labelForegroundColor; }
  public void setLabelForgeroundColor(Color c) { labelForegroundColor = c; }

  public Color getLabelBackgroundColor() { return labelBackgroundColor; }
  public void setLabelBackgroundColor(Color c) { labelBackgroundColor = c; }

  public double getX() { return x; }
  public void setX(double v) { x = v; }

  public double getY() { return y; }
  public void setY(double v) { y = v; }

  public double getZ() { return z; }
  public void setZ(double v) { z = v; }

  public double getNormalizedX() { return normalizedX; }

  public double getNormalizedY() { return normalizedY; }

  public double getNormalizedZ() { return normalizedZ; }

  /**
   * Get the underlyingObject (the label) of the underlying POElem.
   */
  public Object getUnderlyingObject() {
    return underlyingElem.getUnderlyingObject();
  }

  public Point2D getProjection() { return proj2d; } 

  public double getProjectedX() { return proj2d.getX(); } 

  public double getProjectedY() { return proj2d.getY(); } 

  public void reset() {
    setColor(null);
    setFilled(false);
    setHighlighted(false);
  }


  public void update () {
    double correction = 1.0;
    if (previousForce != null) {
      correction = 1.0 + FACTOR * correlation(currentForce, previousForce);
    } 
    else {
      previousForce = new Point2D.Double();
    }
    x += correction * currentForce.getX();
    y += correction * currentForce.getY();
    previousForce.setLocation(currentForce);
    currentForce.setLocation(0.0, 0.0);
  }

  public static double correlation(Point2D pt1, Point2D pt2) {
    //return 0.0;
    double len1 = Math.sqrt(pt1.getX() * pt1.getX() + pt1.getY() * pt1.getY());
    double len2 = Math.sqrt(pt2.getX() * pt2.getX() + pt2.getY() * pt2.getY());
    if (len1 == 0.0 || len2 == 0.0) {
      return(0.0);
    } else {
      return (pt1.getX()*pt2.getX() + pt1.getY()*pt2.getY()) / (len1*len2);
    }
  }


  public void adjustForce(double dx, double dy) {
    currentForce.setLocation(currentForce.getX() + dx, 
                             currentForce.getY() + dy);
  }

  protected void setNormalizedCoords() {
    setNormalizedCoords(scaleFactor);
  }

  protected void setNormalizedCoords(double scaleFactor) {
    this.scaleFactor = scaleFactor;
    normalizedX = x/scaleFactor;
    normalizedY = y/scaleFactor;
    normalizedZ = z/scaleFactor;
  }

  public void project2d(double angle) {
    proj2d.setLocation(Math.cos(angle) * normalizedX 
                       + Math.sin(angle) * normalizedY, normalizedZ);
  }

  public void project2d(double cos, double sin) {
    proj2d.setLocation(cos * normalizedX + sin * normalizedY, normalizedZ);
  }

  public void setLocationFromDrag(double deltaH, double deltaV, double angle) {
    final double foo = scaleFactor * deltaH;
    final double sin = Math.sin(angle);
    final double cos  = Math.cos(angle);
    x += foo * cos;
    y += foo * sin;
    z += scaleFactor * deltaV;
    setNormalizedCoords();
    project2d(cos, sin);
  }

  /**
   * This is for horizontal movements only. 
   *
   * @param delta  the <b>horizontal</b> movement in lat coordiantes in the
   *                2d projection.
   */
  public void setHorizontalLocationFromDrag(double delta, double angle) {
    final double foo = scaleFactor * delta;
    //final double foo = delta;
    final double sin = Math.sin(angle);
    final double cos  = Math.cos(angle);
    x += foo * cos;
    y += foo * sin;
    setNormalizedCoords();
    project2d(cos, sin);
  }

  public Object clone() {
    Vertex p = new Vertex(underlyingElem);
    p.x = x;
    p.y = y;
    p.z = z;
    p.currentForce = currentForce;
    p.previousForce = previousForce;
    p.proj2d = (Point2D)proj2d.clone();
    p.normalizedX = normalizedX;  
    p.normalizedY = normalizedY;  
    p.normalizedZ = normalizedZ;
    return p;
  }

  public String toString() {
    return "("+getLabel()+":" + x + ", " + y + ", " + z + ")" 
                 + "[" + proj2d.getX() + ", " + proj2d.getY() + "]";
    /*
    return "("+getLabel()+":" + normalizedX + ", " 
              + normalizedY + ", " + normalizedZ + ")" 
                 + "[" + proj2d.x + ", " + proj2d.y + "]";
    */
  }
}

