
package org.latdraw.beans;

import java.awt.*;
import java.awt.geom.*;
import java.util.*;


/**
 * This holds information about the current window (viewport) in 
 * lattice coordinates and the transformation to screen coordinates.
 * <p>
 * <b>3D Lattice Coordinates:</b> 3-space with the least element of the
 * lattice (or ordered set) at the origin. (Ordered set without 0
 * can be drawn by adding a 0 and not displaying it.) 
 * The <tt>z</tt> coordinate of every element will be nonnegative.
 * <p>
 * <b>Screen Transform:</b> the lattice coordinates will be projected
 * to a plane containing the <tt>z</tt> axis. (This is not done here but
 * in Diagram.) The origin will be 
 * mapped to a screen point centered horizontally and 5% above 
 * the bottom edge. 
 * <p>
 * This could handle zooming if we want it.
 *
 *
 *
 *
 *
 *
 * old:

 * This holds the current window (viewport) in lattice coordinates being 
 * displayed. Suppose you want to display the nose. If the length of the
 * board is 180, you might want to show from x = 160 to x = 190 and 
 * vertically you want the board centered. To do this you would
 * construct a View by <br><br>
 * <tt>new View(View.HORIZONTAL, new double[] {160, 190}, 0.0)</tt><br><br>
 *
 * So a View has a direction either HORIZONTAL or VERTICAL, a range,
 * and a value of the center in the other direction. There is also a
 * boolean to indicate whether the vertical coordinate goes up or down.
 *
 * There is also a verticalShiftFrac which will shift the the image 
 * vertically in screen by the screen (window) height times this.
 * 
 * In the HORIZONTAL case, 
 * with a range [x_0, x_1] of x values and a single 
 * y value, the view the user will see will be a rectangle with 
 * upper left [x_0, y - d] and lower right [x_1, y + d] where d is as big as
 * can be shown on the screen given that the aspect ratio must be preserved.
 * <p>
 * This class is bacically immutable except for the transforms.
 *
 * @author Ralph Freese
 * @version $Id: View.java,v 1.4 2003/08/25 09:12:07 ralph Exp $
 */
public class View { 

  /**
   * A transform applied to the lattice before it is mapped to screen,
   * usually a rotation.
   */
  AffineTransform latTransform;

  private AffineTransform latToScreen;
  private AffineTransform screenToLat;


  /**
   * Set the latToScreen transfrom for the view port to be shown.
   */
  public void setTransform(double w, double h) {
    AffineTransform ans = new AffineTransform();
    ans.translate(w / 2, 0.95 * h);
    ans.scale(0.9 * w, - 0.9 * h);
    latToScreen = ans;
    screenToLat = null;
  }


  public AffineTransform getLatToScreen() { return latToScreen; }

  public AffineTransform getLatToScreen(double wd, double ht) {
    setTransform(wd, ht);
    return latToScreen;
  }

  public AffineTransform getScreenToLat() {
    if (screenToLat == null) {
      try {
        screenToLat = latToScreen.createInverse();
      }
      catch (NoninvertibleTransformException e) {
        e.printStackTrace();
      }
    }
    return screenToLat;
  }

  public AffineTransform getScreenToLat(double wd, double ht) {
    setTransform(wd, ht);
    return getScreenToLat();
  }

}
