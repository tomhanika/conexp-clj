package no.geosoft.cc.graphics;



import no.geosoft.cc.geometry.Geometry;



/**
 * This class represent a client 3D plane world extent. The world extent
 * is defined by three points as follows:
 *
 * <pre>
 *        w2 o 
 *           |
 *           |
 *           |
 *        w0 o-------o w1
 * </pre>
 *
 * Each point is a 3D coordinate [x,y,z]. A typical world extent will have
 * w0[y] == w1[y], w0[x] == w2[x] and all z coordinates == 0, but the
 * definition makes it possible to use any planar suerface in 3D space
 * as world extent.
 * <p>
 * Clients sets world extent on a scene through the
 * <tt>GScene.setWorldExtent()</tt> methods.
 *
 * @see GScene#setWorldExtent (double[], double[], double[]).
 * @see GScene#setWorldExtent (double, double, double, double). 
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GWorldExtent
{
  // To enhance indexing readability
  private static final int X = 0;
  private static final int Y = 1;
  private static final int Z = 2;
  
  double  w0_[], w1_[], w2_[];  // [x,y,z]



  /**
   * Create a world extent specified by three coordinates as follows:
   *
   * <pre>
   *        w2 o 
   *           |
   *           |
   *           |
   *        w0 o-------o w1
   * </pre>
   * 
   * @param w0  First world extent coordinate [x,y,z].
   * @param w1  Second world extent coordinate [x,y,z].
   * @param w2  Third world extent coordinate [x,y,z].
   */
  GWorldExtent (double w0[], double w1[], double w2[])
  {
    w0_ = new double[3];
    w1_ = new double[3];
    w2_ = new double[3];

    set (w0, w1, w2);
  }

  
  
  /**
   * Create a default (normalized) world extent. The normalized
   * world extent is in the Z=0 plane with X/Y extents [0.0 - 1.0].
   */
  GWorldExtent()
  {
    this (new double[] {0.0, 0.0, 0.0},
          new double[] {1.0, 0.0, 0.0},
          new double[] {0.0, 1.0, 0.0});
  }
  


  /**
   * Create a world extent as a copy of the specified world extent.
   * 
   * @param worldExtent  World extent to copy.
   */
  GWorldExtent (GWorldExtent worldExtent)
  {
    this (worldExtent.get(0), worldExtent.get(1), worldExtent.get(2));
  }

  

  /**
   * Set the three world extent coordinates.
   * 
   * @param w0  Coordinate 0 of world extent [x,y,z].
   * @param w1  Coordinate 1 of world extent [x,y,z].
   * @param w2  Coordinate 2 of world extent [x,y,z].
   */
  void set (double w0[], double w1[], double w2[])
  {
    for (int i = 0; i < 3; i++) {
      w0_[i] = w0[i];
      w1_[i] = w1[i];
      w2_[i] = w2[i];      
    }
  }


  
  /**
   * Return world extent coordinate of specified index.
   * 
   * @param index  index of coordinate to get.
   * @return       Coordinate of point at index [x,y,z].
   */
  public double[] get (int index)
  {
    switch (index) {
      case 0  : return w0_;
      case 1  : return w1_;
      case 2  : return w2_;
    }

    throw new ArrayIndexOutOfBoundsException (index);
  }



  /**
   * Resize this viewport a specified fraction in X and Y direction.
   * 
   * @param dx  Fraction to resize in X direction.
   * @param dy  Fraction to resize in Y direction.
   */
  void resize (double dx, double dy)
  {
    double newWidth  = getWidth()  * dx;
    double newHeight = getHeight() * dy;

    extendWidth (newWidth);
    extendHeight (newHeight);
  }



  /**
   * Check if this world extent is in an XY plane (i.e. all Z coordinates
   * are the same).
   * 
   * @return  True if this world extent is in the XY plane, false otherwise.
   */
  boolean isXyPlane()
  {
    return w0_[Z] == w1_[Z] && w0_[Z] == w2_[Z];
  }


  
  /**
   * Check if this world extent is in an XZ plane (i.e. all Y coordinates
   * are the same).
   * 
   * @return  True if this world extent is in the XZ plane, false otherwise.
   */
  boolean isXzPlane()
  {
    return w0_[Y] == w1_[Y] && w0_[Y] == w2_[Y];
  }



  /**
   * Check if this world extent is in an YZ plane (i.e. all X coordinates
   * are the same).
   * 
   * @return  True if this world extent is in the YZ plane, false otherwise.
   */
  boolean isYzPlane()
  {
    return w0_[X] == w1_[X] && w0_[X] == w2_[X];
  }



  /**
   * Return width of this world extent.
   * 
   * @return   Width of this world extent.
   */
  public double getWidth()
  {
    return Geometry.length (w0_, w1_);
  }

  

  /**
   * Return height of this world extent.
   * 
   * @return   Height of this world extent.
   */
  public double getHeight()
  {
    return Geometry.length (w0_, w2_);
  }


  
  /**
   * Recompute this world extent to the specified width. Width is defined
   * as the length between w0 and w1.
   * The world extent is resized equally in each direction.
   * 
   * @param newWidth  New width of this world extent.
   */
  void extendWidth (double newWidth)
  {
    double oldW0[] = new double[3];
    oldW0[X] = w0_[X];
    oldW0[Y] = w0_[Y];
    oldW0[Z] = w0_[Z];
    
    Geometry.extendLine (w0_, w1_, newWidth, 0.5);

    double dx = w0_[X] - oldW0[X];
    double dy = w0_[Y] - oldW0[Y];
    double dz = w0_[Z] - oldW0[Z];
    
    w2_[X] += dx;
    w2_[Y] += dy;
    w2_[Z] += dz;
  }


  
  /**
   * Recompute this world extent to the specified height. Height is defined
   * as the length between w0 and w2.
   * The world extent is resized equally in each direction.
   * 
   * @param newHeight  New height of this world extent.
   */
  void extendHeight (double newHeight)
  {
    double oldW0[] = new double[3];
    oldW0[X] = w0_[X];
    oldW0[Y] = w0_[Y];
    oldW0[Z] = w0_[Z];
    
    Geometry.extendLine (w0_, w2_, newHeight, 0.5);

    double dx = w0_[X] - oldW0[X];
    double dy = w0_[Y] - oldW0[Y];
    double dz = w0_[Z] - oldW0[Z];
    
    w1_[X] += dx;
    w1_[Y] += dy;
    w1_[Z] += dz;
  }
}
