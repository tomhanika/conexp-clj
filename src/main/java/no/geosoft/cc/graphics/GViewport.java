package no.geosoft.cc.graphics;



import no.geosoft.cc.geometry.Geometry;



/**
 * This class represent the area of the window where graphics
 * will be drawn. The viewport is defined by three points as
 * follows:
 *
 * <pre>
 *     x0,y0 o-------o  x1,y1
 *           |
 *           |
 *           |
 *     x2,y2 o
 * </pre>
 *
 * A typical viewport will have y0 == y1 and x2 == x0, but the
 * definition makes it possible to create skewed viewports in
 * order to produce pseudo 3D graphics for instance.
 * <p>
 * Clients sets viewport on a scene through the
 * <tt>GScene.setViewport()</tt> methods.
 *
 * @see GScene#setViewport (int, int, int, int, int, int);
 * @see GScene#setViewport (int, int, int, int);
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GViewport
{
  // To enhance indexing readability
  private static final int X = 0;
  private static final int Y = 1;

  private final int  p0_[] = new int[2];
  private final int  p1_[] = new int[2];
  private final int  p2_[] = new int[2];


  
  /**
   * Create an empty viewport.
   */
  GViewport()
  {
    set (0, 0, 0, 0, 0, 0);
  }


  /**
   * Crete a rectangular viewport as specified.
   * 
   * @param x0      X coordinate of upper left corner of viewport.
   * @param y0      Y coordinate of upper left corner of viewport.
   * @param width   Width of viewport.
   * @param height  Height of viewport.
   */
  GViewport (int x0, int y0, int width, int height)
  {
    set (x0, y0, x0+width, y0, x0, y0+height);
  }
  
  

  /**
   * Create a viewport defined by three points. The points define
   * the viewport as follows:
   *
   * <pre>
   *     x0,y0 o-------o  x1,y1
   *           |
   *           |
   *           |
   *     x2,y2 o
   * </pre>
   * 
   * @param x0  X coordinate of first point.
   * @param y0  Y coordinate of first point.
   * @param x1  X coordinate of second point.
   * @param y1  Y coordinate of second point.
   * @param x2  X coordinate of third point.
   * @param y2  Y coordinate of third point.
   */
  GViewport (int x0, int y0, int x1, int y1, int x2, int y2)
  {
    set (x0, y0, x1, y1, x2, y2);
  }


  
  /**
   * Set coordinates that defines the viewport.
   * 
   * @param x0  X coordinate of first point.
   * @param y0  Y coordinate of first point.
   * @param x1  X coordinate of second point.
   * @param y1  Y coordinate of second point.
   * @param x2  X coordinate of third point.
   * @param y2  Y coordinate of third point.
   */
  void set (int x0, int y0, int x1, int y1, int x2, int y2)
  {
    p0_[X] = x0;
    p0_[Y] = y0;

    p1_[X] = x1;
    p1_[Y] = y1;

    p2_[X] = x2;
    p2_[Y] = y2;
  }

  

  /**
   * Return viewport coordinate of specified index.
   * 
   * @param index  Index of coordinate to get.
   * @return       Coordinate of point at index [x,y].
   */
  int[] get (int index)
  {
    switch (index) {
      case 0 : return p0_;
      case 1 : return p1_;
      case 2 : return p2_;        
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
    p0_[X] = (int) Math.round (p0_[X] * dx);
    p0_[Y] = (int) Math.round (p0_[Y] * dy);
    
    p1_[X] = (int) Math.round (p1_[X] * dx);
    p1_[Y] = (int) Math.round (p1_[Y] * dy);

    p2_[X] = (int) Math.round (p2_[X] * dx);
    p2_[Y] = (int) Math.round (p2_[Y] * dy);
  }
  

  
  /**
   * Return X coordinate of viewport point 0.
   * 
   * @return  X coordinate of viewport point 0.
   */
  public int getX0()
  {
    return p0_[X];
  }

  
  
  /**
   * Return Y coordinate of viewport point 0.
   * 
   * @return  Y coordinate of viewport point 0.
   */
  public int getY0()
  {
    return p0_[Y];
  }

  
  
  /**
   * Return X coordinate of viewport point 1.
   * 
   * @return  X coordinate of viewport point 1.
   */
  public int getX1()
  {
    return p1_[X];
  }

  
  
  /**
   * Return Y coordinate of viewport point 1.
   * 
   * @return  Y coordinate of viewport point 1.
   */
  public int getY1()
  {
    return p1_[Y];
  }

  
  
  /**
   * Return X coordinate of viewport point 2.
   * 
   * @return  X coordinate of viewport point 2.
   */
  public int getX2()
  {
    return p2_[X];
  }

  
  
  /**
   * Return Y coordinate of viewport point 2.
   * 
   * @return  Y coordinate of viewport point 2.
   */
  public int getY2()
  {
    return p2_[Y];
  }


  
  /**
   * Return X coordinate of viewport point 3. Point
   * 3 is the lower right corner of the viewport.
   * 
   * @return  X coordinate of viewport point 2.
   */
  public int getX3()
  {
    return p2_[X] + p1_[X] - p0_[X];
  }
  

  
  /**
   * Return Y coordinate of viewport point 3. Point
   * 3 is the lower right corner of the viewport.
   * 
   * @return  Y coordinate of viewport point 2.
   */
  public int getY3()
  {
    return p1_[Y] + p2_[Y] - p0_[Y];
  }


  
  /**
   * Return viewport point 0.
   * 
   * @return  Viewport point 0 [x,y].
   */
  int[] getP0()
  {
    return p0_;
  }
  

  
  /**
   * Return viewport point 1.
   * 
   * @return  Viewport point 1 [x,y].
   */
  int[] getP1()
  {
    return p1_;
  }


  
  /**
   * Return viewport point 2.
   * 
   * @return  Viewport point 2 [x,y].
   */
  int[] getP2()
  {
    return p2_;
  }



  /**
   * Return width of viewport
   * 
   * @return  Width of viewport.
   */
  public double getWidth()
  {
    return Geometry.length (p0_[X], p0_[Y], p1_[X], p1_[Y]);
  }


  
  /**
   * Return height of viewport
   * 
   * @return  Width of viewport.
   */
  public double getHeight()
  {
    return Geometry.length (p0_[X], p0_[Y], p2_[X], p2_[Y]);    
  }


  
  /**
   * Check if this viewport is skewed.
   * 
   * @return  True if this viewport is skewed, false otherwise.
   */
  boolean isSkewed()
  {
    return p0_[X] != p2_[X] || p0_[Y] != p1_[Y];
  }


  
  /**
   * Return shear factor in X direction of this viewport.
   * 
   * @return  Shear factor in X direction of this viewport.
   */
  double getShearFactorX()
  {
    return (double) (p2_[X] - p0_[X]) / (double) (p2_[Y] - p0_[Y]);
  }


  
  /**
   * Return shear factor in Y direction of this viewport.
   * 
   * @return  Shear factor in Y direction of this viewport.
   */
  double getShearFactorY()
  {
    return (double) (p1_[Y] - p0_[Y]) / (double) (p1_[X] - p0_[X]);
  }


  
  /**
   * Return horizontal center of this viewport.
   * 
   * @return  Horizontal center of this viewport.
   */
  public double getCenterX()
  {
    return (double) p2_[X] + (double) (p1_[X] - p0_[X]) / 2.0;
  }


  
  /**
   * Return vertical center of this viewport.
   * 
   * @return  Vertical center of this viewport.
   */
  public double getCenterY()
  {
    return (double) p1_[Y] + (double) (p2_[Y] - p0_[Y]) / 2.0;
  }
}
