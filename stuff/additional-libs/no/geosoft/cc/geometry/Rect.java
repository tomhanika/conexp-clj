package no.geosoft.cc.geometry;



/**
 * A integer based rectangle. The strange name is to avoid name
 * clashes with java.awt.Rectangle.
 * <p>
 * Rect and Box represents the same concept, but their different definition
 * makes them suitable for use in different situations.
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class Rect
{
  public int  x;
  public int  y;
  public int  height;
  public int  width;


  
  /**
   * Create a rectangle.
   * 
   * @param x       X coordinate of upper left corner.
   * @param y       Y coordinate of upper left corner.
   * @param width   Width of rectangle.
   * @param height  Height of rectangle.
   */
  public Rect (int x, int y, int width, int height)
  {
    set (x, y, width, height);
  }


  /**
   * Create a default rectangle.
   */
  public Rect()
  {
    this (0, 0, 0, 0);
  }

  
  
  /**
   * Create a rectangle as a copy of the specified rectangle.
   * 
   * @param rectangle
   */
  public Rect (Rect rectangle)
  {
    this (rectangle.x, rectangle.y, rectangle.width, rectangle.height);
  }

  

  /**
   * Create a rectnagle based on specified box.
   * 
   * @param box  Box to create rectangle from.
   */
  public Rect (Box box)
  {
    this (box.x1, box.y1, box.x2 - box.x1, box.y2 - box.y1);
  }

  

  /**
   * Copy the specified rectangle.
   * 
   * @param rectangle  Rectangle to copy.
   */
  public void copy (Rect rectangle)
  {
    this.x      = rectangle.x;
    this.y      = rectangle.y;
    this.width  = rectangle.width;
    this.height = rectangle.height;
  }

  

  /**
   * Clone this rectangle
   * 
   * @return  Clone of this rectangle.
   */
  public Object clone()
  {
    return new Rect (x, y, width, height);
  }


  
  /**
   * Check if this rectangle equals the specified object.
   * 
   * @param object  Object to chack.
   * @return        True if the two equals, false otherwise.
   */
  public boolean equals (Object object)
  {
    Rect rectangle = (Rect) object;

    return this.x      == rectangle.x     &&
           this.y      == rectangle.y     &&
           this.width  == rectangle.width &&
           this.height == rectangle.height;
  }
  


  /**
   * Return true if this rectangle is empty.
   * 
   * @return  True if this rectangle is empty, false otherwise.
   */
  public boolean isEmpty()
  {
    return width <= 0 || height <= 0;
  }

  

  /**
   * Expand this rectangle the specified amount in each direction.
   * 
   * @param dx  Amount to expand to left and right.
   * @param dy  Amount to expand on top and botton.
   */
  public void expand (int dx, int dy)
  {
    x      -= dx;
    y      -= dy;
    width  += dx + dx;
    height += dy + dy;
  }

  
  
  /**
   * Set the parameters for this rectangle.
   * 
   * @param x       X coordinate of upper left corner.
   * @param y       Y coordinate of upper left corner.
   * @param width   Width of rectangle.
   * @param height  Height of rectangle.
   */
  public void set (int x, int y, int width, int height)
  {
    this.x      = x;
    this.y      = y;
    this.width  = width;
    this.height = height;
  }

  

  /**
   * Set this rectangle as extent of specified polyline.
   * 
   * @param xArray  X coordinates of polyline.
   * @param yArray  Y coordinates of polyline.
   */
  public void set (int xArray[], int yArray[])
  {
    int  minX = Integer.MAX_VALUE;
    int  maxX = Integer.MIN_VALUE;
    
    int  minY = Integer.MAX_VALUE;
    int  maxY = Integer.MIN_VALUE;

    for (int i = 0; i < xArray.length; i++) {
      if (xArray[i] < minX) minX = xArray[i];
      if (xArray[i] > maxX) maxX = xArray[i];

      if (yArray[i] < minY) minY = yArray[i];
      if (yArray[i] > maxY) maxY = yArray[i];
    }

    x = minX;
    y = minY;

    width  = maxX - minX + 1;
    height = maxY - minY + 1;    
  }


  
  /**
   * Return X coordinate of center of this rectangle.
   * 
   * @return  X coordinate of center of this rectangle.
   */
  public int getCenterX()
  {
    return x + (int) Math.floor (width / 2.0);
  }
  

  
  /**
   * Return Y coordinate of center of this rectangle.
   * 
   * @return  Y coordinate of center of this rectangle.
   */
  public int getCenterY()
  {
    return y + (int) Math.floor (height / 2.0);
  }
  

  
  /**
   * Return a string representation of this rectangle.
   * 
   * @return  String representation of this rectangle.
   */
  public String toString()
  {
    return new String ("Rectangle: x= " + x + " y=" + y +
                       " width=" + width + " height=" + height);
  }
  
}
