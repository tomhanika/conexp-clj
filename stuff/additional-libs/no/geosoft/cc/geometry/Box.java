package no.geosoft.cc.geometry;



/**
 * A rectangle defined by its upper left (included) and lower
 * right (not included) corners.
 *
 * <pre>
 *   1##############
 *   ###############
 *   ###############
 *                  2
 * </pre>
 *
 * This corresponds to a Rect of width = x2 - x1 and height = y2 - y1.
 * <p>
 * Rect and Box represents the same concept, but their different definition
 * makes them suitable for use in different situations.
 *
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class Box 
  implements Cloneable
{
  public int  x1;
  public int  x2;
  public int  y1;
  public int  y2;


  
  /**
   * Create an empty box.
   */
  public Box()
  {
    set (0, 0, 0, 0);
  }



  /**
   * Create a new box as a copy of the specified box.
   * 
   * @param box  Box to copy.
   */
  public Box (Box box)
  {
    set (box.x1, box.y1, box.x2, box.y2);
  }

  

  /**
   * Create a new box with specified coordinates. The box includes
   * the (x1,y1) as upper left corner. The lower right corner (x2,y2)
   * is just outside the box.
   * 
   * @param x1  X of upper left corner (inclusive).
   * @param y1  Y of upper left corner (inclusive).
   * @param x2  X of lower right corner (not inclusive)
   * @param y2  Y of lower right corner (not inclusive).
   */
  public Box (int x1, int y1, int x2, int y2)
  {
    set (x1, y1, x2, y2);
  }


  
  /**
   * Create a new box based on the specified rectangle.
   * 
   * @param rectangle  Rectangle to copy.
   */
  public Box (Rect rectangle)
  {
    x1 = rectangle.x;
    y1 = rectangle.y;    
    x2 = rectangle.x + rectangle.width;
    y2 = rectangle.y + rectangle.height;
  }

  

  /**
   * Copy the specified box.
   * 
   * @param box  Box to copy.
   */
  public void copy (Box box)
  {
    set (box.x1, box.y1, box.x2, box.y2);    
  }

  

  /**
   * Clone this box.
   * 
   * @return  Clone of this box.
   */
  public Object clone()
  {
    return new Box (x1, y1, x2, y2);
  }
  

  
  /**
   * Set the parameters of this box.
   * 
   * @param x1  X coordinate of upper left corner of box.
   * @param y1  Y coordinate of upper left corner of box.
   * @param x2  X coordinate of lower right corner of box.
   * @param y2  Y coordinate of lower right corner of box.
   */
  public void set (int x1, int y1, int x2, int y2)
  {
    this.x1 = x1;
    this.y1 = y1;
    this.x2 = x2;
    this.y2 = y2;
  }


  

  /**
   * Check if the specified point is ionside this box.
   * 
   * @param x  X coordinate of point to check.
   * @param y  Y coordinate of point to check.
   * @return   True if the point is inside this box, false otherwise.
   */
  public boolean isInside (int x, int y)
  {
    return x >= x1 && x < x2 && y >= y1 && y < y2;
  }


  
  /**
   * Return true if this box is inside the specified box.
   * 
   * @param   box  Box to check if this is inside of.
   * @return       True if this box in inside the specified box,
   *               false otherwise.
   */
  public boolean isInsideOf (Box box)
  {
    return x1 >= box.x1 && y1 >= box.y1 && 
           x2 <= box.x2 && y2 <= box.y2;
  }
  


  /**
   * Return true if this box overlaps the specified box.
   * 
   * @param box  Box to check if this is inside of.
   * @return     True if this box overlaps the specified box,
   *             false otherwise.
   */
  public boolean isOverlapping (Box box)
  {
    return x2 > box.x1 && y2 > box.y1 && 
           x1 < box.x2 && y1 < box.y2;
  }
  

  
  /**
   * Return true if this box overlaps the specified rectangle.
   * 
   * @param rectangle  Rectnagle to check if this is inside of.
   * @return           True if this box overlaps the specified rectangle,
   *                   false otherwise.
   */
  public boolean isOverlapping (Rect rectangle)
  {
    return x2 > rectangle.x && x1 < rectangle.x + rectangle.width  &&
           y2 > rectangle.y && y1 < rectangle.y + rectangle.height;
  }
  
  

  /**
   * Offset this box a specified distance in x and y direction.
   * 
   * @param dx  Offset in x direction.
   * @param dy  Offset in y direction.
   */
  public void offset (int dx, int dy)
  {
    x1 += dx;
    y1 += dy;    
    x2 += dx;
    y2 += dy;
  }


  
  /**
   * Return a string representation of this box.
   * 
   * @return  String representation of this box.
   */
  public String toString()
  {
    return "Box: " + "y1=" + y1 + " y2=" + y2 + " x1=" + x1 + " x2=" + x2;
  }
}
