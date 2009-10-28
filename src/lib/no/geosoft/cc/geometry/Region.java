package no.geosoft.cc.geometry;



/**
 * A <em>Region</em> is simply an area, as the name implies, and is
 * implemented as a so called "y-x-banded" array of rectangles; Each Region
 * is made up of a certain number of rectangles sorted by y coordinate first,
 * and then by x coordinate.
 * <p>
 * Furthermore, the rectangles are banded such that every rectangle with a
 * given upper-left y coordinate (y1) will have the same lower-right y
 * coordinate (y2) and vice versa. If a rectangle has scanlines in a band,
 * it will span the entire vertical distance of the band. This means that
 * some areas that could be merged into a taller rectangle will be represented
 * as several shorter rectangles to account for shorter rectangles to its
 * left or right but within its "vertical scope".
 * <p>
 * An added constraint on the rectangles is that they must cover as much
 * horizontal area as possible. E.g. no two rectangles in a band are allowed
 * to touch. Whenever possible, bands will be merged together to cover a
 * greater vertical distance (and thus reduce the number of rectangles).
 * Two bands can be merged only if the bottom of one touches the top of the
 * other and they have rectangles in the same places (of the same width, of
 * course). This maintains the y-x-banding.
 * <p>
 * Region operations includes add (union), subtract, intersect, and
 * exclusive-or.
 * <p>
 * This class corresponds to Region.c of the X11 distribution and the
 * implemntation is based on it.
 * <p>
 * The <em>Region</em> is essentially equivalent to an AWT <em>Area</em>
 * but with different back-end implementation. Becnhmarking proves it more
 * than 100 times faster than AWT Area for binary CAG operations,
 * <p>
 * Thanks to:
 * <ul>
 * <li>Bryan Lin @ China Minmetals Corporation - for identifying
 *     synchronization errors when run on the MS WindowsXP platform.
 * </ul>
 *
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class Region
  implements Cloneable
{
  private static final int OPERATION_UNION        = 0;
  private static final int OPERATION_INTERSECTION = 1;
  private static final int OPERATION_SUBTRACTION  = 2;
  private static final int OPERATION_XOR          = 3;      

  private static final int INITIAL_SIZE = 40; // 10 rectangles
  
  // Temporary working area common for all regions for maximum performance
  private static int       gRectangles_[] = new int[INITIAL_SIZE];
  private static int       gNRectangles_  = 0;
  private static boolean   isLocked_      = false;

  private Box              extent_;
  private int              rectangles_[]; // y0,y1,x0,x1,.....
  private int              nRectangles_;

  
  
  /**
   * Create an empty region. Corresponds to XCreateRegion of X11.
   */
  public Region()
  {
    extent_      = new Box (0, 0, 0, 0);
    rectangles_  = new int[INITIAL_SIZE];  
    nRectangles_ = 0;
  }

  
  
  /**
   * Create the region covering the (possibly complex) polygon specified
   * by the x and y parameters.
   * Corresponds to XPolygonRegion of X11.
   * 
   * @param x  X values of polygon vertices.
   * @param y  Y values of polygon vertices.
   */
  public Region (int x[], int y[])  
  {
    // TODO. See PolyReg.c of X11.
  }


  
  /**
   * Create a region constituting of a single rectangle as specified.
   * 
   * @param rectangle  Rectangle to create region from.
   */
  public Region (Rect rectangle)
  {
    this();
    set (rectangle);
  }
  

  
  /**
   * Create a region consisting of one rectangle as specified.
   * 
   * @param x       X position of upper left corner of rectangle.
   * @param y       Y position of upper left corner of rectangle.
   * @param width   Width of rectangle.
   * @param height  Height of rectangle.
   */
  public Region (int x, int y, int width, int height)
  {
    this (new Rect (x, y, width, height));
  }



  /**
   * Create a region consisting of one rectangle as specified.
   * 
   * @param box  Box specification of rectangle to create region from.
   */
  public Region (Box box)
  {
    this (new Rect (box));
  }
  
  

  /**
   * Create a region as a copy of the specified region.
   * 
   * @param region  Region to copy.
   */
  public Region (Region region)
  {
    extent_ = new Box();
    rectangles_ = new int[region.nRectangles_ << 2];
    set (region);
  }
  


  /**
   * Clone this region.
   * 
   * @return  Clone of this region.
   */
  public Object clone()
  {
    return new Region (this);
  }


  
  /**
   * Convert this region to an AWT Area.
   * <p>
   * Note: The AWT classes are referenced explicitly here rather tham
   * importing them to indicate that the Region implementation does not
   * dependent on the AWT.
   * 
   * @return  Area equivalent of this rectangle.
   */
  public java.awt.geom.Area createArea()
  {
    if (nRectangles_ == 0) return null;
    
    java.awt.Rectangle rectangle =
      new java.awt.Rectangle (rectangles_[2],
                              rectangles_[0],
                              rectangles_[3] - rectangles_[2],
                              rectangles_[1] - rectangles_[0]);
    java.awt.geom.Area area = new java.awt.geom.Area (rectangle);
    
    for (int i = 1; i < nRectangles_; i++) {
      int j = i * 4;
      rectangle = new java.awt.Rectangle (rectangles_[j+2],
                                          rectangles_[j+0],
                                          rectangles_[j+3] - rectangles_[j+2],
                                          rectangles_[j+1] - rectangles_[j+0]);
      
      area.add (new java.awt.geom.Area (rectangle));
    }

    return area;
  }


  
  private static void checkMemory (Region region, int nRectangles)
  {
    int nEntries = nRectangles << 2;

    if (region == null) {
      if (gRectangles_.length < nEntries) {
        int newSize = nEntries * 2;
        int newArray[] = new int[newSize];
        System.arraycopy (gRectangles_, 0, newArray, 0, gRectangles_.length);
        gRectangles_ = newArray;
      }
    }
    else {
      if (region.rectangles_.length < nEntries) {
        int newSize = nEntries * 2;
        int newArray[] = new int[newSize];
        System.arraycopy (region.rectangles_, 0, newArray, 0,
                          region.rectangles_.length);
        region.rectangles_ = newArray;
      }
    }
  }
  
  

  /**
   * Set the content of this region according to the specified
   * region.
   * 
   * @param region  Region to copy.
   */
  public void set (Region region)
  {
    extent_.copy (region.extent_);

    checkMemory (this, region.nRectangles_);

    System.arraycopy (region.rectangles_, 0,
                      rectangles_, 0, region.nRectangles_ << 2);

    nRectangles_ = region.nRectangles_;
  }


  
  /**
   * Set the content of this region according to the specified
   * rectangle.
   *
   * @param rectangle  Rectangle to set region according to.
   */
  public void set (Rect rectangle)
  {
    rectangles_ = new int[INITIAL_SIZE];      

    if (rectangle.isEmpty()) {
      extent_      = new Box();
      nRectangles_ = 0;
    }
    else {
      extent_ = new Box (rectangle);
      rectangles_[0] = extent_.y1;
      rectangles_[1] = extent_.y2;    
      rectangles_[2] = extent_.x1;
      rectangles_[3] = extent_.x2;    
      nRectangles_ = 1;
    }
  }
  
  

  /**
   * Clear the region.
   */
  public void clear()
  {
    nRectangles_ = 0;
    extent_.set (0, 0, 0, 0);
  }



  /**
   * Return true if this region equals the specified object.
   * Corrensponds to XEqualRegion of X11.
   * 
   * @param object  Object to check against.
   * @return        True if the two regions equals, false otherwise.
   * @throws        ClassCastException if object is not of type Region.
   */
  public boolean equals (Object object) 
  {
    Region region = (Region) object;
    
    if      (nRectangles_ != region.nRectangles_) return false;
    else if (nRectangles_ == 0)                   return true;
    else if (extent_.x1 != region.extent_.x1)     return false;
    else if (extent_.x2 != region.extent_.x2)     return false;
    else if (extent_.y1 != region.extent_.y1)     return false;
    else if (extent_.y2 != region.extent_.y2)     return false;
    else {
      for (int i = 0; i < nRectangles_ << 2; i++)
        if (rectangles_[i] != region.rectangles_[i]) return false;
    }
    
    return true;
  }
  
  

  /**
   * Return true if the region is empty. Corresponds to XEmptyRegion in X11.
   * 
   * @return  True if the region is empty, false otherwise.
   */
  public boolean isEmpty()
  {
    return nRectangles_ == 0;
  }
  


  /**
   * Offset the entire region a specified distance.
   * Corresponds to XOffsetRegion in X11.
   * 
   * @param dx  Offset in x direction.
   * @param dy  Offset in y direction.
   */
  public void offset (int dx, int dy)
  {
    for (int i = 0; i < rectangles_.length; i+=4) {
      rectangles_[i+0] += dy;
      rectangles_[i+1] += dy;      
      rectangles_[i+2] += dx;
      rectangles_[i+3] += dx;      
    }

    extent_.offset (dx, dy);
  }

  

  /**
   * Return true if the specified region intersect this region.
   * 
   * @param region  Region to check against.
   * @return        True if the region intersects this one, false otherwise.
   */
  public boolean isIntersecting (Region region)
  {
    Region r = (Region) clone();
    r.intersect (region);
    return !r.isEmpty();
  }



  /**
   * Return true if the specified rectangle intersect this region.   
   * 
   * @param rectangle  Rectangle to check against.
   * @return           True if the rectangle intersects this, false otherwise.
   */
  public boolean isIntersecting (Rect rectangle)
  {
    Region region = new Region (rectangle);
    return isIntersecting (region);
  }



  /**
   * Return true if the specified point is inside this region.
   * This method corresponds to XPointInRegion in X11.
   * 
   * @param x  X part of point to check.
   * @param y  Y part of point to check.
   * @return   True if the point is inside the region, false otherwise.
   */
  public boolean isInside (int x, int y)
  {
    if (isEmpty())
      return false;
    
    if (!extent_.isInside (x, y))
      return false;

    int rEnd = nRectangles_ << 2;
    
    // Find correct band
    int i = 0;
    while (i < rEnd && rectangles_[i+1] < y) {
      if (rectangles_[i] > y) return false; // Passed the band
      i += 4;
    }

    // Check each rectangle in the band
    while (i < rEnd && rectangles_[i] <= y) {
      if (x >= rectangles_[i+2] && x < rectangles_[i+3]) return true;
      i += 4;
    }

    return false;
  }



  /**
   * Return true if the specified rectangle is inside this region.
   * This method corresponds to XRectInRegion in X11.   
   * 
   * @param rectangle  Rectangle to check.
   * @return           True if the rectangle is inside this region,
   *                   false otherwise.
   */
  public boolean isInside (Rect rectangle)
  {
    // Trivial reject case 1 
    if (isEmpty())
      return false;

    // Trivial reject case 2
    if (!extent_.isOverlapping (rectangle))
      return false;

    int x1 = rectangle.x;
    int x2 = rectangle.x + rectangle.width;
    int y1 = rectangle.y;
    int y2 = rectangle.y + rectangle.height;

    int rEnd = nRectangles_ << 2;
    
    // Find start band
    int i = 0;
    while (i < rEnd && rectangles_[i+1] < y1) {
      if (rectangles_[i] > y1) return false; // Passed the band
      i += 4;
    }

    while (i < rEnd) {
      int yTop = rectangles_[i];
    
      // Find start rectangle within band
      while (i < rEnd && rectangles_[i+3] <= x1) {
        if (rectangles_[i] > yTop) return false; // Passed the band
        i += 4;
      }
      
      if (i == rEnd) return false;
      
      // This rectangle must cover the entire rectangle horizontally
      if (x1 < rectangles_[i+2] || x2 > rectangles_[i+3]) return false;
      
      // See if we are done
      if (rectangles_[i+1] >= y2) return true;
      
      // Move to next band
      i += 4;
      while (i < rEnd && rectangles_[i] == yTop)
        i += 4;
    }

    return false;
  }
  

  
  /**
   * Return true if this region is inside of the specified rectangle.
   * 
   * @param rectangle  Rectangle to check if this is inside of.
   * @return           True if this region is inside the specified rectangle,
   *                   false otherwise.
   */
  public boolean isInsideOf (Rect rectangle)
  {
    return subtract (this, rectangle).isEmpty();
  }

  

  /**
   * Return the extent of the region.
   * Correspond to XClipBox in X11.
   * 
   * @return  The extent of this region.
   */
  public Rect getExtent()
  {
    return new Rect (extent_);
  }



  /**
   * Return the number of rectangles in the region. In case the number
   * is getting very high, the application might choose to call collapse().
   *
   * @return  Number of rectangles this region consists of.
   */
  public int getNRectangles()
  {
    return nRectangles_;
  }
  
  

  /**
   * Collapse the region into its extent box. Useful if the region becomes
   * very complex (number of rectangles is getting high) and the client
   * accepts the (in general) coarser result region.
   */
  public void collapse()
  {
    rectangles_[0] = extent_.y1;
    rectangles_[1] = extent_.y2;    
    rectangles_[2] = extent_.x1;
    rectangles_[3] = extent_.x2;
    nRectangles_ = 1;
  }
  
  

  /**
   * Perform a logical set operation between this and the specified
   * region. Corresponds to miRegionOp in Region.c of X11.
   * 
   * @param region         Region to combine with.
   * @param operationType  Combination operator.
   */
  private void combine (Region region, int operationType)
  {
    // This is the only method (with sub methods) that utilize the
    // common working area gRectangles_. The lock ensures that only
    // one thread access this variable at any time.
    while (isLocked_);
    isLocked_ = true;

    int r1 = 0;
    int r2 = 0;
    int r1End = nRectangles_        << 2;
    int r2End = region.nRectangles_ << 2;

    // Initialize the working region
    gNRectangles_ = 0;

    int yTop    = 0;
    int yBottom = extent_.y1 < region.extent_.y1 ?
                  extent_.y1 : region.extent_.y1;

    int previousBand = 0;
    int currentBand;

    int r1BandEnd, r2BandEnd;
    int top, bottom;
    
    // Main loop
    do {
      currentBand = gNRectangles_;

      // Find end of the current r1 band
      r1BandEnd = r1 + 4;
      while (r1BandEnd != r1End &&
             rectangles_[r1BandEnd] == rectangles_[r1])
        r1BandEnd += 4;

      // Find end of the current r2 band
      r2BandEnd = r2 + 4;
      while (r2BandEnd != r2End &&
             region.rectangles_[r2BandEnd] == region.rectangles_[r2])
        r2BandEnd += 4;

      // First handle non-intersection band if any
      if (rectangles_[r1] < region.rectangles_[r2]) {
        top    = Math.max (rectangles_[r1],    yBottom);
        bottom = Math.min (rectangles_[r1+1],  region.rectangles_[r2]);

        if (top != bottom)
          nonOverlap1 (rectangles_, r1, r1BandEnd, top, bottom, operationType);

        yTop = region.rectangles_[r2];
      }
      else if (region.rectangles_[r2] < rectangles_[r1]) {
        top    = Math.max (region.rectangles_[r2],   yBottom);
        bottom = Math.min (region.rectangles_[r2+1], rectangles_[r1]);

        if (top != bottom)
          nonOverlap2 (region.rectangles_,
                       r2, r2BandEnd, top, bottom, operationType);

        yTop = rectangles_[r1];
      }
      else
        yTop = rectangles_[r1];
      
      // Then coalesce if possible
      if (gNRectangles_ != currentBand)
        previousBand = coalesceBands (previousBand, currentBand);
      currentBand = gNRectangles_;

      // Check if this is an intersecting band
      yBottom = Math.min (rectangles_[r1+1], region.rectangles_[r2+1]);
      if (yBottom > yTop)
        overlap (rectangles_,        r1, r1BandEnd,
                 region.rectangles_, r2, r2BandEnd,
                 yTop, yBottom, operationType);

      // Coalesce again
      if (gNRectangles_ != currentBand)
        previousBand = coalesceBands (previousBand, currentBand);

      // If we're done with a band, skip forward in the region to the next band
      if (rectangles_[r1+1]        == yBottom) r1 = r1BandEnd;
      if (region.rectangles_[r2+1] == yBottom) r2 = r2BandEnd;

    } while (r1 != r1End && r2 != r2End);

    currentBand = gNRectangles_;
    
    //
    // Deal with whichever region still has rectangles left
    //
    if (r1 != r1End) {
      do {

        r1BandEnd = r1;
        while (r1BandEnd < r1End &&
               rectangles_[r1BandEnd] == rectangles_[r1])
          r1BandEnd += 4;

        top    = Math.max (rectangles_[r1], yBottom);
        bottom = rectangles_[r1+1];
        
        nonOverlap1 (rectangles_, r1, r1BandEnd, top, bottom, operationType);
        r1 = r1BandEnd;
        
      } while (r1 != r1End);
    }
    else if (r2 != r2End) {
      do {

        r2BandEnd = r2;
        while (r2BandEnd < r2End &&
               region.rectangles_[r2BandEnd] == region.rectangles_[r2])
          r2BandEnd += 4;

        top    = Math.max (region.rectangles_[r2], yBottom);
        bottom = region.rectangles_[r2+1];
        
        nonOverlap2 (region.rectangles_, r2, r2BandEnd, top, bottom,
                     operationType);
        r2 = r2BandEnd;
        
      } while (r2 != r2End);
    }

    // Coalesce again
    if (currentBand != gNRectangles_)
      coalesceBands (previousBand, currentBand);

    // Copy the work region into this
    checkMemory (this, gNRectangles_);
    System.arraycopy (gRectangles_, 0, rectangles_, 0, gNRectangles_ << 2);
    nRectangles_ = gNRectangles_;

    isLocked_ = false;
  }


  
  private void nonOverlap1 (int rectangles[], int r, int rEnd,
                            int yTop, int yBottom, int operationType)
  {
    int i = gNRectangles_ << 2;
    
    if (operationType == OPERATION_UNION ||
        operationType == OPERATION_SUBTRACTION) {
      while (r != rEnd) {
        checkMemory (null, gNRectangles_ + 1);

        gRectangles_[i] = yTop;            i++;
        gRectangles_[i] = yBottom;         i++;
        gRectangles_[i] = rectangles[r+2]; i++;
        gRectangles_[i] = rectangles[r+3]; i++;
        gNRectangles_++;
        r += 4;
      }
    }
  }
  

  
  private void nonOverlap2 (int rectangles[], int r, int rEnd,
                            int yTop, int yBottom, int operationType)
  {
    int i = gNRectangles_ << 2;
    
    if (operationType == OPERATION_UNION) {
      while (r != rEnd) {
        checkMemory (null, gNRectangles_ + 1);
        gRectangles_[i] = yTop;            i++;
        gRectangles_[i] = yBottom;         i++;
        gRectangles_[i] = rectangles[r+2]; i++;
        gRectangles_[i] = rectangles[r+3]; i++;

        gNRectangles_++;
        r += 4;
      }
    }
  }
  

  private void overlap (int rectangles1[], int r1, int r1End,
                        int rectangles2[], int r2, int r2End,
                        int yTop, int yBottom, int operationType)
  {
    int i = gNRectangles_ << 2;

    //
    // UNION
    //
    if (operationType == OPERATION_UNION) {
      while (r1 != r1End && r2 != r2End) {
        if (rectangles1[r1+2] < rectangles2[r2+2]) {
          if (gNRectangles_ > 0            &&
              gRectangles_[i-4] == yTop    &&
              gRectangles_[i-3] == yBottom &&
              gRectangles_[i-1] >= rectangles1[r1+2]) {
            if (gRectangles_[i-1] < rectangles1[r1+3])
              gRectangles_[i-1] = rectangles1[r1+3];
          }
          else {
            checkMemory (null, gNRectangles_ + 1);
            
            gRectangles_[i]   = yTop;
            gRectangles_[i+1] = yBottom;
            gRectangles_[i+2] = rectangles1[r1+2];
            gRectangles_[i+3] = rectangles1[r1+3];
            
            i += 4;
            gNRectangles_++;
          }

          r1 += 4;
        }
        else {
          if (gNRectangles_ > 0            &&
              gRectangles_[i-4] == yTop    &&
              gRectangles_[i-3] == yBottom &&
              gRectangles_[i-1] >= rectangles2[r2+2]) {
            if (gRectangles_[i-1] < rectangles2[r2+3])
              gRectangles_[i-1] = rectangles2[r2+3];
          }
          else {
            checkMemory (null, gNRectangles_ + 1);

            gRectangles_[i]   = yTop;
            gRectangles_[i+1] = yBottom;
            gRectangles_[i+2] = rectangles2[r2+2];
            gRectangles_[i+3] = rectangles2[r2+3];

            i += 4;
            gNRectangles_++;
          }

          r2 += 4;
        }
      }

      if (r1 != r1End) {
        do {
          if (gNRectangles_ > 0            &&
              gRectangles_[i-4] == yTop    &&
              gRectangles_[i-3] == yBottom &&
              gRectangles_[i-1] >= rectangles1[r1+2]) {
            if (gRectangles_[i-1] < rectangles1[r1+3])
              gRectangles_[i-1] = rectangles1[r1+3];
          }
          else {
            checkMemory (null, gNRectangles_ + 1);

            gRectangles_[i]   = yTop;
            gRectangles_[i+1] = yBottom;
            gRectangles_[i+2] = rectangles1[r1+2];
            gRectangles_[i+3] = rectangles1[r1+3];

            i += 4;
            gNRectangles_++;
          }

          r1 += 4;
          
        } while (r1 != r1End);
      }
      else {
        while (r2 != r2End) {
          if (gNRectangles_ > 0            &&
              gRectangles_[i-4] == yTop    &&
              gRectangles_[i-3] == yBottom &&
              gRectangles_[i-1] >= rectangles2[r2+2]) {
            if (gRectangles_[i-1] < rectangles2[r2+3])
              gRectangles_[i-1] = rectangles2[r2+3];
          }
          else {
            checkMemory (null, gNRectangles_ + 1);

            gRectangles_[i]   = yTop;
            gRectangles_[i+1] = yBottom;
            gRectangles_[i+2] = rectangles2[r2+2];
            gRectangles_[i+3] = rectangles2[r2+3];

            i += 4;
            gNRectangles_++;
          }

          r2 += 4;
        }
      }
    }

    //
    // SUBTRACT
    //
    else if (operationType == OPERATION_SUBTRACTION) {
      int x1 = rectangles1[r1+2];
      
      while (r1 != r1End && r2 != r2End) {
        if (rectangles2[r2+3] <= x1)
          r2 += 4;
        else if (rectangles2[r2+2] <= x1) {
          x1 = rectangles2[r2+3];
          if (x1 >= rectangles1[r1+3]) {
            r1 += 4;
            if (r1 != r1End) x1 = rectangles1[r1+2];
          }
          else
            r2 += 4;
        }
        else if (rectangles2[r2+2] < rectangles1[r1+3]) {
          checkMemory (null, gNRectangles_ + 1);
          
          gRectangles_[i+0] = yTop;
          gRectangles_[i+1] = yBottom;
          gRectangles_[i+2] = x1;
          gRectangles_[i+3] = rectangles2[r2+2];

          i += 4;
          gNRectangles_++;

          x1 = rectangles2[r2+3];
          if (x1 >= rectangles1[r1+3]) {
            r1 += 4;
            if (r1 != r1End) x1 = rectangles1[r1+2];
            else             r2 += 4;
          }
        }
        else {
          if (rectangles1[r1+3] > x1) {
            checkMemory (null, gNRectangles_ + 1);
            
            gRectangles_[i+0] = yTop;
            gRectangles_[i+1] = yBottom;
            gRectangles_[i+2] = x1;
            gRectangles_[i+3] = rectangles1[r1+3];

            i += 4;
            gNRectangles_++;
          }
          
          r1 += 4;
          if (r1 != r1End) x1 = rectangles1[r1+2];
        }
      }
      while (r1 != r1End) {
        checkMemory (null, gNRectangles_ + 1);
          
        gRectangles_[i+0] = yTop;
        gRectangles_[i+1] = yBottom;
        gRectangles_[i+2] = x1;
        gRectangles_[i+3] = rectangles1[r1+3];

        i += 4;
        gNRectangles_++;

        r1 += 4;
        if (r1 != r1End) x1 = rectangles1[r1+2];
      }
    }

    //
    // INTERSECT
    //
    else if (operationType == OPERATION_INTERSECTION) {
      while (r1 != r1End && r2 != r2End) {
        int x1 = Math.max (rectangles1[r1+2], rectangles2[r2+2]);
        int x2 = Math.min (rectangles1[r1+3], rectangles2[r2+3]);

        if (x1 < x2) {
          checkMemory (null, gNRectangles_ + 1);

          gRectangles_[i]   = yTop;
          gRectangles_[i+1] = yBottom;
          gRectangles_[i+2] = x1;
          gRectangles_[i+3] = x2;
          
          i += 4;
          gNRectangles_++;
        }

        if      (rectangles1[r1+3] < rectangles2[r2+3]) r1 += 4;
        else if (rectangles2[r2+3] < rectangles1[r1+3]) r2 += 4;
        else {
          r1 += 4;
          r2 += 4;
        }
      }
    }
  }
  
  

  /**
   * Corresponds to miCoalesce in Region.c of X11.
   */
  private int coalesceBands (int previousBand, int currentBand)
  {
    int r1   = previousBand  << 2;
    int r2   = currentBand   << 2;
    int rEnd = gNRectangles_ << 2;

    // Number of rectangles in prevoius band
    int nRectanglesInPreviousBand = currentBand - previousBand;

    // Number of rectangles in current band
    int nRectanglesInCurrentBand  = 0;
    int r = r2;
    int y = gRectangles_[r2];
    while (r != rEnd && gRectangles_[r] == y) {
      nRectanglesInCurrentBand++;
      r += 4;
    }

    // If more than one band was added, we have to find the start
    // of the last band added so the next coalescing job can start
    // at the right place.
    if (r != rEnd) {
      rEnd -= 4;
      while (gRectangles_[rEnd-4] == gRectangles_[rEnd])
        rEnd -= 4;

      currentBand = rEnd >> 2 - gNRectangles_;
      rEnd = gNRectangles_ << 2;
    }

    if (nRectanglesInCurrentBand == nRectanglesInPreviousBand &&
        nRectanglesInCurrentBand != 0) {
      
      // The bands may only be coalesced if the bottom of the previous
      // band matches the top of the current.
      if (gRectangles_[r1+1] == gRectangles_[r2]) {
        
        // Chek that the bands have boxes in the same places
        do {
          if ((gRectangles_[r1+2] != gRectangles_[r2+2]) ||
              (gRectangles_[r1+3] != gRectangles_[r2+3]))
            return currentBand; // No coalescing
          
          r1 += 4;
          r2 += 4;
          
          nRectanglesInPreviousBand--;
        } while (nRectanglesInPreviousBand != 0);

        //
        // OK, the band can be coalesced
        //
        
        // Adjust number of rectangles and set pointers back to start
        gNRectangles_ -= nRectanglesInCurrentBand;
        r1 -= nRectanglesInCurrentBand << 2;
        r2 -= nRectanglesInCurrentBand << 2;        

        // Do the merge
        do {
          gRectangles_[r1+1] = gRectangles_[r2+1];
          r1 += 4;
          r2 += 4;
          nRectanglesInCurrentBand--;
        } while (nRectanglesInCurrentBand != 0);

        // If only one band was added we back up the current pointer
        if (r2 == rEnd)
          currentBand = previousBand;
        else {
          do {
            gRectangles_[r1] = gRectangles_[r2];
            r1++;
            r2++;
          } while (r2 != rEnd);
        }
      }
    }

    return currentBand;
  }
  


  /**
   * Update region extent based on rectangle values.
   */
  private void updateExtent()
  {
    if (nRectangles_ == 0)
      extent_.set (0, 0, 0, 0);
    else {
      // Y values
      extent_.y1 = rectangles_[0];
      extent_.y2 = rectangles_[(nRectangles_ << 2) - 3];

      // X values initialize
      extent_.x1 = rectangles_[2];
      extent_.x2 = rectangles_[3];
      
      // Scan all rectangles for extreme X values
      for (int i = 4; i < nRectangles_ << 2; i += 4) {
        if (rectangles_[i+2] < extent_.x1) extent_.x1 = rectangles_[i+2];
        if (rectangles_[i+3] > extent_.x2) extent_.x2 = rectangles_[i+3];
      }
    }
  }


  
  /**
   * Union this region with the specified region.
   * Corresponds to XUnionRegion in X11.
   *
   * @param region  Region to union this with.
   */
  public void union (Region region) 
  {
    // Trivial case #1. Region is this or empty
    if (this == region || region.isEmpty())
      return;

    // Trivial case #2. This is empty
    if (isEmpty()) {
      set (region);
      return;
    }
    
    // Trivial case #3. This region covers the specified one
    if (rectangles_.length == 1 && region.extent_.isInsideOf (extent_))
      return;

    // Trivial case #4. The specified region covers this one
    if (region.rectangles_.length == 1 &&
        extent_.isInsideOf (region.extent_)) {
      set (region);
      return;
    }

    // Ceneral case
    combine (region, OPERATION_UNION);

    // Update extent
    extent_.x1 = Math.min (extent_.x1, region.extent_.x1);
    extent_.y1 = Math.min (extent_.y1, region.extent_.y1);
    extent_.x2 = Math.max (extent_.x2, region.extent_.x2);
    extent_.y2 = Math.max (extent_.y2, region.extent_.y2);
  }



  /**
   * Union this region with the specified rectangle.
   * Corresponds to XUnionRectWithRegion in X11.
   * 
   * @param rectangle  Rectangle to union this with.
   */
  public void union (Rect rectangle)
  {
    if (rectangle.isEmpty())
      return;
    
    union (new Region (rectangle));
  }
  

  
  /**
   * Create a new region as the union between two specified regions.
   * 
   * @param r1  First region to union.
   * @param r2  Second region to union.
   * @return    Union of the two specified regions.
   */
  public static Region union (Region r1, Region r2)
  {
    Region region = new Region (r1);
    region.union (r2);
    return region;
  }


  
  /**
   * Create a new region as the union between a region and a rectangle.
   * 
   * @param region     Region to union.
   * @param rectangle  Rectangle to intersect with.
   * @return           Union of the region and the rectangle.
   */
  public static Region union (Region region, Rect rectangle)
  {
    if (rectangle.isEmpty())
      return new Region (region);
    else
      return union (region, new Region (rectangle));
  }
  
  
  
  /**
   * Leave this region as the intersection between this region and the
   * specified region.
   * Corresponds to XIntersectRegion in X11.
   * 
   * @param region  Region to intersect this with.
   */
  public void intersect (Region region)
  {
    // Trivial case which results in an empty region
    if (isEmpty() || region.isEmpty() ||
        !extent_.isOverlapping (region.extent_)) {
      clear();
      return;
    }

    // General case
    combine (region, OPERATION_INTERSECTION);

    // Update extent
    updateExtent();
  }

  

  /**
   * Leave this region as the intersection between this region and the
   * specified rectangle.
   * 
   * @param region  Region to intersect this with.
   */
  public void intersect (Rect rectangle)
  {
    if (rectangle.isEmpty())
      clear();
    else
      intersect (new Region (rectangle));
  }



  /**
   * Create a new region as the intersection between two specified regions.
   * 
   * @param r1  First region to intersect.
   * @param r2  Second region to intersect.
   * @return    Intersection between the two specified regions.
   */
  public static Region intersect (Region r1, Region r2)
  {
    Region region = new Region (r1);
    region.intersect (r2);
    return region;
  }
  


  /**
   * Create a new region as the intersection between a region and a rectangle.
   * 
   * @param region     Region to intersect.
   * @param rectangle  Rectangle to intersect with.
   * @return           Intersection between the region and the rectangle.
   */
  public static Region intersect (Region region, Rect rectangle)
  {
    if (rectangle.isEmpty())
      return new Region();
    else
      return intersect (region, new Region (rectangle));
  }
  

  
  /**
   * Subtract the specified region from this region.
   * Corresponds to XSubtractRegion in X11.
   * 
   * @param region  Region to subtract from this region.
   */
  public void subtract (Region region)
  {
    // Trivial check for non-op
    if (isEmpty() || region.isEmpty() ||
        !extent_.isOverlapping (region.extent_))
      return;

    // General case
    combine (region, OPERATION_SUBTRACTION);

    // Update extent
    updateExtent();
  }

  

  /**
   * Subtract the specified rectangle from this region.
   * 
   * @param rectangle  Rectangle to subtract from this region.
   */
  public void subtract (Rect rectangle)
  {
    if (rectangle.isEmpty())
      return;

    subtract (new Region (rectangle));
  }

  

  /**
   * Create a new region as the subtraction of one region from another.
   * 
   * @param r1  Region to subtract from.
   * @param r2  Region to subtract.
   * @return    Subtraction of the two specified regions.
   */
  public static Region subtract (Region r1, Region r2)
  {
    Region region = new Region (r1);
    region.subtract (r2);
    return region;
  }
  

  
  /**
   * Create a new region as the subtraction of a rectangle from
   * a region.
   * 
   * @param region     Region to subtract from.
   * @param rectangle  Ractangle to subtract.
   * @return           Subtraction of the two specified regions.
   */
  public static Region subtract (Region region, Rect rectangle)
  {
    if (rectangle.isEmpty())
      return new Region (region);
    else 
      return subtract (region, new Region (rectangle));
  }


  
  /**
   * Leave the exclusive-or between this and the specified region in
   * this region. Corresponds to the XXorRegion in X11.
   * 
   * @param region  Region to xor this region with.
   */
  public void xor (Region region)
  {
    Region r = (Region) region.clone();
    r.subtract (this);
    subtract (region);
    union (r);
  }


  
  /**
   * Leave the exclusive-or between this and the specified rectangle in
   * this region.
   * 
   * @param rectangle  Rectangle to xor this region with.
   */
  public void xor (Rect rectangle)
  {
    if (rectangle.isEmpty())
      clear();
    else
      xor (new Region (rectangle));
  }


  
  /**
   * Do an exlusive-or operation between two regions and return
   * the result.
   * 
   * @param r1  First region to xor.
   * @param r2  Second region to xor.
   * @return    Result of operation.
   */
  public static Region xor (Region r1, Region r2)
  {
    Region region = new Region (r1);
    region.xor (r2);
    return region;
  }
  

  
  /**
   * Do an exlusive-or operation between a regions and a rectangle
   * and return the result.
   * 
   * @param region     Region to xor.
   * @param rectangle  Rectangle to xor with.
   * @return           Result of operation.
   */
  public static Region xor (Region region, Rect rectangle)
  {
    if (rectangle.isEmpty())
      return new Region();
    else
      return xor (region, new Region (rectangle));
  }

  

  // DEBUG
  private boolean isExtentCorrect()
  {
    int yMin = 0;
    int yMax = 0;
    int xMin = 0;
    int xMax = 0;

    if (nRectangles_ > 0) {
      yMin = rectangles_[0];
      yMax = rectangles_[1];
      xMin = rectangles_[2];
      xMax = rectangles_[3];
      for (int i = 4; i < nRectangles_ << 2;  i+= 4) {
        if (rectangles_[i+0] < yMin) yMin = rectangles_[i+0];
        if (rectangles_[i+1] > yMax) yMax = rectangles_[i+1];
        if (rectangles_[i+2] < xMin) xMin = rectangles_[i+2];
        if (rectangles_[i+3] > xMax) xMax = rectangles_[i+3];
      }
    }

    if (extent_.x1 != xMin) {
      System.out.println ("Extent error x1");
      return false;
    }
    if (extent_.x2 != xMax) {
      System.out.println ("Extent error x2");
      return false;
    }
    if (extent_.y1 != yMin) {
      System.out.println ("Extent error y1");
      return false;
    }
    if (extent_.y2 != yMax) {
      System.out.println ("Extent error y2");
      return false;
    }

    return true;
  }

  

  // DEBUG
  private boolean isCoalesced()
  {
    if (nRectangles_ < 2) return true;

    int rEnd = nRectangles_ << 2;
    
    int thisBand = 0;
    while (thisBand != rEnd) {
      // Find start of next band
      int nextBand = thisBand;
      while (nextBand != rEnd &&
             rectangles_[nextBand] == rectangles_[thisBand])
        nextBand += 4;
      if (nextBand == rEnd) return true;

      // Now we have two consecutive bands. See if they touch.
      if (rectangles_[thisBand+1] == rectangles_[nextBand+1]) {

        // Check the x values
        int thisY = rectangles_[thisBand];
        int nextY = rectangles_[nextBand];
        int i = thisBand;
        int j = nextBand;

        while (j != rEnd &&
               rectangles_[i] == thisY && rectangles_[j] == nextY) {
          if (rectangles_[i+2] != rectangles_[j+2] ||
              rectangles_[i+3] != rectangles_[j+3])
            break;

          i += 4;
          j += 4;
        }

        if (rectangles_[i] != thisY && (rectangles_[j] != nextY || j == rEnd))
          System.out.println ("Coalesce error at Y=" + thisY);
      }
      
      thisBand = nextBand;
    }

    return true;
  }
  
  
  
  // DEBUG
  private boolean isConsistent()
  {
    boolean isExtentCorrect = isExtentCorrect();
    if (!isExtentCorrect) return false;

    if (nRectangles_ == 0) return true;

    for (int i = 0; i < nRectangles_; i += 4) {
      int y1 = rectangles_[i+0];
      int y2 = rectangles_[i+1];      
      int x1 = rectangles_[i+2];
      int x2 = rectangles_[i+3];

      if (y2 <= y1) {
        System.out.println ("Rectangle error y2 > y1");
        return false;
      }
      if (x2 <= x1) {
        System.out.println ("Rectangle error x2 > x1");
        return false;
      }

      if (i+4 < nRectangles_) {
        int y1next = rectangles_[i+4];
        int y2next = rectangles_[i+5];      
        int x1next = rectangles_[i+6];
        int x2next = rectangles_[i+7];

        if (y1next < y1) {
          System.out.println ("Band alignment top error");
          return false;
        }
        
        if (y1next == y1) {
          if (y2next != y2) {
            System.out.println ("Band alignment bottom error");
            return false;
          }
          if (x1next < x2)  {
            System.out.println ("X bands intersect error");
            return false;
          }
          if (x1next == x2) {
            System.out.println ("X bands touch error");
            return false;
          }
        }
      }
    }

    if (!isCoalesced())
      return false;
    
    return true;
  }


  
  // DEBUG
  private void print()
  {
    System.out.println ("-------------------------------");
    System.out.println (extent_);
    System.out.println ("nRectangles = " + nRectangles_);
    for (int i = 0; i < nRectangles_; i++) {
      System.out.print   ("y1=" + rectangles_[i*4 + 0] + ", ");
      System.out.print   ("y2=" + rectangles_[i*4 + 1] + ", ");
      System.out.print   ("x1=" + rectangles_[i*4 + 2] + ", ");
      System.out.println ("x2=" + rectangles_[i*4 + 3]);                  
    }
  }

  
  // DEBUG
  private void printRects()
  {
    for (int i = 0; i < nRectangles_ << 2; i++) {
      if (i % 4 == 0 && i != 0) System.out.print ("  ");
      System.out.print (rectangles_[i]);
      if ((i+1) % 4 != 0) System.out.print (',');      
    }
    System.out.println ();
  }
}
