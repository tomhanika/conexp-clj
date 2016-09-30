package no.geosoft.cc.graphics;



import java.util.Collection;
import java.util.Iterator;

import no.geosoft.cc.geometry.Geometry;
import no.geosoft.cc.geometry.Region;
import no.geosoft.cc.geometry.Rect;



/**
 * Layout manager for positionals. Positionals are object attached
 * to graphic elements by means of rendering hints.
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
class GAnnotator
{
  private static final int     MAX_ATTEMTS = 300;
  private static final double  DLENGTH     = 8.0;
    
  private final GScene  scene_;   
  private final Region  region_;  // Annotation region

  // Precomputed viewport numbers for speed
  private int     vx0_, vy0_;
  private int     vx1_, vy1_;  
  private int     vx2_, vy2_;
  private int     vx3_, vy3_;  

  // Working variable during annotation
  private double  distance_;

  

  /**
   * Create annotator for the specified scene. The annotator use the
   * scene viewport as its canvas, and position all positional elements
   * of the scene inside this region.
   * 
   * @param scene  Scene of this annotator.
   */
  GAnnotator (GScene scene)
  {
    region_  = new Region();
    scene_ = scene;
  }


  
  /**
   * Reset this annotator. The annotator must be reset when the scene
   * viewport is changed.
   */
  void reset()
  {
    region_.set (scene_.getRegion());

    // Precompute some viewport numbers
    GViewport viewport = scene_.getViewport();
    int p0[] = viewport.getP0();  // x,y
    int p1[] = viewport.getP1();  // x,y
    int p2[] = viewport.getP2();  // x,y
    
    vx0_ = p0[0];
    vy0_ = p0[1];
    
    vx1_ = p1[0];
    vy1_ = p1[1];
    
    vx2_ = p2[0];
    vy2_ = p2[1];
    
    vx3_ = vx2_ + vx1_ - vx0_;
    vy3_ = vy1_ + vy2_ - vy0_;     
  }



  /**
   * Compute the x and y position of the specified positional elements
   * and set isVisible flag in case they are outside the screen.
   * After this computation is done on all nodes, the positionals can be
   * rendered directly by the GCanvas object.
   * 
   * @param positionals  Elements to position (of GPositional).
   */
  void computePositions (Collection positionals)
  {
    // Return here if nothing to position
    if (positionals == null) return;

    int objectNo = 0;
    for (Iterator i = positionals.iterator(); i.hasNext(); ) {
      GPositional positional = (GPositional) i.next();
      GSegment segment = positional.getSegment();
      int positionHint = positional.getPositionHint();
      
      // Save a copy of the old rectangle
      Rect oldRectangle = new Rect (positional.getRectangle());
      boolean wasVisible = positional.isVisible();
      
      if (segment.isVisible()) {
        int x[] = segment.getX();
        int y[] = segment.getY();
        int x0  = segment.getCenterX();
        int y0  = segment.getCenterY();
        
        // Compute positional size
        positional.computeSize();
        if (positional.getRectangle().isEmpty()) {
          positional.setVisible (false);
          continue;
        }
        
        // Find the preferred annotation according to line position hints
        int pointNo = findPreferredPosition (positional, x, y, x0, y0,
                                             objectNo);

        // If the current point is outside the viewport move it inside 
        if ((positionHint & GPosition.DYNAMIC) != 0 &&
            (positionHint & GPosition.MIDDLE) == 0) {
          positional.setVisible (true);

          distance_ = 0.0;
          if (!scene_.getRegion().isInside (positional.getRectangle().x,
                                            positional.getRectangle().y))
            pointNo = moveInsideWindow (positional, x, y, pointNo);
        }
        else {
          positional.setVisible (scene_.getRegion().
                                 isInside (positional.getRectangle().x,
                                           positional.getRectangle().y));
        }
        
        if (positional.isVisible()) {
          
          // Tune the positional according to point position hint
          adjustPosition (positional);

          // For a static positional we're now done
          if ((positionHint & GPosition.STATIC) != 0 |
              (positionHint & GPosition.MIDDLE) != 0)
            positional.setVisible (region_.isIntersecting (positional.
                                                           getRectangle()));
          else { // GPosition.DYNAMIC and don't allow overlapping
            if (!positional.isAllowingOverlaps() &&
                !region_.isInside (positional.getRectangle()))
              findNonOverlappingPosition (positional, x, y, pointNo);
          }
        }
      }

      // If positional was moved update damage
      if (!oldRectangle.equals (positional.getRectangle())) {
        if (wasVisible)
          scene_.getWindow().updateDamageArea (oldRectangle);
        if (positional.isVisible())
          scene_.getWindow().updateDamageArea (positional.getRectangle());

        if (wasVisible || positional.isVisible())
          segment.getOwner().flagRegionValid (false);
      }

      // If a valid position was found update the annotation region
      if (positional.isVisible() &&
          (positional.getPositionHint() & GPosition.STATIC) == 0 &&
          !positional.isAllowingOverlaps())
        region_.subtract (positional.getRectangle());

      if (!positional.isLinePositional()) objectNo++;
    }
  }


  
  /**
   * Find the preferred position of a positional element. This is the
   * position as dictaed by its rendering hint and before any adjustments
   * are done.
   * 
   * @param positional  Positional element to position.
   * @param x           X values of polyline to position according to.
   * @param y           Y values of polyline to position according to.   
   * @param x0          X coordinate of center of gravity of polyline
   *                    (used with rendering hint MIDDLE).
   * @param y0          Y coordinate of center of gravity of polyline
   *                    (used with rendering hint MIDDLE).
   * @param pointNo     Index number of this positional on this polyline
   *                    (used when no line rendering hints are given).
   * @return            Index in x,y of point to attach this positional to.
   */
  private int findPreferredPosition (GPositional positional,
                                     int x[], int y[], int x0, int y0,
                                     int pointNo)
  {
    int positionHint = positional.getPositionHint();
    int nPoints  = x.length;

    if (positional.isLinePositional()) {
      
      // Find the point on the segment that the positional should attach to
      if ((positionHint & GPosition.LAST) != 0) {
        pointNo = nPoints - 1;
      }
      else if ((positionHint & GPosition.MIDDLE) != 0) {
        positional.getRectangle().x = x0;
        positional.getRectangle().y = y0;
        return -1;
      }
      else if ((positionHint & GPosition.TOP) != 0) {
        int yMin = y[0];
        for (int i = 0; i < nPoints; i++) {
          if (y[i] < yMin) {
            yMin = y[i];
            pointNo = i;
          }
        }
      }
      else if ((positionHint & GPosition.BOTTOM) != 0) {
        int yMax = y[0];
        for (int i = 0; i < nPoints; i++) {
          if (y[i] > yMax) {
            yMax = y[i];
            pointNo = i;
          }
        }
      }
      else if ((positionHint & GPosition.LEFT) != 0) {
        int xMin = x[0];
        for (int i = 0; i < nPoints; i++) {
          if (x[i] < xMin) {
            xMin = x[i];
            pointNo = i;
          }
        }
      }
      else if ((positionHint & GPosition.RIGHT) != 0) {
        int xMax = x[0];
        for (int i = 0; i < nPoints; i++) {
          if (x[i] > xMax) {
            xMax = x[i];
            pointNo = i;
          }
        }
      }
    }
    
    positional.getRectangle().x = x[pointNo];
    positional.getRectangle().y = y[pointNo];

    return pointNo;
  }

  

  /**
   * Adjsut the position of the specified positional according to
   * point position hint.
   * 
   * @param positional  Positionla to adjust.
   */
  private void adjustPosition (GPositional positional)
  {
    int positionHint = positional.getPositionHint();
    int x            = positional.getRectangle().x;
    int y            = positional.getRectangle().y;
    int width        = positional.getRectangle().width;
    int height       = positional.getRectangle().height;

    int halfWidth  = (int) Math.ceil ((double) width  / 2.0);
    int halfHeight = (int) Math.ceil ((double) height / 2.0);    
                    

    if ((positionHint & GPosition.NORTHWEST) != 0) {
      x -= width  + positional.getMargin();
      y -= height + positional.getMargin();
    }
    else if ((positionHint & GPosition.NORTHEAST) != 0) {
      x += positional.getMargin();
      y -= height + positional.getMargin();
    }
    else if ((positionHint & GPosition.SOUTHWEST) != 0) {
      x -= width  + positional.getMargin();
      y += positional.getMargin();
    }
    else if ((positionHint & GPosition.SOUTHEAST) != 0) {
      x += positional.getMargin();
      y += positional.getMargin();
    }
    else if ((positionHint & GPosition.NORTH) != 0) {
      x -= halfWidth;
      y -= height + positional.getMargin();
    }
    else if ((positionHint & GPosition.WEST) != 0) {
      x -= width  + positional.getMargin();
      y -= halfHeight;
    }
    else if ((positionHint & GPosition.EAST) != 0) {
      x += positional.getMargin();
      y -= halfHeight;
    }
    else if ((positionHint & GPosition.SOUTH) != 0) {
      x -= halfWidth;
      y += positional.getMargin();
    }
    else { // GPosition.CENTER
      x -= halfWidth;
      y -= halfHeight;
    }

    positional.getRectangle().x = x;
    positional.getRectangle().y = y;
  }

  

  /**
   * Adjust position of the specified positional so that it doesn't overlap
   * any other positionals. The algorithm is to start on polyline at current
   * location, move back and forth on the line further and further away from
   * the origin until a free location is found.
   * 
   * @param positional  Positional to adjust.
   * @param x           X values of polyline to position along.
   * @param y           Y values of polyline to position along.   
   * @param pointNo     Index in polyline where positional is initially
   *                    attached.
   */
  private void findNonOverlappingPosition (GPositional positional,
                                           int x[], int y[], int pointNo)
  {
    // Find current position along polyline
    int x0, y0;
    
    if (distance_ == 0.0) {
      x0 = x[pointNo];
      y0 = y[pointNo];
    }
    else {
      double legLength = Geometry.length (x[pointNo], y[pointNo],
                                          x[pointNo+1], y[pointNo+1]);
      double fraction = (double) distance_ / (double) legLength;
      
      x0 = (int) Math.round (x[pointNo] +
                             fraction * (x[pointNo+1] - x[pointNo]));
      y0 = (int) Math.round (y[pointNo] +
                             fraction * (y[pointNo+1] - y[pointNo]));
    }
    
    // Find total length into the polyline
    double totalLength = 0.0;
    for (int i = 0; i < pointNo; i++)
      totalLength += Geometry.length (x[i], y[i], x[i+1], y[i+1]);
    totalLength += distance_;

    // Then loop alternating back and forth from x0,y0 and check position
    boolean isDoneLeft  = false;
    boolean isDoneRight = false;
    int     nAttempts = 0; // We cannot try for ever
    boolean isDone = false;

    double left  = totalLength -= DLENGTH;
    double right = totalLength += DLENGTH;

    int leftPosition[]  = {x0, y0};
    int rightPosition[] = {x0, y0};
    int position[]      = new int[2];        

    Rect  leftRectangle  = new Rect (positional.getRectangle());
    Rect  rightRectangle = new Rect (positional.getRectangle());
    
    int dx, dy;
    
    while (!isDone) {
      // Check to the left
      if (!isDoneLeft) {
        isDoneLeft = !Geometry.findPolygonPosition (x, y, left, position);
        if (!isDoneLeft) {
          dx = leftPosition[0] - position[0];
          dy = leftPosition[1] - position[1];

          leftRectangle.x -= dx;
          leftRectangle.y -= dy;

          if (region_.isInside (leftRectangle)) {
            positional.getRectangle().copy (leftRectangle);
            return;
          }
        }

        leftPosition[0] = position[0];
        leftPosition[1] = position[1];        
        
        nAttempts++;
      }
      
      // Check to the right
      if (!isDoneRight) {
        isDoneRight = !Geometry.findPolygonPosition (x, y, right, position);
        if (!isDoneRight) {
          dx = rightPosition[0] - position[0];
          dy = rightPosition[1] - position[1];

          rightRectangle.x -= dx;
          rightRectangle.y -= dy;

          if (region_.isInside (rightRectangle)) {
            positional.getRectangle().copy (rightRectangle);            
            return;            
          }
        }

        rightPosition[0] = position[0];
        rightPosition[1] = position[1];        

        nAttempts++;        
      }

      left  -= DLENGTH;
      right += DLENGTH;

      isDone = (isDoneLeft && isDoneRight) || nAttempts > MAX_ATTEMTS;
    }

    positional.setVisible (false);
  }



  /**
   * Given a polyline (x[], y[]) and a position (pointNo) on this
   * polyline for the positional. This function is called because we
   * know that pointNo is outside the visible. We find the closest
   * point (along line) that is inside. If none is found the positional
   * is set invisible and no further positioning is required.
   * If a point is found the positional rectangle is set to the
   * viewport intersection point, the polyline point is returned and
   * the distance_ variable is set as the distance from the positional
   * to the returned point.
   *
   * @param positional  Positional to move inside window.
   * @param x           X values of polyline.
   * @param y           Y values of polyline.
   * @param pointNo     Index of polyline where positional is initially
   *                    attached.
   * @return            New index to attach positional to.
   */
  private int moveInsideWindow (GPositional positional,
                                int x[], int y[], int pointNo)
  {
    int intersection[] = null;
    int iPoint;
    
    // Check to the "left" of current point
    for (iPoint= pointNo - 1; iPoint >= 0; iPoint--) {
      intersection = findViewportIntersection (x[iPoint+1], y[iPoint+1],
                                               x[iPoint], y[iPoint]);
      if (intersection != null) break;
    }

    // Check the "right" of the current point
    if (intersection == null) {
      for (iPoint = pointNo; iPoint < x.length - 1; iPoint++) {
        intersection = findViewportIntersection (x[iPoint], y[iPoint],
                                                 x[iPoint+1], y[iPoint+1]);
        if (intersection != null) break;
      }
    }

    if (intersection == null)
      positional.setVisible (false);
    else {
      positional.setVisible (true);      
      positional.getRectangle().x = intersection[0];
      positional.getRectangle().y = intersection[1];      
      distance_ = Geometry.length (intersection[0], intersection[1],
                                   x[iPoint], y[iPoint]);
    }
    
    return iPoint;
  }


  
  /**
   * Find the intersection point of the specified line segment and the
   * current viewport.
   * 
   * @param x0  X coordinate of first line segment endpoint.
   * @param y0  Y coordinate of first line segment endpoint.
   * @param x1  X coordinate of second line segment endpoint.
   * @param y1  Y coordinate of second line segment endpoint.
   * @return    Coordinate of intersection (or null if the specified
   *            line segment doesn't intersect the viewport).
   */
  private int[] findViewportIntersection (int x0, int y0, int x1, int y1)
  {
    double  length[]       = new double[2];
    int     x[]            = new int[2];
    int     y[]            = new int[2];
    double  intersection[] = new double[2];
    int     result[]       = new int[2];
    int     n = 0;
    int     type;

    
    // Top viewport edge
    type = Geometry.findLineSegmentIntersection (x0, y0, x1, y1,
                                                 vx0_, vy0_, vx1_, vy1_,
                                                 intersection);
    if (type == 1l) {
      x[n] = (int) Math.round (intersection[0]);
      y[n] = (int) Math.round (intersection[1]);
      length[n] = Geometry.length (x0, y0, x[n], y[n]);
      n++;
    }
    
    // Left viewport edge
    type = Geometry.findLineSegmentIntersection (x0, y0, x1, y1,
                                                 vx0_, vy0_, vx2_, vy2_,
                                                 intersection);
    if (type == 1) {
      x[n] = (int) Math.round (intersection[0]);
      y[n] = (int) Math.round (intersection[1]);
      length[n] = Geometry.length (x0, y0, x[n], y[n]);
      n++;
    }

    // Right viewport edge
    if (n < 2) {
      type = Geometry.findLineSegmentIntersection (x0, y0, x1, y1,
                                                   vx1_, vy1_, vx3_, vy3_,
                                                   intersection);
      if (type == 1) {
        x[n] = (int) Math.round (intersection[0]);
        y[n] = (int) Math.round (intersection[1]);
        length[n] = Geometry.length (x0, y0, x[n], y[n]);
        n++;
      }
    }

    // Bottom viewport edge
    if (n < 2) {    
      type = Geometry.findLineSegmentIntersection (x0, y0, x1, y1,
                                                   vx2_, vy2_, vx3_, vy3_,
                                                   intersection);
      if (type == 1) {
        x[n] = (int) Math.round (intersection[0]);
        y[n] = (int) Math.round (intersection[1]);
        length[n] = Geometry.length (x0, y0, x[n], y[n]);
        n++;
      }
    }

    if (n == 0) return null;

    if (n == 1) {
      result[0] = x[0];
      result[1] = y[0];
    }

    // Intersects the viewport twice. Pick the closest
    else {
      if (length[0] < length[1]) {
        result[0] = x[0];
        result[1] = y[0];
      }
      else {
        result[0] = x[1];
        result[1] = y[1];
      }
    }
    
    return result;
  }


  
  /**
   * This method is used to position a positional on every vertex along
   * a polyline. The positioanl rectangle x and y values are computed
   * as delta measure, so that the positionals can later be positioned
   * by adding this delta to each vertex.
   * 
   * @param positional  Positional to compute position of.
   */
  void computeVertexPositions (GPositional positional)
  {
    positional.computeSize();

    int width  = positional.getRectangle().width;
    int height = positional.getRectangle().height;

    int x, y;

    int positionHint = positional.getPositionHint();
    
    if      ((positionHint & GPosition.NORTH)   != 0) {
      x = - (int) Math.floor (width / 2.0);
      y = - height;
    }
    else if ((positionHint & GPosition.SOUTH)     != 0) {
      x = - (int) Math.floor (width / 2.0);
      y = 0;
    }
    else if ((positionHint & GPosition.WEST)      != 0) {
      x = - width;
      y = - (int) Math.floor (height / 2.0);
    }
    else if ((positionHint & GPosition.EAST)      != 0) {
      y = - (int) Math.floor (height / 2.0);
      x = 0;
    }
    else if ((positionHint & GPosition.NORTHWEST) != 0) {
      x = - width;
      y = - height;
    }
    else if ((positionHint & GPosition.NORTHEAST) != 0) {
      x = 0;
      y = - height;      
    }
    else if ((positionHint & GPosition.SOUTHWEST) != 0) {
      x = - width;      
      y = 0;
    }
    else if ((positionHint & GPosition.SOUTHEAST) != 0) {
      x = 0;
      y = 0;
    }
    else { // CENTER (or unset implying center)
      x = - (int) Math.floor (width  / 2.0);
      y = - (int) Math.floor (height / 2.0);      
    }

    positional.getRectangle().x = x;
    positional.getRectangle().y = y;    
  }
}
