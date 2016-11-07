package no.geosoft.cc.graphics;



import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import no.geosoft.cc.geometry.Box;
import no.geosoft.cc.geometry.Geometry;
import no.geosoft.cc.geometry.Rect;
import no.geosoft.cc.geometry.Region;



/**
 * Class for holding a polyline. <tt>GSegment</tt>s are contained by
 * <tt>GObjects</tt>. They can have its own rendering style (<tt>GStyle</tt>)
 * or inherit style from its parent <tt>GObject</tt> if not specified.
 * <p>
 * Example usage:
 *
 * <pre>
 *    public class Box extends GObject
 *    {
 *       private double    x0_, y0_, width_, height_;
 *       private GSegment  border_;
 *
 *       public Box (double x0, double y0, double width, double height)
 *       {
 *          // Store the abstract representation of the box
 *          x0_     = x0;
 *          y0_     = y0;
 *          width_  = width;
 *          height_ = height;
 *
 *          // Prepare the graphics representation of the box
 *          border_ = new GSegment();
 *          addSegment (border_);
 *       }
 *
 *       public void draw()
 *       {
 *          // Complete the graphics representation of the box
 *          double[] xy = new double {x0_,          y0_,
 *                                    x0_ + width_, y0_,
 *                                    x0_ + width_, y0_ + height_,
 *                                    x0_,          y0_ + height_,
 *                                    x0_,          y0}
 *          border_.setGeometry (xy);
 *       }
 *    }
 * </pre>
 *
 * A typical <tt>GObject</tt> will have many <tt>GSegment</tt>s and
 * sub-<tt>GObject</tt>s. Some of these can be created in the constructor
 * while others may need to be created within the draw method as they depend
 * ont external factors such as zoom etc.
 * <p>
 * For efficiency, <b>G</b> does not store world coordinates internally but
 * converts these to device coordinates in the rendering step. It is
 * therfore essential that geometry is provided in the <tt>draw()</tt>
 * method which is called by <b>G</b> on retransformations
 * (zoom/resize etc.).
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GSegment
  implements GStyleListener
{
  private GObject      owner_;       // Owner
  private int          x_[], y_[];
  private GImage       vertexImage_;
  private Rect         rectangle_;   // Bounding box
  private Object       userData_;    // Whatever app assoc with graphics
  private GStyle       style_;       // As applied to this object
  private GStyle       actualStyle_; // Adjusted for owner inherits
  private boolean      isVisible_;   // Due to position not vis. setting
  private List         texts_;       // of GText
  private Collection   components_;  // of GComponent
  private Collection   images_;      // of GImage


  
  /**
   * Create a GSegment.
   */
  public GSegment()
  {
    owner_       = null;
    x_           = null;
    y_           = null;
    rectangle_   = null;
    texts_       = null;
    images_      = null;
    vertexImage_ = null;
    components_  = null;
    isVisible_   = false;

    style_       = null;
    actualStyle_ = new GStyle();
  }


  
  /**
   * Return the owner GObject of this GSegment. If the GSegment has not been 
   * added to a GObject, owner is null.
   *
   * @return  GObject owner of this GSegment, or null if not attacted to one.
   */
  public GObject getOwner()
  {
    return owner_;
  }

  

  /**
   * Set the owner of this GSegment.
   * 
   * @param owner  New owner of this GSegment.
   */
  void setOwner (GObject owner)
  {
    owner_ = owner;
    updateContext();
  }


  
  /**
   * Convenience method to get the scene of the graphics hierarchy of this
   * GSegment.
   * 
   * @return  Scene of the graphics hierarchy of this GSegment (or null if
   *          it is somehow not attached to a scene).
   */
  GScene getScene()
  {
    return owner_ == null ? null : owner_.getScene();
  }
  

  
  /**
   * Set user data of this GSegment. 
   * 
   * @param userData  User data of this GSegment.
   */
  public void setUserData (Object userData)
  {
    userData_ = userData;
  }
  

  
  /**
   * Return user data of this GSegment.
   * 
   * @return  User data of this GSegment.
   */
  public Object getUserData()
  {
    return userData_;
  }


  
  /**
   * Return device X coordinates of this segment.
   * 
   * @return  Device X coordinates of this segment.
   */
  public int[] getX()
  {
    return x_;
  }


  
  /**
   * Return device X coordinates of this GSegment.
   * 
   * @return  Device X coordinates of this segment.
   */
  public int[] getY()
  {
    return y_;
  }



  /**
   * Return number of points in the polyline of this GSegment.
   * 
   * @return  Number of points in this GSegment. May be 0 if the GSegment
   *          is empty.
   */
  public int getNPoints()
  {
    return x_ == null ? 0 : x_.length;
  }
  

  
  /**
   * Return rectangle bounding box of this GSegment. Covers GSegment geometry
   * only, not associated annotations, images or AWT components.
   * 
   * @return  Rectangle bounding box of the geometry of this GSegment.
   */
  Rect getRectangle()
  {
    return rectangle_;
  }


  
  /**
   * Return region of this GSegment including associated annotations,
   * images and AWT components.
   * 
   * @return  Region of this GSegment includings its sub components.
   */
  Region getRegion()
  {
    Region region = new Region();

    // First add geometry part
    if (rectangle_ != null)
      region.union (rectangle_);

    // Add extent of all texts
    if (texts_ != null) {
      for (Iterator i = texts_.iterator(); i.hasNext(); ) {
        GText text = (GText) i.next();
        region.union (text.getRectangle());
      }
    }

    // Add extent of all images
    if (images_ != null) {
      for (Iterator i = images_.iterator(); i.hasNext(); ) {
        GImage image = (GImage) i.next();
        region.union (image.getRectangle());
      }
    }

    // Add extent of all AWT components
    if (images_ != null) {
      for (Iterator i = images_.iterator(); i.hasNext(); ) {
        GComponent component = (GComponent) i.next();
        region.union (component.getRectangle());
      }
    }

    return region;
  }
  

  
  /**
   * Return the X center of the geometry of this GSegment.
   * 
   * @return  X center of the geometry of this GSegment.
   */
  int getCenterX()
  {
    return rectangle_ == null ? 0 : rectangle_.getCenterX();
  }


  
  /**
   * Return the Y center of the geometry of this GSegment.
   * 
   * @return  Y center of the geometry of this GSegment.
   */
  int getCenterY()
  {
    return rectangle_ == null ? 0 : rectangle_.getCenterY();
  }

  
  
  /**
   * This method is called in a response to updated geometry, new
   * parent or new style settings.
   */
  private void updateContext()
  {
    isVisible_ = false; // Until otherwise proved

    // Nothing to update if we are not in the tree
    if (owner_ == null)
      return;

    // Flag owner region as invalid
    owner_.flagRegionValid (false);

    // Not more we can do if we're not in the scene
    if (owner_.getScene() == null)
      return;

    // Check if we're outside the scene
    if (rectangle_ != null)
      isVisible_ = owner_.getScene().getRegion().isIntersecting (rectangle_);

    // Update GWindow damage area
    updateDamage();

    // If segment has text, annotation must be updated
    if (texts_ != null)
      owner_.getScene().setAnnotationValid (false);

    // If segment has images, their positions must be updated
    if (images_ != null)
      owner_.getScene().computePositions (images_);
  }
  

  
  /**
   * Flag the geometry part of this GSegment as damaged.
   */
  void updateDamage()
  {
    if (isVisible_            &&
        rectangle_ != null    &&
        !rectangle_.isEmpty() &&
        owner_ != null        &&
        owner_.getWindow() != null)
      owner_.getWindow().updateDamageArea (rectangle_);
  }

  

  /**
   * Compute the rectangle bounding box of this GSegment.
   */
  private void computeRectangle()
  {
    // Actual rectangle depends on line width
    int lineWidth = actualStyle_.getLineWidth() - 1;

    if (x_ != null) {
      if (rectangle_ == null) rectangle_ = new Rect();
      
      rectangle_.set (x_, y_);
      rectangle_.expand (lineWidth + 1, lineWidth + 1);
    }
  }

  
  
  /**
   * Set single point device coordinate geometry.
   * 
   * @param x  X coordinate.
   * @param y  Y coordinate.   
   */
  public void setGeometry (int x, int y)
  {
    setGeometry (new int[] {x}, new int[] {y});
  }


  
  /**
   * Set two point (line) device coordinate geometry.
   * 
   * @param x0  X coordinate of first end point.
   * @param y0  Y coordinate of first end point.   
   * @param x1  X coordinate of second end point.
   * @param y1  Y coordinate of second end point.   
   */
  public void setGeometry (int x0, int y0, int x1, int y1)
  {
    setGeometry (new int[] {x0, x1}, new int[] {y0, y1});
  }
  

  
  /**
   * Set polyline device coordinate geometry.
   * 
   * @param x  X coordinates.
   * @param y  Y coordinates.   
   */
  public void setGeometry (int[] x, int[] y)
  {
    // Mark old area as damaged
    updateDamage();

    // Update geometry
    if (x == null) {
      x_ = null;
      y_ = null;
      rectangle_ = null;
    }
    else {
      int  nPoints = x.length;

      // Reallocate
      if (x_ == null || x_.length != nPoints) {
        x_ = new int[nPoints];
        y_ = new int[nPoints];
      }
    
      for (int i = 0; i < nPoints; i++) {
        x_[i] = x[i];
        y_[i] = y[i];      
      }

      // Update bounding box
      computeRectangle();
    }

    updateContext();
  }


  
  /**
   * Set polyline device coordinate geometry.
   * 
   * @param xy  Polyline geometry [x,y,x,y,...]. null can be specified
   *            to indicate that the present geometry should be removed.
   */
  public void setGeometry (int[] xy)
  {
    // Mark old area as damaged
    updateDamage();

    // Update geometry
    if (xy == null) {
      x_ = null;
      y_ = null;
      rectangle_ = null;
    }
    else {
      int  nPoints = xy.length / 2;

      // Reallocate
      if (x_ == null || x_.length != nPoints) {
        x_ = new int[nPoints];
        y_ = new int[nPoints];
      }
    
      for (int i = 0; i < nPoints; i++) {
        x_[i] = xy[i*2 + 0];
        y_[i] = xy[i*2 + 1];      
      }

      // Update bounding box
      computeRectangle();
    }

    updateContext();
  }



  /**
   * Set single point world coordinate geometry.
   * 
   * @param x  X coordinate.
   * @param y  Y coordinate.
   * @param z  Z coordinate.   
   */
  public void setGeometry (double x, double y, double z)
  {
    setGeometry (new double[] {x}, new double[] {y}, new double[] {z});
  }


  
  /**
   * Set single point world coordinate geometry. Ignore Z coordinate
   * (set to 0.0).
   * 
   * @param x  X coordinate.
   * @param y  Y coordinate.   
   */
  public void setGeometry (double x, double y)
  {
    setGeometry (x, y, 0.0);
  }


  
  /**
   * Set two point (line) world coordinate geometry.
   * 
   * @param x0  X coordinate of first end point.
   * @param y0  Y coordinate of first end point.
   * @param z0  Z coordinate of first end point.      
   * @param x1  X coordinate of second end point.
   * @param y1  Y coordinate of second end point.   
   * @param z1  Z coordinate of second end point.      
   */
  public void setGeometry (double x0, double y0, double z0,
                           double x1, double y1, double z1)
  {
    setGeometry (new double[] {x0, x1},
                 new double[] {y0, y1},
                 new double[] {z0, z1});
  }

  
  
  /**
   * Set two point (line) world coordinate geometry. Ignore Z coordinate
   * (set to 0.0).
   * 
   * @param x0  X coordinate of first end point.
   * @param y0  Y coordinate of first end point.
   * @param x1  X coordinate of second end point.
   * @param y1  Y coordinate of second end point.   
   */
  public void setGeometry (double x0, double y0, double x1, double y1)
  {
    setGeometry (new double[] {x0, x1},
                 new double[] {y0, y1},
                 new double[] {0.0, 0.0});
  }
  
  

  /**
   * Set polyline world coordinate geometry.
   * TODO: Look at the implementation   
   * 
   * @param wx  X coordinates.
   * @param wy  Y coordinates.
   * @param wz  Z coordinates. May be null if Z is to be ignored.
   */
  public void setGeometry (double x[], double y[], double z[])
  {
    GTransformer transformer = owner_.getScene().getTransformer();

    int  nPoints = x.length;

    double[] world  = new double[3];
    int[]    device = new int[2];
    int[]    devx   = new int[nPoints];
    int[]    devy   = new int[nPoints];    
    
    for (int i = 0; i < x.length; i++) {
      world[0] = x[i];
      world[1] = y[i];
      world[2] = z == null ? 0.0 : z[i];      

      device = transformer.worldToDevice (world);

      devx[i] = device[0];
      devy[i] = device[1];
    }

    setGeometry (devx, devy);
  }


  
  /**
   * Set polyline world coordinate geometry. Ignore Z coordinate
   * (interpret all as 0.0).
   * 
   * @param x  X coordinates.
   * @param y  Y coordinates.
   */
  public void setGeometry (double[] x, double[] y)
  {
    setGeometry (x, y, null);
  }


  
  /**
   * Set polyline world coordinate geometry.
   * 
   * @param xyz  Polyline geometry [x,y,z,x,y,z,...].
   */
  public void setGeometry (double[] xyz)
  {
    GTransformer transformer = owner_.getScene().getTransformer();

    int  nPoints = xyz.length / 3;

    int[] devxy = new int[nPoints * 2];

    double[] world  = new double[3];
    int[]    device = new int[2];

    int wIndex = 0;
    int dIndex = 0;
    
    for (int i = 0; i < nPoints; i++) {
      world[0] = xyz[wIndex + 0];
      world[1] = xyz[wIndex + 1];
      world[2] = xyz[wIndex + 2];

      device = transformer.worldToDevice (world);

      devxy[dIndex + 0] = device[0];
      devxy[dIndex + 1] = device[1];

      wIndex += 3;
      dIndex += 2;
    }

    setGeometry (devxy);
  }


  
  /**
   * Set polyline world coordinate geometry. Ignore Z coordinate
   * (set to 0.0).
   * 
   * @param xy  Polyline geometry [x,y,x,y,...].
   */
  public void setGeometryXy (double[] xy)
  {
    GTransformer transformer = owner_.getScene().getTransformer();

    int  nPoints = xy.length / 2;

    int[] devxy = new int[nPoints * 2];

    double[] world  = new double[3];
    int[]    device = new int[2];

    int wIndex = 0;
    int dIndex = 0;
    
    for (int i = 0; i < nPoints; i++) {
      world[0] = xy[wIndex + 0];
      world[1] = xy[wIndex + 1];
      world[2] = 0.0;

      device = transformer.worldToDevice (world);

      devxy[dIndex + 0] = device[0];
      devxy[dIndex + 1] = device[1];

      wIndex += 2;
      dIndex += 2;
    }

    setGeometry (devxy);
  }


  
  /**
   * Translate this segment in device.
   * 
   * @param dx  Translation in x direction.
   * @param dy  Translation in Y direction.
   */
  public void translate (int dx, int dy)
  {
    if (x_ == null) return;
    
    int[] newX = new int[x_.length];
    int[] newY = new int[x_.length];    

    for (int i = 0; i < x_.length; i++) {
      newX[i] = x_[i] + dx;
      newY[i] = y_[i] + dy;      
    }

    setGeometry (newX, newY);
  }

  

  /**
   * Set new style for this segment. Style elements not explicitly
   * set within this GStyle object are inherited from parent
   * objects. Default style is null, i.e. all style elements are
   * inherited from parent.
   * 
   * @param style  Style for this segment (or null if the intent is to
   *               unset the current style).
   */
  public void setStyle (GStyle style)
  {
    if (style_ != null)
      style_.removeListener (this);

    style_ = style;

    if (style_ != null)
      style_.addListener (this);
    
    updateStyle();
  }
  

  
  /**
   * Return style for this segment. This is the style set by setStyle()
   * and not necesserily the style as it appears on screen as unset
   * style elements are inherited from parents.
   * 
   * @return  Style of this GSegment as specified with setStyle(), (or
   *          null if no style has been provided).
   */
  public GStyle getStyle()
  {
    return style_;
  }

  
  
  /**
   * These are the actual style used for this GSegment when
   * inheritance for unset values are resolved.
   * TODO: Make this public?
   * 
   * @return  Actual style for this segment.
   */
  GStyle getActualStyle()
  {
    return actualStyle_;
  }


  
  /**
   * Resolve unset values in segment style.
   */
  void updateStyle()
  {
    // Invalidate all style
    actualStyle_ = new GStyle();

    // Update with owner style
    if (owner_ != null)
      actualStyle_.update (owner_.getActualStyle());

    // Update (and possibly override) with present style
    if (style_ != null)
      actualStyle_.update (style_);

    // Update children object style
    if (texts_ != null) {
      for (Iterator i = texts_.iterator(); i.hasNext(); ) {
        GText text = (GText) i.next();
        text.updateStyle();
      }
    }

    // TODO: This might not be necessary for all style changes
    computeRectangle();
    updateContext();
  }



  /**
   * Find region of a set of positionals.
   * 
   * @param positionals  Positionals to find region of.
   * @return             Region of specified positionals.
   */
  private Region findRegion (Collection positionals)
  {
    Region region = new Region();

    for (Iterator i = positionals.iterator(); i.hasNext(); ) {
      GPositional positional = (GPositional) i.next();
      if (positional.isVisible())
        region.union (positional.getRectangle());
    }

    return region;
  }

  

  /**
   * Add a text element to this segment.
   * <p>
   * Text elements without line position hint will be associated with
   * the n'th segment coordinate according to the number of texts added.
   * 
   * @param text  Text element to add.
   */
  public void addText (GText text)
  {
    // Create if first text
    if (texts_ == null)
      texts_ = new ArrayList();

    // Add to list
    texts_.add (text);
    text.setSegment (this);
    
    // Flag owner region as invalid and annotation too
    if (owner_ != null) {
      owner_.flagRegionValid (false);
      if (owner_.getScene() != null)
        owner_.getScene().setAnnotationValid (false);
    }
  }


  
  /**
   * Set text element of this segment. Replaces all current
   * text elements of this segment.
   * 
   * @param text  Text element to set.
   */
  public void setText (GText text)
  {
    removeText();
    addText (text);
  }


  
  /**
   * Return all text elements of this segment.
   * 
   * @return  All text elements of this segment (or null if none).
   */
  public List getTexts()
  {
    return texts_;
  }



  /**
   * Return the first text element of this segment. Convenient when
   * caller knows that there are exactly one text element.
   * 
   * @return  First text elements of this segment (or null if none).
   */
  public GText getText()
  {
    return texts_ != null ? (GText) texts_.iterator().next() : null;
  }
  

  
  /**
   * Remove all text elements set on this segment.
   */
  public void removeText()
  {
    // Update damage area
    if (owner_ != null && owner_.getWindow() != null && texts_ != null) {
      Region damage = findRegion (texts_);
      owner_.getWindow().updateDamageArea (damage);
    }
    
    // Nullify texts
    texts_ = null;
  }
  

  
  /**
   * Add an image to this segment.
   * 
   * @param image  Image to add.
   */
  public void addImage (GImage image)
  {
    // Create if first time
    if (images_ == null)
      images_ = new ArrayList();

    // Add to list
    images_.add (image);
    image.setSegment (this);
    
    // Flag owner region as invalid
    if (owner_ != null)
      owner_.flagRegionValid (false);
  }


  
  /**
   * Set image of this segment. All current images are removed.
   * 
   * @param image  Image to set.
   */
  public void setImage (GImage image)
  {
    removeImages();
    addImage (image);
  }


  
  /**
   * Return all images associated with this segment.
   * 
   * @return   All images associated with this segment.
   */
  public Collection getImages()
  {
    return images_;
  }

  

  /**
   * Remove all images from this GSegment.
   */
  public void removeImages()
  {
    // Update damage area
    if (owner_ != null && owner_.getWindow() != null && images_ != null) {
      Region damage = findRegion (images_);
      owner_.getWindow().updateDamageArea (damage);
    }
    
    // Nullify images
    images_ = null;
  }
  

  
  /**
   * Set image to associate with every vertex of this GSegment.
   * 
   * @param image  Image to decorate every vertex of this
   *               polyline (or null to turn off this feature).
   */
  public void setVertexImage (GImage image)
  {
    vertexImage_ = image;
  }



  /**
   * Return the image that is to be associated with all vertices
   * of this GSegment. Return null if none is specified.
   * 
   * @return  Image that decorates every vertex of this
   *          GSegment (or null if not specified).
   */
  public GImage getVertexImage()
  {
    return vertexImage_;
  }
  

  
  /**
   * Add a AWT component to this segment.
   * 
   * @param component  Component to add.
   */
  public void addComponent (GComponent component)
  {
    // Create if first time
    if (components_ == null)
      components_ = new ArrayList();

    component.setSegment (this);
    
    // Add to list
    components_.add (component);
  }
  


  /**
   * Set component of this segment. All current components are removed.
   * 
   * @param component  Component to set.
   */
  public void setComponent (GComponent component)
  {
    removeComponents();
    addComponent (component);
  }
  

  
  /**
   * Return all AWT components of this segment.
   * 
   * @return  All components of this segment.
   */
  public Collection getComponents()
  {
    return components_;
  }

  

  /**
   * Remove all AWT components from this GSegment.
   * 
   * @param component
   */
  public void removeComponents()
  {
    // Update damage area
    if (owner_ != null && owner_.getWindow() != null && components_ != null) {
      Region damage = findRegion (components_);
      owner_.getWindow().updateDamageArea (damage);
    }
    
    // Nullify images
    components_ = null;
  }
  

  
  /**
   * Check if this segment is visible. This is visibility due to
   * geometry position relative to viewport, <em>not</em> due to
   * GObject visibility settings.
   *
   * @return  True if segment geometry is visible, false otherwise. 
   */
  boolean isVisible()
  {
    return isVisible_;
  }

  

  /**
   * Check if this segment is filled. The <em>fill</em> property depends
   * on the style settings of the segment and it is used to determine
   * segment intersections.
   * 
   * @return  True of the segment is filled, false otherwise.
   */
  boolean isFilled()
  {
    return actualStyle_.isDefiningFill();
  }


  
  /**
   * Check if the geometry of this GSegment is inside the specified
   * rectangle.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.   
   * @return    True if the geometry of this GSegment is completely
   *            inside the specified rectangle, false otherwise. If
   *            this GSegment has no geometry, false is returned.
   */
  boolean isInsideRectangle (int x0, int y0, int x1, int y1)
  {
    if (rectangle_ == null) return false;
    Box box = new Box (rectangle_);
    return box.isInsideOf (new Box (x0, y0, x1, y1));
  }
  
  

  /**
   * Check if the geometry of this GSegment intersects the specified
   * rectangle.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.   
   * @return    True if the geometry of this GSegment intersects
   *            the specified rectangle, false otherwise. If
   *            the GSegment has no geometry, false is returned.
   */
  boolean isIntersectingRectangle (int x0, int y0, int x1, int y1)
  {
    if (x_ == null) return false;
    
    return (isFilled() &&
            Geometry.isPolygonIntersectingRectangle (x_, y_, x0, y0, x1, y1)) ||
           (!isFilled() &&
            Geometry.isPolylineIntersectingRectangle (x_, y_, x0, y0, x1, y1));
  }


  
  /**
   * Check if this GSegment intersects the specified point.
   * 
   * @param x  X coordinate of point.
   * @param y  Y coordinate of point.
   * @return   True if this GSegment intersects the specified point,
   *           false otherwise. If the GSegment has no geometry,
   *           false is returned.
   */
  boolean isIntersectingPoint (int x, int y)
  {
    if (x_ == null) return false;
    
    return (isFilled() &&
            Geometry.isPointInsidePolygon (x_, y_, x, y)) ||
           (!isFilled() &&
            Geometry.isPolylineIntersectingRectangle (x_, y_,
                                                      x-1, y-1, x+1, y+1));
  }


  
  /**
   * Called when the style of this object is changed.
   *
   * @param style  Style that has changed.
   */
  public void styleChanged (GStyle style)
  {
    updateStyle();
  }
}
