package no.geosoft.cc.graphics;



import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.Collection;

import no.geosoft.cc.geometry.Region;
import no.geosoft.cc.geometry.Rect;
import no.geosoft.cc.geometry.Box;



/**
 * Class representing a graphical object. The representation is 
 * by actual geometry (GSegments) or in terms of sub objects (or both).
 * <p>
 * Typically GObject is extended and its draw() method is overloaded:
 *
 * <pre>
 *    public class ClientGraphicsObject extends GObject
 *    {
 *        public ClientGraphicsObject()
 *        {
 *           // Prepare the graphics object here, i.e. add the
 *           // GSegments and sub-GObjects it consists of, and
 *           // set rendering style (GStyle) on the elements
 *           // and add annotations (GText) and images (GImage).
 *        }
 *
 *        public void draw()
 *        {
 *           // Set geometry for all containing GSegments.
 *        }
 *    }
 * </pre>
 *
 * A top level GObject is inserted into the scene like this:
 *
 * <pre>
 *    GScene scene = new GScene (window);
 * 
 *    GObject object = new ClientGraphicsObject();
 *    scene.add (object);
 * </pre>
 *
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GObject
  implements GStyleListener
{
  private static final int  FORWARD              = -2;
  private static final int  BACKWARD             = -3;

  public  static final int  DATA_VISIBLE         = 1;
  public  static final int  ANNOTATION_VISIBLE   = 2;
  public  static final int  SYMBOLS_VISIBLE      = 4;
  public  static final int  WIDGETS_VISIBLE      = 8;
  public  static final int  VISIBLE              = 15;

  public  static final int  DATA_INVISIBLE       = 16;
  public  static final int  ANNOTATION_INVISIBLE = 32;
  public  static final int  SYMBOLS_INVISIBLE    = 64;
  public  static final int  WIDGETS_INVISIBLE    = 128;  
  public  static final int  INVISIBLE            = 240;

  private String      name_;
  private Region      region_;         // Including all children
  private boolean     isRegionValid_;  // Has it been computed?
  private GObject     parent_;         // Immediate ancestor
  private int         visibilityMask_;    
  private List        children_;       // of GObject
  private GStyle      style_;          // As specified by application
  private GStyle      actualStyle_;    // Adjusted for inherits
  private List        segments_;       // of GSegment
  private boolean     isDrawn_;
  private Object      userData_;       // Application defined
  


  
  /**
   * Create a graphical object with specified name.
   * 
   * @param name  Name of object.
   */
  public GObject (String name)
  {
    name_           = name;
    parent_         = null;
    region_         = new Region();
    children_       = new ArrayList();
    isRegionValid_  = true;
    segments_       = null;
    visibilityMask_ = VISIBLE;
    isDrawn_        = false;

    style_          = null;
    actualStyle_    = new GStyle();
  }


  
  /**
   * Create a new unnamed graphic object.
   */
  public GObject()
  {
    this (null);
  }
  


  /**
   * Return name of this graphic object.
   * 
   * @return  Name of this graphic object.
   */
  public String getName()
  {
    return name_;
  }

  

  /**
   * Set name of this graphic object.
   * 
   * @param name  New name of this graphic object.
   */
  public void setName (String name)
  {
    name_ = name;
  }


  
  /**
   * Return scene of this graphic object. The scene defines the
   * world-to-device transformation.
   * 
   * @return  Scene of this graphic object (or null if the object is not
   *          attached to a scene).
   */
  public GScene getScene()
  {
    return parent_ != null ? getParent().getScene() : null;
  }



  /**
   * Conveience method for getting the transformation object of the
   * scene of this object.
   * 
   * @return  Transformation object for this graphic object (or null if the
   *          object is not attached to a scene).
   */
  public GTransformer getTransformer()
  {
    GScene scene = getScene();
    return scene != null ? scene.getTransformer() : null;
  }
  

  
  /**
   * Return the window of this graphic object.
   * 
   * @return  Window of this graphic object (or null if this object is
   *          not attached to a window).
   */
  public GWindow getWindow()
  {
    GScene scene = getScene();
    return scene != null ? scene.getWindow() : null;
  }



  /**
   * Return the region of this graphic object. The region is a union of
   * all children objects and the geometry of this object.
   * 
   * @return  The region of this graphic object.
   */
  Region getRegion()
  {
    return region_;
  }


  
  /**
   * Return all segments of this graphic object.
   * 
   * @return  All segments of this object (or null if none).
   */
  public List getSegments()
  {
    return segments_;
  }


  
  /**
   * Return first segment of this graphic object. Convenient if the caller
   * knows that it contains exactly one segment.
   * 
   * @return  First segment of this graphics object (or null if none).
   */
  public GSegment getSegment()
  {
    return segments_ != null && segments_.size() > 0 ?
                        (GSegment) segments_.get(0) : null;
  }
  
   

  /**
   * Return the current visibility mask for this object.
   * @see     #setVisibility(int)
   *
   * @return  Current visibility of this object.
   */
  public int getVisibility()
  {
    return visibilityMask_;
  }


  
  /**
   * Set user data of this graphics object.
   * 
   * @param userData  User data of this graphics object.
   */
  public void setUserData (Object userData)
  {
    userData_ = userData;
  }
  

  
  /**
   * Return user data of this graphics object.
   * 
   * @return  User data of this graphics object.
   */
  public Object getUserData()
  {
    return userData_;
  }


  
  /**
   * Find sub object based on user data tag. Search from this (included)
   * and in entire sub tree, depth first.
   * 
   * @param userData  User data of child to find.
   * @return          Requested object (or null if not found).
   */
  public GObject find (Object userData)
  {
    if (userData_ == userData)
      return this;

    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      GObject found = child.find (userData);
      if (found != null) return found;
    }

    // Not found
    return null;
  }
  

  
  /**
   * Find a segment with specified user data. Start search in this node.
   * 
   * @param userData  User data of requested segment.
   * @return          Segment (or null if not found).
   */
  public GSegment findSegment (Object userData)
  {
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (userData.equals (segment.getUserData()))
          return segment;
      }
    }

    if (children_ != null) {
      for (Iterator i = children_.iterator(); i.hasNext(); ) {
        GObject child = (GObject) i.next();
        GSegment segment = child.findSegment (userData);
        if (segment != null)
          return segment;
      }
    }

    // Not found
    return null;
  }
  
  
  
  /**
   * Compute the region for this graphics object.
   * 
   * @param visibilityMask  Visibility of object.
   */
  void computeRegion (int visibilityMask)
  {
    visibilityMask &= visibilityMask_;

    // Stop here if nothing is visible anyway
    if (visibilityMask == 0) return;
    
    // First compute regions for all children
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.computeRegion (visibilityMask);
    }

    // The region of the scene is always valid
    if (this instanceof GScene) {
      isRegionValid_ = true;
      return;
    }
    
    // Then compute region as union of all children regions and
    // content of this object
    if (!isRegionValid_) {
      region_.clear();

      // Children
      for (Iterator i = children_.iterator(); i.hasNext(); ) {
        GObject child = (GObject) i.next();
        region_.union (child.region_);
      }

      // Graphics components at this level
      if (segments_ != null) {
        for (Iterator i = segments_.iterator(); i.hasNext(); ) {
          GSegment segment = (GSegment) i.next();
          if (segment.isVisible())
            region_.union (segment.getRectangle());

          // Add text rectangles to region
          Collection texts = segment.getTexts();
          if (texts != null) {
            for (Iterator j = texts.iterator(); j.hasNext(); ) {
              GPositional text = (GPositional) j.next();
              region_.union (text.getRectangle());
            }
          }

          // Add image rectangles to region
          Collection images = segment.getImages();
          if (images != null) {
            for (Iterator j = images.iterator(); j.hasNext(); ) {
              GPositional image = (GPositional) j.next();
              region_.union (image.getRectangle());
            }
          }

          // Add component rectangles to region
          Collection components = segment.getComponents();
          if (components != null) {
            for (Iterator j = components.iterator(); j.hasNext(); ) {
              GPositional component = (GPositional) j.next();
              region_.union (component.getRectangle());
            }
          }
          
          // Add vertex image rectangles to region
          GImage vertextImage = segment.getVertexImage();
          if (vertextImage != null && segment.getNPoints() > 0) {
            Rect imageRectangle = vertextImage.getRectangle();

            // The rectangle needs to be positioned for every point
            int[] x = segment.getX();
            int[] y = segment.getY();

            for (int j = 0; j < x.length; j++) {
              Rect rectangle = new Rect (imageRectangle);
              rectangle.x += x[j];
              rectangle.y += y[j];
              
              region_.union (rectangle);
            }
          }
        }
      }

      // It might be more efficient to continue with the extent only
      if (region_.getNRectangles() > 100) region_.collapse();
      
      // This region is now OK, but parent region has to be updated
      isRegionValid_ = true;
      if (parent_ != null && !(parent_ instanceof GScene))
        parent_.isRegionValid_ = false;
    }
  }


  
  /**
   * Flag the region of this graphics object as either valid or invalid.
   * 
   * @param isValid  True if region is valid, false if it is invalid.
   */
  void flagRegionValid (boolean isValid)
  {
    isRegionValid_ = isValid;
  }
  


  /**
   * Add a sub object to this graphics object.
   * Use as:
   *
   * <ul>
   * <li>add (object, inFronOf (otherObject));
   * <li>add (object, behind (otherObject));
   * <li>add (object, front());
   * <li>add (object, back());
   * <li>add (object, 4);
   * <li>add (object);  // Same as add (object, front());
   * </ul>
   *
   * Add is performed on a parent node, adding child to self.
   *
   * @param child     Object to add
   * @param position  Position to add to
   */
  public void add (GObject child, int position)
  {
    // Remove object from current location as it can only have one parent
    if (child.parent_ != null)
      child.parent_.remove (child);

    // Brain damage
    if (position > children_.size()) position = children_.size();
    if (position < 0)                position = 0;
    
    // Add to this object
    children_.add (position, child);
    child.parent_ = this;
    // child.window_ = window_;
    // child.view_   = view_;

    // Since we inherit from parent, style may have changed
    child.updateStyle();

    // Adding stuff means we need to recompute stuff
    GScene scene = getScene();
    if (scene != null)
      scene.setAnnotationValid (false); // Possibly

    isRegionValid_ = false;
  }

  
  
  /**
   * Add child object to this object. Add to front (on screen).
   * 
   * @param child  Child to add.
   */
  public void add (GObject child)
  {
    add (child, front());
  }
  

  
  /**
   * Move this object to the front of its siblings in its parent
   * object. If doesn't have a parent, this method has no effect.
   * This is a convenience shorthand of
   * getParent().reposition (this, getParent().front());
   */
  public void toFront()
  {
    if (parent_ != null)
      parent_.reposition (this, parent_.front());
  }
  

  
  /**
   * Move this object to the behind its siblings in its parent
   * object. If doesn't have a parent, this method has no effect.
   * This is a convenience shorthand of
   * getParent().reposition (this, getParent().back());
   */
  public void toBack()
  {
    if (parent_ != null)
      parent_.reposition (this, parent_.back());
  }
  
  
  /**
   * Reposition specified child object.
   * Use as:
   *
   * <ul>
   * <li>reposition (inFronOf (otherObject));
   * <li>reposition (behind (otherObject));
   * <li>reposition (front());
   * <li>reposition (back());
   * <li>reposition (4);
   * </ul>
   *
   * @param child     Child object to be repositioned.
   * @param position  New position of child.
   */
  public void reposition (GObject child, int position)
  {
    if (position == FORWARD)
      position = getPositionOfChild (child) + 1;
    else if (position == BACKWARD)
      position = getPositionOfChild (child) - 1;

    children_.remove (child);
    add (child, position);

    updateDamage();
  }


  
  /**
   * Return the position code for the position behind the specified
   * child node.
   * @see #reposition(GObject,int)
   * 
   * @param child  Child node.
   * @return       Position for "behind" child.
   */
  public int behind (GObject child)
  {
    return getPositionOfChild (child);
  }
  

  
  /**
   * Return the position code for the position in front of the specified
   * child node.
   * @see #reposition(GObject,int)   
   * 
   * @param child  Child node.
   * @return       Position for "in front of" child.
   */
  public int inFrontOf (GObject child)
  {
    return getPositionOfChild (child) + 1;
  }

  

  /**
   * Return the position code for front.
   * @see #reposition(GObject,int)      
   *
   * @return  Position code for "front".
   */
  public int front()
  {
    return getNChildren();
  }
  

  
  /**
   * Return the position code for back.
   * @see #reposition(GObject,int)      
   *
   * @return  Position code for "back".
   */
  public int back()
  {
    return 0;
  }


  
  /**
   * Return position code for "forward".
   * @see #reposition(GObject,int)      
   * 
   * @return  Position code for "forward".
   */
  public int forward()
  {
    return FORWARD;
  }


  
  /**
   * Return position code for "backward".
   * @see #reposition(GObject,int)         
   * 
   * @return  Position code for "backward".
   */
  public int backward()
  {
    return BACKWARD;
  }


  
  /**
   * Return the position of the specified child among the children
   * of this GObject.
   * 
   * @param child  Child to find index of.
   * @return       Index of child (or -1 if no such child).
   *               0 indicates back, and so on.
   */
  private int getPositionOfChild (GObject child)
  {
    return children_.indexOf (child);
  }


  
  /**
   * Return all children objects of this GObject.
   * 
   * @return  All children objects of this GObject.
   */
  public List getChildren()
  {
    return children_;
  }


  
  /**
   * Return number of children of this graphic object.
   * 
   * @return  Number of children of this GObject.
   */
  public int getNChildren()
  {
    return children_.size();
  }

  

  /**
   * Find a child object with specified name. Search entire sub tree
   * from this node, depth first.
   * 
   * @param name  Name of object to find.
   * @return      Requested object (or null if not found).
   */
  public GObject find (String name)
  {
    if ((name_ == null && name == null) ||
        (name_ != null && name_.equals (name)))
      return this;
    
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      GObject foundObject = child.find (name);
      if (foundObject != null)
        return foundObject;
    }

    return null;
  }
  

  
  /**
   * Return all segments that intersects the specified rectangle.
   * Serach this node, and all sub nodes.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.
   * @return    List of segments intersecting the rectangle.
   *            If none do, an empty list is returned.
   */
  public List findSegments (int x0, int y0, int x1, int y1)
  {
    List segments = new ArrayList();
    findSegments (x0, y0, x1, y1, segments);
    return segments;
  }


  
  /**
   * Return all segments that are completely inside of the specified
   * rectangle. Serach this node, and all sub nodes.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.
   * @return    List of segments intersecting the rectangle.
   *            If none do, an empty list is returned.
   */
  public List findSegmentsInside (int x0, int y0, int x1, int y1)
  {
    List segments = new ArrayList();
    findSegmentsInside (x0, y0, x1, y1, segments);
    return segments;
  }
  

  
  /**
   * Return the first segment found that intersects the specified rectangle
   * Search this node, and all sub nodes.
   *
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.
   * @return    A segment intersecting the rectangle (or null
   *            if none do).
   */
  public GSegment findSegment (int x0, int y0, int x1, int y1)
  {
    // Tailormake to stop after first found
    List segments = findSegments (x0, y0, x1, y1);
    return segments.size() == 0 ? null :
                     (GSegment) segments.get (segments.size() - 1);
    
  }


  
  /**
   * Return the first segment found that are completely inside
   * the specified rectangle. Search this node, and all sub nodes.
   *
   * @param x0        X coordinate of upper left corner of rectangle.
   * @param y0        Y coordinate of upper left corner of rectangle.
   * @param x1        X coordinate of lower right corner of rectangle.
   * @param y1        Y coordinate of lower right corner of rectangle.
   * @return          A segment intersecting the rectangle (or null
   *                  if none do).
   */
  public GSegment findSegmentInside (int x0, int y0, int x1, int y1)
  {
    // TODO: Stop after first found
    List segments = findSegmentsInside (x0, y0, x1, y1);
    return segments.size() == 0 ? null :
                     (GSegment) segments.get (segments.size() - 1);
  }

  
  
  /**
   * Return the first segment found that intersects the specified point.
   * Search this node, and all sub nodes.
   *
   * @param x  X coordinate of point to check.
   * @param y  Y coordinate of point to check.   
   * @return   Front-most segment intersecting the point (or null if none do).
   */
  public GSegment findSegment (int x, int y)
  {
    List segments = findSegments (x, y);
    return segments.size() == 0 ? null :
                     (GSegment) segments.get (segments.size() - 1);
  }
  
  

  /**
   * Return all segments that intersects with the specified point.
   * Search this node, and all sub nodes.   
   *
   * @param x, y  Point to check.
   * @return      All segments intersecting the point.  If none do, an
   *              empty list is returned.
   */
  public List findSegments (int x, int y)
  {
    List segments = new ArrayList();
    findSegments (x, y, segments);
    return segments;
  }

  
  
  /**
   * Find all segments of the subtree rooted at this GObject that
   * intersects the specified rectangle.
   * 
   * @param x0        X coordinate of upper left corner of rectangle.
   * @param y0        Y coordinate of upper left corner of rectangle.
   * @param x1        X coordinate of lower right corner of rectangle.
   * @param y1        Y coordinate of lower right corner of rectangle.
   * @param segments  List to add segments to.
   */
  private void findSegments (int x0, int y0, int x1, int y1, List segments)
  {
    // Don't scan any furher if the region doesn't intersect
    if (!region_.isIntersecting (new Rect (x0, y0, x1-x0+1, y1-y0+1)))
      return;
    
    // The region intersects, but we need to consider each segment
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (segment.isIntersectingRectangle (x0, y0, x1, y1))
          segments.add (segment);
      }
    }

    // Check subobjects
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.findSegments (x0, y0, x1, y1, segments);
    }
  }
  

  
  /**
   * Find all segments of the subtree rooted at this GObject that are
   * intersects the specified point.
   * 
   * @param x         X coordinate of point to check.
   * @param y         Y coordinate of point to check.
   * @param segments  List to add segments to.
   */
  private void findSegments (int x, int y, List segments)
  {
    // Don't scan any furher if the region doesn't intersect
    if (!region_.isInside (x, y))
      return;
    
    // The region intersects, but we need to consider each segment
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (segment.isIntersectingPoint (x, y))
          segments.add (segment);
      }
    }

    // Check subobjects
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.findSegments (x, y, segments);
    }
  }


  
  /**
   * Find all segments of the subtree rooted at this GObject that are
   * inside the specified rectangle.
   * 
   * @param x0        X coordinate of upper left corner of rectangle.
   * @param y0        Y coordinate of upper left corner of rectangle.
   * @param x1        X coordinate of lower right corner of rectangle.
   * @param y1        Y coordinate of lower right corner of rectangle.
   * @param segments  List to add segments to.
   */
  private void findSegmentsInside (int x0, int y0, int x1, int y1,
                                   List segments)
  {
    // Don't scan any furher if the region doesn't intersect
    if (!region_.isIntersecting (new Rect (x0, y0, x1-x0+1, y1-y0+1)))
      return;

    // The region intersects, but we need to consider each segment
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (segment.isInsideRectangle (x0, y0, x1, y1))
          segments.add (segment);
      }
    }

    // Check subobjects
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.findSegmentsInside (x0, y0, x1, y1, segments);
    }
  }
  

    
  /**
   * Return front-most object intersecting the specfied rectangle.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.
   * @return    Front-most object intersecting the specified rectangle
   *            (or null if none do).
   */
  public GObject find (int x0, int y0, int x1, int y1)
  {
    // TODO: As it needs the first only, this can be optimized
    List objects = findAll (x0, y0, x1, y1);
    return objects.size() == 0 ? null :
                     (GObject) objects.get (objects.size() - 1);
  }


  
  /**
   * Find front-most object that are inside the specified rectangle.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.
   * @return    Front most child object that are fully inside the
   *            specified rectangle (or null if none are).
   */
  public GObject findInside (int x0, int y0, int x1, int y1)
  {
    // TODO: As it needs the first only, this can be optimized
    List objects = findAllInside (x0, y0, x1, y1);
    return objects.size() == 0 ? null :
                     (GObject) objects.get (objects.size() - 1);
  }


  
  /**
   * Return front-most object intersecting the specfied point.
   * 
   * @param x  X coordinate of point to check.
   * @param y  Y coordinate of point to check.   
   * @return   Front-most object intersecting the point
   *           (or null if none do).
   */
  public GObject find (int x, int y)
  {
    return find (x - 1, y - 1, x + 1, y + 1);
  }
  

  
  /**
   * Find all objects intersecting a specified rectangle. Objects are
   * returned with front most object on screen last in list. Seacrh is
   * done in the subtree of this object (including this). If no objects
   * intersects, an empty list is returned.
   * 
   * @param x0  X coordinate of upper left corner of rectangle.
   * @param y0  Y coordinate of upper left corner of rectangle.
   * @param x1  X coordinate of lower right corner of rectangle.
   * @param y1  Y coordinate of lower right corner of rectangle.
   * @return    All objects intersecting the specified rectangle.
   */
  public List findAll (int x0, int y0, int x1, int y1)
  {
    List objects = new ArrayList();
    findAll (x0, y0, x1, y1, objects);
    return objects;
  }


  
  /**
   * Find all objects inside a specified rectangle. Objects are
   * returned with front most object on screen last in list. Seacrh is
   * done in the subtree of this object (including this). If no objects
   * is inside, an empty list is returned.
   * 
   * @param x0, y0, x1, y1  Rectangle to check.
   * @return                All objects intersecting the rectangle.
   */
  public List findAllInside (int x0, int y0, int x1, int y1)
  {
    List objects = new ArrayList();
    findAllInside (x0, y0, x1, y1, objects);
    return objects;
  }


  
  /**
   * Find all objects intersecting a specified point. Objects are
   * returned with front most object on screen last in list. Seacrh is
   * done in the subtree of this object (including this). If no objects
   * intersects, an empty list is returned.
   * 
   * @param x  X coordinate of point to check.
   * @param y  Y coordinate of point to check.   
   * @return   All objects intersecting the specfied point.
   */
  public List findAll (int x, int y)
  {
    return findAll (x - 1, y - 1, x + 1, y + 1);
  }
  

  
  /**
   * Find all sub objects that intersects the given rectangle.
   * 
   * @param x0       X coordinate of upper left corner of rectangle.
   * @param y0       Y coordinate of upper left corner of rectangle.
   * @param x1       X coordinate of lower right corner of rectangle.
   * @param y1       Y coordinate of lower right corner of rectangle.
   * @param objects  Object collection to add to.
   */
  private void findAll (int x0, int y0, int x1, int y1, List objects)
  {
    // Don't scan any furher if the region doesn't intersect
    if (!region_.isIntersecting (new Rect (x0, y0, x1 - x0 + 1, y1 - y0 + 1)))
      return;

    // Check subobjects first
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.findAll (x0, y0, x1, y1, objects);
    }
    
    // The region intersects, but we need to consider each segment
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (segment.isIntersectingRectangle (x0, y0, x1, y1)) {
          objects.add (this);
          break;
        }
      }
    }
  }
  
  

  /**
   * Find all sub objects that are inside the given rectangle.
   * 
   * @param x0       X coordinate of upper left corner of rectangle.
   * @param y0       Y coordinate of upper left corner of rectangle.
   * @param x1       X coordinate of lower right corner of rectangle.
   * @param y1       Y coordinate of lower right corner of rectangle.
   * @param objects  Object collection to add to.
   */
  private void findAllInside (int x0, int y0, int x1, int y1, List objects)
  {
    Box box = new Box (x0, y0, x1, y1);
    
    // Don't scan any furher if the region doesn't intersect
    if (!region_.isIntersecting (new Rect (box)))
      return;

    // Check subobjects first
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.findAllInside (x0, y0, x1, y1, objects);
    }
    
    if (region_.isInsideOf (new Rect (box)))
      objects.add (this);
  }
  
  

  /**
   * Unlink self from parent child list.
   */
  public void remove()
  {
    if (parent_ != null)
      parent_.remove (this);
  }
  
  

  /**
   * Remove child from child list.
   * 
   * @param child  Child object to remove.
   */
  public void remove (GObject child)
  {
    // Update damage with removed region
    child.updateDamage();

    children_.remove (child);
    child.parent_ = null;

    // Annotation might need to be recomputed
    // TODO: Check if subtree actually has texts before assuming this
    GScene scene = getScene();
    if (scene != null)
      scene.setAnnotationValid (false);

    // Force a region recompmutation
    isRegionValid_ = false;
  }




  /**
   * Remove all children objects.
   */
  public void removeAll()
  {
    // Loop over a copy of the children list as they are removed
    Collection children = new ArrayList (children_);
    for (Iterator i = children.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      remove (child);
    }
  }

  

  /**
   * Return the position of this object among its siblings.
   * 
   * @return  Position of this object among its siblings.
   */
  private int getPosition()
  {
    if (parent_ == null) return -1;
    return parent_.children_.indexOf (this);
  }



  /**
   * Return the child object at specified position.
   * 
   * @param position  Position to return child of.
   * @return          Requested child (or null if non existing).
   */
  public GObject getChild (int position)
  {
    if (position < 0 || position > children_.size() - 1) 
      return null;

    return (GObject) children_.get (position);
  }
  

  /**
   * Return true if this object is in front of all its siblings.
   * 
   * @return  True if this object is in fron of all siblings, false otherwise.
   */
  public boolean isInFront()
  {
    return parent_ == null ? false : getPosition() == parent_.getNChildren()-1;
  }



  /**
   * Return true if this object is behind all its siblings.
   * 
   * @return  True if this object is behind all siblings, false otherwise.
   */
  public boolean isInBack()
  {
    return parent_ == null ? false : getPosition() == 0;
  }
  

  
  /**
   * Return the sibling object in the immediate front of this object.
   * 
   * @return  The sibling object in the immediate front of this object
   *          (or null if this object is in front, or it is not attach to a
   *          parent).
   */
  public GObject getObjectInFront()
  {
    if (parent_ == null) return null;
    if (isInFront()) return null;

    int position = getPosition();
    return parent_.getChild (position + 1);
  }



  /**
   * Return the sibling object immediately behind this object.
   * 
   * @return  The sibling object immediately behind this object
   *          (or null if this object is in back, or it is not attach to a
   *          parent).
   */
  public GObject getObjectBehind()
  {
    if (parent_ == null) return null;
    if (isInBack()) return null;

    int position = getPosition();
    return parent_.getChild (position - 1);
  }

  

  /**
   * Add a new segment to this object.
   * 
   * @param segment  Segment to add.
   */
  public void addSegment (GSegment segment)
  {
    // Lazy create as not all GObjects will have segments
    if (segments_ == null)
      segments_ = new ArrayList();

    // Do nothing if it is there already
    if (segments_.contains (segment))
      return;
    
    segments_.add (segment);
    segment.setOwner (this);

    // Since segment style may inherit from this
    segment.updateStyle();
  }



  /**
   * Return the n'th segment of this object.
   * 
   * @param segmentNo  Segment number to return.
   * @return           N'th segment (or null if non existent).
   */
  public GSegment getSegment (int segmentNo)
  {
    int nSegments = getNSegments();
    return segmentNo < 0 || segmentNo >= nSegments ? null :
                                         (GSegment) segments_.get (segmentNo);
  }


  
  /**
   * Return number of segments in this GObject.
   * 
   * @return  Number of segments in this GObject.
   */
  public int getNSegments()
  {
    return segments_ != null ? segments_.size() : 0;
  }
  


  /**
   * Remove the specified segment from this GObject.
   * 
   * @param segment  Segment to remove. If the specified segment is not
   *                 a child of this GObject, this call has no effect.
   */
  public void removeSegment (GSegment segment)
  {
    // Cannot remove if it doesn't belong here
    if (segment.getOwner() != this)
      return;

    // Update damage
    Region region = segment.getRegion();
    GWindow window = getWindow();
    if (window != null)
      window.updateDamageArea (region);    
    
    // If there was texts on this segment, flag annotation to be redone
    if (segment.getTexts() != null) {
      GScene scene = getScene();
      if (scene != null)
        scene.setAnnotationValid (false);
    }
    
    segments_.remove (segment);
    segment.setOwner (null);
    
    if (segments_.size() == 0)
      segments_ = null;
      
    isRegionValid_ = false;    
  }


  
  /**
   * Remove specified segment from this GObject.
   * 
   * @param segmentNo  Segment to remove. If the specified segment does not
   *                   exist, this casll has no effect.
   */
  public void removeSegment (int segmentNo)
  {
    GSegment segment = getSegment (segmentNo);
    if (segment != null)
      removeSegment (segment);
  }


  
  /**
   * Remove all segments from this GObject.
   */
  public void removeSegments()
  {
    while (segments_ !=  null)
      removeSegment (0);
  }
  

  
  /**
   * Remove sequence of segments from this object.
   * 
   * @param from  Starting segment
   * @param to    Ending segment (-1 indicates all).
   */
  public void removeSegments (int from, int to)
  {
    if (segments_ == null || from >= segments_.size())
      return;
    
    if (from < 0) from = 0;
    
    if (to >= segments_.size() || to == -1)
      to = segments_.size() - 1;
    if (to < 0) to = 0;

    int nSegments = from - to + 1;
    for (int i = 0; i < nSegments; i++)
      removeSegment (from);

    isRegionValid_ = false;    
  }
  


  /**
   * Remove sequence of segments from this object. Remove all from
   * specified start segment.
   * 
   * @param from  Starting segment
   */
  public void removeSegments (int from)
  {
    removeSegments (from, -1);
  }
  


  /**
   * Refresh all segments of this GObject.
   * 
   * @param visibilityMask  Visibility of parent object.
   */
  void refreshData (int visibilityMask)
  {
    // Compute actual visibility of this object
    visibilityMask &= visibilityMask_;

    // If data is not visible on this level, return
    if ((visibilityMask & DATA_VISIBLE) == 0)
      return;

    // If we don't intersect with the damage, return
    if (!region_.isIntersecting (getWindow().getDamageRegion()))
      return;

    // Refreshing self
    if (segments_ != null) {
      GCanvas canvas = (GCanvas) getWindow().getCanvas();
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (!segment.isVisible()) continue;

        // Render the segment
        canvas.render (segment.getX(), segment.getY(), 
                       segment.getActualStyle());

        // Render vertex images
        GImage vertexImage = segment.getVertexImage();
        if (vertexImage != null)
          canvas.render (segment.getX(), segment.getY(),
                         vertexImage);
        
        // Render the images
        Collection images = segment.getImages();
        if (images != null) {
          for (Iterator j = images.iterator(); j.hasNext(); ) {
            GImage image = (GImage) j.next();
            if (image != null && image.isVisible())
              canvas.render (image);
          }
        }
      }
    }

    // Refreshing children
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.refreshData (visibilityMask);
    }
  }

  

  /**
   * Refresh all annotations of this GObject.
   * 
   * @param visibilityMask  Visibility of parent object.
   */
  void refreshAnnotation (int visibilityMask)
  {
    // Compute actual visibility of this object
    visibilityMask &= visibilityMask_;

    // If annotation is not visible on this level, return
    if ((visibilityMask & GObject.ANNOTATION_VISIBLE) == 0)
      return;

    // If we don't intersect with damage, return
    if (!region_.isIntersecting (getWindow().getDamageRegion()))
      return;

    // Refreshing self
    if (segments_ != null) {
      GCanvas canvas = (GCanvas) getWindow().getCanvas();
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        if (!segment.isVisible()) continue;

        Collection texts = segment.getTexts();
        if (texts != null) {
          for (Iterator j = texts.iterator(); j.hasNext(); ) {
            GText text = (GText) j.next();
            if (text != null && text.isVisible())
              canvas.render (text, text.getActualStyle());
          }
        }
      }
    }

    // Refreshing children
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.refreshAnnotation (visibilityMask);
    }
  }
  
  

  /**
   * Refresh all AWT components of this GObject.
   * 
   * @param visibilityMask  Visibility of parent object.
   */
  void refreshComponents (int visibilityMask)
  {
    // Compute actual visibility of this object    
    visibilityMask &= visibilityMask_;

    // If components are not visible on this level, return
    if ((visibilityMask & GObject.WIDGETS_VISIBLE) == 0)
      return;

    // If we don't intersect with damage, return
    if (!region_.isIntersecting (getWindow().getDamageRegion()))
      return;

    // Refreshing self
    if (segments_ != null) {
      GCanvas canvas = (GCanvas) getWindow().getCanvas();
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {

        GSegment segment = (GSegment) i.next();
        if (!segment.isVisible()) continue;

        Collection components = segment.getComponents();
        if (components != null) {
          for (Iterator j = components.iterator(); j.hasNext(); ) {
            GComponent component = (GComponent) j.next();
            canvas.render (component);
          }
        }
      }
    }

    // Refreshing children
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.refreshComponents (visibilityMask);
    }
  }

  

  /**
   * The redraw method ensures that the geometry in the entire
   * scene becomes up to date. Called initially and then in the
   * following cases:
   *
   * <ul>
   * <li>resize
   * <li>setViewport
   * <li>setWorldExtent
   * <li>setVisibility to on, because invisible views are not
   *     redrawn in the case above.
   * </ul>
   * 
   * @param visibilityMask
   */
  protected void redraw (int visibilityMask)
  {
    // If not visible at this level, skip here but mark as not drawn
    isDrawn_ = false;
    visibilityMask &= visibilityMask_;
    if (visibilityMask == 0) return;

    // Mark annotation as dirty
    if (this instanceof GScene) {
      GScene scene = (GScene) this;
      scene.setAnnotationValid (false);
    }

    // Redraw children
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.redraw (visibilityMask);
    }

    // Let application object draw itself
    draw();

    // Compute posistions for all images
    computeImagePositions();

    // Marks as redrawn
    isDrawn_ = true;
  }


  
  /**
   * Force a redraw of this object.
   * <p>
   * Normally this method is called automatically whenever needed
   * (typically on retransformations).
   * A client application <em>may</em> call this method explicitly
   * if some external factor that influence the graphics has been
   * changed. However, beware of the performance overhead of such
   * an approach, and consider reorganizing the code so that model
   * changes explictly affects the graphics elements.
   */
  public void redraw()
  {
    redraw (getVisibility());
  }
  


  /**
   * Set new style for this object. Style elements not explicitly
   * set within this GStyle object are inherited from parent
   * objects. Child objects with not explicit set style will
   * inherit from this style object. A GObject does not need a style
   * instance; in this case all style elements are inherited.
   * 
   * @param style  Style of this object (or null if the intent is
   *               to unset the current style).
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
   * Return style of this object. This is the style set by setStyle()
   * and not necesserily the style as it appears on screen as unset
   * style elements are inherited from parents.
   * 
   * @return  Style of this object as specified with setStyle(), (or
   *          null if no style has been provided).
   */
  public GStyle getStyle()
  {
    return style_;
  }

  

  /**
   * This is the actual style used for this object when
   * inheritance for unset values are resolved.
   * 
   * @return  Actual style for this segment.
   */
  GStyle getActualStyle()
  {
    return actualStyle_;
  }
  

  
  /**
   * Resolve unset values in the style element of this object.
   * This involved creating a default "actual style" element,
   * override with style elements of the parents actual style
   * and override with style elements explicitly set in the style
   * of this object (if any).
   */
  private void updateStyle()
  {
    // Initialize the actual style used for this object
    actualStyle_ = new GStyle();

    // Update with parent style
    if (parent_ != null)
      actualStyle_.update (parent_.getActualStyle());

    // Update (and possible override) with present style
    if (style_ != null)
      actualStyle_.update (style_);

    // Update style of segments
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        segment.updateStyle();
      }
    }

    // Update style of children objects
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.updateStyle();
    }
  }


  
  /**
   * Change the visibility for this object.
   * <p>
   * NOTE: The present visibility is maintained, and the new
   * flag is taken as an <em>instruction</em> for change.
   *
   * The instruction is an or'ed combination of:
   *
   * <ul>
   *   <li> DATA_VISIBLE        
   *   <li> ANNOTATION_VISIBLE
   *   <li> SYMBOLS_VISIBLE
   *   <li> WIDGETS_VISIBLE
   *   <li> DATA_INVISIBLE
   *   <li> ANNOTATION_INVISIBLE
   *   <li> SYMBOLS_INVISIBLE
   *   <li> WIDGETS_INVISIBLE
   * </ul>
   *
   * The settings VISIBLE and INVISIBLE are conveniences compound
   * for all visible and invisible settings respectively.
   *
   * @param visibilityMask  Visibility instruction.
   */
  public void setVisibility (int visibilityMask)
  {
    int  oldVisibilityMask = visibilityMask_;

    // Determine the new visibility code
    if ((visibilityMask & DATA_VISIBLE)         != 0) 
      visibilityMask_ |=  DATA_VISIBLE;
    if ((visibilityMask & ANNOTATION_VISIBLE)   != 0) 
      visibilityMask_ |=  ANNOTATION_VISIBLE;
    if ((visibilityMask & SYMBOLS_VISIBLE)      != 0)   
      visibilityMask_ |=  SYMBOLS_VISIBLE;
    if ((visibilityMask & WIDGETS_VISIBLE)      != 0)   
      visibilityMask_ |=  WIDGETS_VISIBLE;
    if ((visibilityMask & DATA_INVISIBLE)       != 0)
      visibilityMask_ &= ~DATA_VISIBLE;
    if ((visibilityMask & ANNOTATION_INVISIBLE) != 0)  
      visibilityMask_ &= ~ANNOTATION_VISIBLE;
    if ((visibilityMask & SYMBOLS_INVISIBLE)    != 0) 
      visibilityMask_ &= ~SYMBOLS_VISIBLE;
    if ((visibilityMask & WIDGETS_INVISIBLE)    != 0)   
      visibilityMask_ &= ~WIDGETS_VISIBLE;

    // Return if nothing has changed
    if (oldVisibilityMask == visibilityMask_)
      return;

    // Redraw if something was turned on and draw has not been done
    if (visibilityMask_ != 0 && !isDrawn_)
      redraw (visibilityMask_);
    
    // Symbol visibility has changed
    if ((oldVisibilityMask & SYMBOLS_VISIBLE) !=
        (visibilityMask_ & SYMBOLS_VISIBLE))
      changeSymbolVisibility();

    // Widget visibility has changed
    if ((oldVisibilityMask & WIDGETS_VISIBLE) !=
        (visibilityMask_ & WIDGETS_VISIBLE))
      changeWidgetVisibility ();
  
    // Annotation visibility has changed
    if ((oldVisibilityMask & ANNOTATION_VISIBLE) !=
        (visibilityMask_ & ANNOTATION_VISIBLE)) {

      GScene scene = getScene();
      if (scene != null)
        scene.setAnnotationValid (false);
      
      // Redo annotation unless this was an empty object
      if (children_.size() > 0 || getNSegments() > 0) {
        updateDamage();
        // Don't think we need this. Check for errors later.
        // getWindow().computeTextPositions();
      }
    }
    
    // Data visibility has changed
    if ((oldVisibilityMask & DATA_VISIBLE) !=
        (visibilityMask_ & DATA_VISIBLE)) {
    
      // If the visibility is turned on, we must find the region first
      if ((visibilityMask & DATA_VISIBLE) != 0)
        getWindow().computeRegion();
      
      updateDamage();
    }
  }


  
  private void changeSymbolVisibility()
  {
    // TODO
    
    // Handle children first
  }


  
  private void changeWidgetVisibility()
  {
    // TODO
    
    // Handle children first
  }


  
  /**
   * Compute position of all GTexts in the subtree rooted
   * at this GObject.
   */
  void computeTextPositions()
  {
    GScene scene = getScene();
    if (scene == null) return;
    
    // Stop here if annotation is not visible
    if ((visibilityMask_ & ANNOTATION_VISIBLE) == 0)
      return;

    //
    // Do annotation on children
    //
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.computeTextPositions();
    }

    //
    // Do annotation on self
    //

    // Do annotation on all segments
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        Collection texts = segment.getTexts();
        scene.computePositions (texts);
      }
    }
  }


  
  /**
   * Compute position of all GComponents in the subtree
   * rooted at this GObject.
   */
  void computeComponentPositions()
  {
    GScene scene = getScene();
    if (scene == null) return;
    
    // Stop here if annotation is not visible
    if ((visibilityMask_ & WIDGETS_VISIBLE) == 0)
      return;

    //
    // Do annotation on children
    //
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.computeComponentPositions();
    }

    //
    // Do annotation on self
    //

    // Do annotation on all segments
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        Collection components = segment.getComponents();
        scene.computePositions (components);
      }
    }
  }


  
  /**
   * Compute position of all GImages in the subtree rooted
   * at this GObject.
   */
  void computeImagePositions()
  {
    GScene scene = getScene();
    if (scene == null) return;

    // Loop over all segments and position their images
    if (segments_ != null) {
      for (Iterator i = segments_.iterator(); i.hasNext(); ) {
        GSegment segment = (GSegment) i.next();
        Collection images = segment.getImages();
        scene.computePositions (images);

        GImage vertexImage = segment.getVertexImage();
        if (vertexImage != null)
          scene.computeVertexPositions (vertexImage);
      }
    }

    // Compute image positions for children
    for (Iterator i = children_.iterator(); i.hasNext(); ) {
      GObject child = (GObject) i.next();
      child.computeImagePositions();
    }
  }


  
  /**
   * Update window damage area with the region extent of this object.
   */
  private void updateDamage()
  {
    GWindow window = getWindow();
    if (window != null)
      window.updateDamageArea (region_);    
  }
  

  
  /**
   * Return parent GObject of this object.
   * 
   * @return  Parent of this object (or null if it doesn't have a parent).
   */
  public GObject getParent()
  {
    return parent_;
  }
  

  
  /**
   * Convenience method for refreshing the window canvas.
   * Equivalent to <code>getWindow().refresh();</code>.
   */
  public void refresh()
  {
    GWindow window = getWindow();
    if (window != null)
      window.refresh();
  }
  
  
  
  /**
   * This method should be overloaded for graphics objects with drawable
   * elements (GSegments). Intermediate nodes should leave the method empty.
   */
  public void draw()
  {
  }


  
  /**
   * Called when the style of this object is changed.
   * 
   * @param style  Style that has changed.
   */
  public void styleChanged (GStyle style)
  {
    updateStyle();
    updateDamage();
  }
  
  

  /**
   * Return a string representation of this object.
   * 
   * @return  String representation of this object.
   */
  public String toString()
  {
    return "GObject: " + name_;
  }
}
