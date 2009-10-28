package no.geosoft.cc.graphics;



import java.util.Collection;
import java.awt.Adjustable;

import no.geosoft.cc.geometry.Rect;



/**
 * The GScene is the link between a GWindow and the graphics objects.
 * <p>
 * The GScene defines the viewport and the world extent and holds
 * device-to-world transformation objects. The scene is itself a
 * graphics object (GObject) and as such it may contain geometry.
 * <p>
 * Typical usage:
 *
 * <pre>
 *    // Creating a window
 *    GWindow window = new GWindow (Color.WHITE);
 *
 *    // Creating a scene within the window
 *    GScene scene = new GScene (window);
 *    scene.setWorldExtent (0.0, 0.0, 1000.0, 1000.0);
 * </pre>
 *
 * Setting world extent is optional. If unset it will have the same
 * extent (in floating point coordinates) as the device.
 * <p>
 * When geometry is specified (in GSegments), coordinates are specified
 * in either device coordinates or in world coordinates. Integer coordinates
 * are assumed to be device relative, while floating point coordinates are
 * taken to be world extent relative.
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GScene extends GObject
{
  private final GWindow      window_;
  private final GTransformer transformer_;
  private final GAnnotator   annotator_;

  private GViewport       viewport_;
  private GWorldExtent    initialWorldExtent_;
  private GWorldExtent    currentWorldExtent_;
  private boolean         shouldZoomOnResize_;  // "see more" or "see bigger"
  private boolean         shouldWorldExtentFitViewport_;  // ie. squeeze world
  private boolean         isAnnotationValid_;
  private GScrollHandler  scrollHandler_;
  private boolean         isViewportFixed_;
  

  
  /**
   * Create a scene within the specified window. For most practical
   * purposes a window will contain one scene only.
   * <p>
   * A default viewport will be established covering the entire window.
   * This is appropriate in most cases.
   * <p>
   * A default world-extent will be established with coordinates equal
   * to the window device extent. This may be sufficient in many cases
   * if object geometry is specified in device coordinates.
   * 
   * @param window  Window to attach this scene to.
   * @param name    Name of this scene.
   */
  public GScene (GWindow window, String name)
  {
    super (name);

    shouldWorldExtentFitViewport_ = true;
    shouldZoomOnResize_           = true;  // "see bigger"

    // Attach to window
    window_ = window;
    window_.addScene (this);

    // Default viewport equals window
    int  viewportWidth  = window_.getWidth();
    int  viewportHeight = window_.getHeight();
    viewport_ = new GViewport (0, 0, viewportWidth, viewportHeight);
    isViewportFixed_ = false;

    // Default world extent equals window
    double w0[] = {0.0,                    (double) viewportHeight, 0.0};
    double w1[] = {(double) viewportWidth, (double) viewportHeight, 0.0};
    double w2[] = {0.0,                    0.0,                     0.0};
    initialWorldExtent_ = new GWorldExtent (w0, w1, w2);
    currentWorldExtent_ = new GWorldExtent (w0, w1, w2);
    
    // Create transformer instance
    transformer_ = new GTransformer (viewport_, currentWorldExtent_);

    // Instantiate the annotator object
    annotator_ = new GAnnotator (this);

    scrollHandler_ = null;

    // Initiate region
    updateRegion();
  }


  
  /**
   * Create a nameless scene within the specified window.
   * 
   * @param window  Window to acttach this scene to.
   */
  public GScene (GWindow window)
  {
    this (window, null);
  }
  

  
  /**
   * Return the scene of this GObject. At this level the scene is
   * this object.
   * 
   * @return  The scene of this GObject.
   */
  public GScene getScene()
  {
    return this;
  }


  
  /**
   * Return the window of this scene. A scene is always attached to
   * a window, a GObject is not necesserily.
   * 
   * @return  Window of this scene. 
   */
  public GWindow getWindow()
  {
    return window_;
  }
  

  
  /**
   * Return the transformation object of this scene. The transformer
   * object can be used for client-side world-to-device and device-to-world
   * coordinate transformations.
   * 
   * @return  Current transformation object of this scene.
   */
  public GTransformer getTransformer()
  {
    return transformer_;
  }
  


  /**
   * Specified if one should zoom on resize. Set to true, the current
   * image will change size (i.e. zoomed). Set to false the current image
   * will be the same size and one will possibley see more or less
   * instead. Default is true.
   * 
   * @param shouldZoomOnResize  True if world extent should be unchanged
   *                            on resize, false otherwise.
   */
  public void shouldZoomOnResize (boolean shouldZoomOnResize)
  {
    shouldZoomOnResize_ = shouldZoomOnResize;
  }
  


  /**
   * Specify wether world extent should always fit the viewport (i.e. have
   * same aspect ratio) during resize. Default is true.
   * 
   * @param shouldWorldExtentFitViewport  True if the world extent should
   *                                      fit the viewport, false otherwise.
   */
  public void shouldWorldExtentFitViewport (boolean 
                                            shouldWorldExtentFitViewport)
  {
    shouldWorldExtentFitViewport_ = shouldWorldExtentFitViewport;
  }
                                         


  /**
   * Set viewport for this scene. The viewport is specified in device
   * coordinates. The layout is as follows:
   *
   * <pre>
   *
   *     x0,y0 o-------o  x1,y1
   *           |
   *           |
   *           |
   *     x2,y2 o
   *
   * </pre>
   *
   * It is thus possible to create a skewed viewport, which may be handy
   * in some situations.
   * <p>
   * If the viewport is not set by a client, it will fit the canvas and
   * adjust to it during resize. If it is set by client, it will stay
   * fixed and not adjusted on resize.
   */
  public void setViewport (int x0, int y0, int x1, int y1, int x2, int y2)
  {
    isViewportFixed_ = true;

    // Flag old viewport as damaged
    // TODO: If the viewport is moved around in the canvas during
    // execution (rare), old content outside the new viewport will
    // remain as redraw is clipped against the new viewport.
    window_.updateDamageArea (getRegion());

    // Update viewport
    viewport_.set (x0, y0, x1, y1, x2, y2);

    // Set the new region for this scene
    updateRegion();

    // Flag new viewport as damaged
    window_.updateDamageArea (getRegion());
    
    adjustCurrentWorldExtent();
    transformer_.update (viewport_, currentWorldExtent_);

    // Redraw
    annotator_.reset();
    redraw (getVisibility());

    // Update scrollbars
    if (scrollHandler_ != null)
      scrollHandler_.updateScrollBars();
  }



  /**
   * Set viewport to a rectangular area of the screen. The viewport
   * layout is as follows:
   *
   * <pre>
   *
   *             width
   *     x0,y0 o-------o
   *           |
   *    height |
   *           |
   *           o
   *
   * </pre>
   * 
   * @param x0      X coordinate of upper left corner of viewport.
   * @param y0      Y coordinate of upper left corner of viewport.   
   * @param width   Width of viewport.
   * @param height  Height of viewport.
   */
  public void setViewport (int x0, int y0, int width, int height)
  {
    setViewport (x0, y0, x0+width-1, y0, x0, y0+height-1);
  }
  

  
  /**
   * Return current viewport.
   * 
   * @return  Current viewport of this scene.
   */
  public GViewport getViewport()
  {
    return viewport_;
  }



  /**
   * Set world extent of this scene. The layout is as follows:
   *
   * <pre>
   *        w2 o 
   *           |
   *           |
   *           |
   *        w0 o-------o w1
   * </pre>
   *
   * Thus w2 is mapped to viewport (x0,y0), w0 is mapped to (x2,y2) and
   * w1 is mapped to lower right corner of viewport.
   * <p>
   * w0,w1 and w2 are three dimensions, and the world extent can thus be
   * any plane in a 3D space, and the plane may be skewed.
   * 
   * @param w0  Point 0 of the new world extent [x,y,z].
   * @param w1  Point 1 of the new world extent [x,y,z].
   * @param w2  Point 2 of the new world extent [x,y,z].
   */
  public void setWorldExtent (double w0[], double w1[], double w2[])
  {
    initialWorldExtent_.set (w0, w1, w2);
    currentWorldExtent_.set (w0, w1, w2);
    
    adjustCurrentWorldExtent();
    transformer_.update (viewport_, currentWorldExtent_);

    // Flag new viewport as damaged
    window_.updateDamageArea (getRegion());

    redraw (getVisibility());
    
    // Update scrollbars
    if (scrollHandler_ != null)
      scrollHandler_.updateScrollBars();
  }



  /**
   * A convenience method for specifying a orthogonal world extent in
   * the Z=0 plane. The layout is as follows:
   *
   * <pre>
   *           o 
   *           |
   *    height |
   *           |
   *     x0,y0 o-------o
   *             width
   *
   * </pre>
   * 
   * @param x0      X coordinate of world extent origin.
   * @param y0      Y coordinate of world extent origin.
   * @param width   Width of world extent.
   * @param height  Height of world extent.
   */
  public void setWorldExtent (double x0, double y0, double width, double height)
  {
    double w0[] = {x0,         y0,          0.0};
    double w1[] = {x0 + width, y0,          0.0};
    double w2[] = {x0,         y0 + height, 0.0};

    setWorldExtent (w0, w1, w2);
  }
  
  

  /**
   * Return the world extent as specified by the application.
   * 
   * @return  The world extent as it was specified through setWorldExtent().
   */
  GWorldExtent getInitialWorldExtent()
  {
    return initialWorldExtent_;
  }


  
  /**
   * Return current world extent. The current world extent may differ the
   * initial world extent due to zooming and window resizing.
   * 
   * @return  Current world extent.
   */
  public GWorldExtent getWorldExtent()
  {
    return currentWorldExtent_;
  }
  
  

  /**
   * Adjust the current world extent according to current viewport.
   * This method is called whenever the viewport has changed.
   */
  private void adjustCurrentWorldExtent()
  {
    // Do nothing if the world extent is supposed to fit viewport
    if (shouldWorldExtentFitViewport_)
      return;

    // Viewport dimensions
    double viewportWidth  = (double) viewport_.getWidth();
    double viewportHeight = (double) viewport_.getHeight();

    // World dimensions
    double worldWidth  = currentWorldExtent_.getWidth();
    double worldHeight = currentWorldExtent_.getHeight();

    // Compute adjusted width or height
    double newWorldWidth;
    double newWorldHeight;

    if (worldWidth / worldHeight > viewportWidth / viewportHeight) {
      newWorldWidth  = worldWidth;
      newWorldHeight = viewportHeight / viewportWidth * worldWidth;
    }
    else {
      newWorldWidth  = viewportWidth / viewportHeight * worldHeight;
      newWorldHeight = worldHeight;
    }

    currentWorldExtent_.extendWidth  (newWorldWidth);
    currentWorldExtent_.extendHeight (newWorldHeight);
  }
  

  
  /**
   * Update region for this GObject. The region of a GScene is always the
   * viewport extent.
   */
  private void updateRegion()
  {
    if (viewport_.isSkewed()) {
      // TODO. Missing the create Region of a general polygon,
      // this case is special though and can be hacked by adding
      // a rectangle for each scan line in the viewport.
    }
    else {
      Rect rectangle = new Rect (viewport_.getX0(),
                                 viewport_.getY0(),
                                 (int) viewport_.getWidth(),
                                 (int) viewport_.getHeight());

      getRegion().set (rectangle);
    }

    flagRegionValid (true);
  }


  
  /**
   * Resize this scene the specified fraction in x and y direction.
   * <p>
   * If a client uses scenes wich covers a specific part of a window,
   * it may want to extend GScene and override this method in order to
   * adjust the viewport according to the new window size. This can
   * be done as follows:
   *
   * <pre>
   *   protected void resize (double dx, double dy)
   *   {
   *     super (dx, dy);
   *     setViewport (...);
   *   }
   * </pre>
   * 
   * @param dx  Resize fraction in x direction.
   * @param dy  Resize fraction in y direction.
   */
  protected void resize (double dx, double dy)
  {
    if (isViewportFixed_) return;
    
    // Resize viewport
    viewport_.resize (dx, dy);

    // Resize world extent
    if (!shouldWorldExtentFitViewport_) {

      // If we resize the world extents accordingly we will see more
      // than before, in same scale, thus "see more".
      // If we keep the world extent unchanged we will see the same
      // extent as before but rescaled, thus "see bigger"
      if (!shouldZoomOnResize_) {
        initialWorldExtent_.resize (dx, dy);
        currentWorldExtent_.resize (dx, dy);
      }

      adjustCurrentWorldExtent();
    }

    transformer_.update (viewport_, currentWorldExtent_);

    // Compute new region
    updateRegion();

    if (scrollHandler_ != null)
      scrollHandler_.updateScrollBars();
  }


  
  /**
   * Zoom a specified amount around center of viewport.
   * 
   * @param zoomFactor  Zoom factor. Zoom in with factor < 1.0 and
   *                    out with factor > 1.0.
   */
  public void zoom (double zoomFactor)
  {
    double x = viewport_.getCenterX();
    double y = viewport_.getCenterY();

    zoom ((int) Math.round (x), (int) Math.round (y), zoomFactor);
  }
    
  

  /**
   * Zoom a specific amount using specified point as fixed.
   *
   * <ul>
   * <li> Zoom in: zoom (x, y, 0.9);
   * <li> Zoom out: zoom (x, y, 1.1);
   * <li> etc.
   * </ul>
   *
   * @param x           X coordinate of fixed point during zoom.
   * @param y           Y coordinate of fixed point during zoom.   
   * @param zoomFactor  Zoom factor.
   */
  public void zoom (int x, int y, double zoomFactor)
  {
    int x0 = viewport_.getX0();
    int y0 = viewport_.getY0();
    int x1 = viewport_.getX3();
    int y1 = viewport_.getY3();

    double width  = viewport_.getWidth();
    double height = viewport_.getHeight();
    
    x0 += (1.0 - zoomFactor) * (x - x0);
    x1 -= (1.0 - zoomFactor) * (x1 - x);
    
    y0 += (1.0 - zoomFactor) * (y - y0);
    y1 -= (1.0 - zoomFactor) * (y1 - y);

    zoom (x0, y0, x1, y1);
  }

  

  /**
   * Zoom into a specific device area.
   * 
   * @param x0  X value of first corner of zoom rectangle.
   * @param y0  Y value of first corner of zoom rectangle.  
   * @param x1  X value of second corner of zoom rectangle.
   * @param y1  Y value of second corner of zoom rectangle.   
   */
  public void zoom (int x0, int y0, int x1, int y1)
  {
    // Make sure x0,y0 is upper left and x1,y1 is lower right
    if (x1 < x0) {
      int temp = x1;
      x1 = x0;
      x0 = temp;
    }
    
    if (y1 < y0) {
      int temp = y1;
      y1 = y0;
      y0 = temp;
    }

    // Tranform to world
    double w0[] = transformer_.deviceToWorld (x0, y1);
    double w1[] = transformer_.deviceToWorld (x1, y1);    
    double w2[] = transformer_.deviceToWorld (x0, y0);    

    zoom (w0, w1, w2);
  }


  
  /**
   * Zoom into a specified world area.
   * 
   * @param w0  First world coordinate of zoom area [x,y,z].
   * @param w1  Second world coordinate of zoom area [x,y,z].
   * @param w2  Third world coordinate of zoom area [x,y,z].   
   */
  public void zoom (double w0[], double w1[], double w2[])
  {
    // Set new world extent
    currentWorldExtent_.set (w0, w1, w2);

    // Flag entire scene as damaged
    window_.updateDamageArea (getRegion());
    
    // Make sure we keep aspect ratio (if required)
    adjustCurrentWorldExtent();

    // Update the transformer
    transformer_.update (viewport_, currentWorldExtent_);

    // Redraw all affected elements
    window_.redraw();

    // Rerender the graphics
    window_.refresh();

    // Update scrollbars if present
    if (scrollHandler_ != null)
      scrollHandler_.updateScrollBars();
  }
  
  

  /**
   * Unzoom. Unzooming sets the current world extent back to the initial
   * world extent as specified by the client application by setWorldExtent().
   */
  public void unzoom()
  {
    zoom (initialWorldExtent_.get(0),
          initialWorldExtent_.get(1),
          initialWorldExtent_.get(2));
  }



  /**
   * Pan a specific device distance.
   * 
   * @param dx  Distance to pan in x direction.
   * @param dy  Distance to pan in y direction.
   */
  public void pan (int dx, int dy)
  {
    int x0 = viewport_.getX0() - dx;
    int y0 = viewport_.getY0() - dy;
    int x1 = viewport_.getX3() - dx;
    int y1 = viewport_.getY3() - dy;

    zoom (x0, y0, x1, y1);
  }
  

  
  /**
   * Flag the annotation of this scene as valid or invalid. Annotation
   * is set to invalid if annotation is changed somewhere down the tree.
   * This is an instruction to the GWindow to redo the annotation on this
   * scene. When the annotation is redone, this flag is set to valid.
   * 
   * @param isAnnotationValid  True if the annotation of this scene is valid
   *                           false otherwise.
   */
  void setAnnotationValid (boolean isAnnotationValid)
  {
    isAnnotationValid_ = isAnnotationValid;
  }


  
  /**
   * Check if annotation in this scene is valid.
   * 
   * @return  True if the annotation is valid, false otherwise.
   */
  boolean isAnnotationValid()
  {
    return isAnnotationValid_;
  }
  


  /**
   * Compute positions of all text (GText) elements in this scene.
   */
  void computeTextPositions()
  {
    annotator_.reset();
    super.computeTextPositions();
    isAnnotationValid_ = true;
  }


  
  /**
   * Compute positions of all AWT components (GComponent) elements
   * in this scene.
   */
  void computeComponentPositions()
  {
    super.computeComponentPositions();
  }


  
  /**
   * Compute positions of the specified positionals.
   * 
   * @param positionals  Positionals to compute positions of.
   */
  void computePositions (Collection positionals)
  {
    annotator_.computePositions (positionals);
  }

  

  /**
   * Compute positions for positional object that are attached
   * to every vertex of its owner.
   * 
   * @param positional  Positional to compute position for.
   */
  void computeVertexPositions (GPositional positional)
  {
    annotator_.computeVertexPositions (positional);    
  }
  


  /**
   * Instruct this scene to update and respond to the specified
   * scrollbars during zoom.
   * <p>
   * <b>NOTE I:</b> The client application is responsible for laying out
   * the scrollbars in the AWT/Swing GUI. The scrollbars should have no
   * access listeneres nor logic added, as this is controlled by the
   * GScene through the internal GScrollHandler object.
   * <p>
   * <b>NOTE II:</b> Do not put the graphics panel in a JScrollPane and use
   * the JScrollPane scrollbars as input to this method, as a JScrollPane
   * contains scroll logic that interfer with the internal GScene logic.
   * The correct approach is to create horizontal and vertical JScrollBar
   * explicitly.
   * <p>
   * Specifying both horizontal and vertical scrollbar as <em>null</em>
   * turns off scroll handling in this scene.
   * 
   * @param hScrollBar  Horizontal scrollbar (or null if a horizontal
   *                    scrollbar is not used).
   * @param hScrollBar  Vertical scrollbar (or null if a vertical scrollbar
   *                    is not used).
   */
  public void installScrollHandler (Adjustable hScrollBar,
                                    Adjustable vScrollBar)
  {
    if (hScrollBar == null && vScrollBar == null)
      scrollHandler_ = null;
    else
      scrollHandler_ = new GScrollHandler (this, hScrollBar, vScrollBar);
  }
}
