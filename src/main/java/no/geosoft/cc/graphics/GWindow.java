package no.geosoft.cc.graphics;



import java.awt.Color;
import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import no.geosoft.cc.geometry.Region;
import no.geosoft.cc.geometry.Rect;
import no.geosoft.cc.geometry.Geometry;



/**
 * GWindow is the top level graphics node and holder of GScene nodes
 * (node containing world-to-device transformation). The GWindow is
 * linked to the GUI through its canvas object.
 * <p>
 * Typical usage:
 *
 * <pre>
 *   // Some Swing component to hold the graphics 
 *   JPanel panel = new JPanel();
 *   panel.setLayout (new BorderLayout());
 * 
 *   // Create the window and attach to GUI
 *   GWindow window = new GWindow (Color.WHITE);
 *   panel.add (window.getCanvas(), BorderLayout.CENTER);
 * </pre>
 *
 * GWindow is also the holder of the current "interaction" object
 * communicating mouse events between the back-end AWT component and
 * the client application.
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GWindow
{
  public static final int ABORT                = 1;
  public static final int MOTION               = 2;

  public static final int BUTTON1_DOWN         = 3;
  public static final int BUTTON1_DRAG         = 4;
  public static final int BUTTON1_UP           = 5;
  public static final int BUTTON1_DOUBLE_CLICK = 6;   // TODO
  
  public static final int BUTTON2_DOWN         = 7;
  public static final int BUTTON2_DRAG         = 8;
  public static final int BUTTON2_UP           = 9;
  public static final int BUTTON2_DOUBLE_CLICK = 10;  // TODO

  public static final int BUTTON3_DOWN         = 11;
  public static final int BUTTON3_DRAG         = 12;
  public static final int BUTTON3_UP           = 13;
  public static final int BUTTON3_DOUBLE_CLICK = 14;  // TODO

  public static final int FOCUS_IN             = 15;
  public static final int FOCUS_OUT            = 16;    

  private final GCanvas  canvas_;
  
  private int           width_;
  private int           height_;
  private GInteraction  interaction_;
  private List          scenes_;
  private GScene        interactionScene_;
  private Region        damageRegion_;


  
  /**
   * Create a new graphic window with the specified background color.
   * <p>
   * The window contains a JComponent canvas which should be added
   * to a container widget in the GUI.
   */
  public GWindow (Color backgroundColor)
  {
    // Rendering engine
    canvas_ = new GCanvas (this);
    if (backgroundColor != null)
      canvas_.setBackground (backgroundColor);
    
    interaction_  = null;
    scenes_       = new ArrayList();
    damageRegion_ = new Region();

    // Cannot set 0 initially as resize is computed relative to current
    width_  = 100;
    height_ = 100;
  }


  
  /**
   * Create a new graphics window with default background color.
   */
  public GWindow()
  {
    this (null);
  }
  

  
  /**
   * Return rendering canvas of this window. This is the component that
   * should be added to the client GUI hierarchy.
   * 
   * @return  Rendering canvas of this window.
   */
  public Component getCanvas()
  {
    return canvas_;
  }
  


  /**
   * Return width of this window.
   * 
   * @return  Width of this window.
   */
  public int getWidth()
  {
    return width_;
  }


  
  /**
   * Return height of this window.
   * 
   * @return  Height of this window.
   */
  public int getHeight()
  {
    return height_;
  }
  


  /**
   * Return the current interaction of this window.
   * 
   * @return  Current interaction of this window (or null if none installed).
   */
  GInteraction getInteraction()
  {
    return interaction_;
  }

  

  /**
   * Add a scene to this window. A window may have more than one scene.
   * The first scene added is rendered first (i.e. it appears in the
   * background of the screen) and so on.
   * 
   * @param scene  Scene to add.
   */
  void addScene (GScene scene)
  {
    scenes_.add (scene);
  }


  
  /**
   * Return all scenes of this window. If no scenes are attached to this
   * window, an empty (non-null) list is returned.
   * 
   * @return  All scenes of this window.
   */
  public List getScenes()
  {
    return scenes_;
  }


  
  /**
   * Return the first scene of this window (or null if no scenes are
   * attached to this window). This method is a convenience where the
   * client application knows that are exactly one scene in the window
   * (which in many practical cases will be the case).
   * 
   * @return  The first scene of this window (or null if none).
   */
  public GScene getScene()
  {
    return scenes_.size() > 0 ? (GScene) scenes_.get (0) : null;
  }
  

  
  /**
   * Find scene at the specified location. If there are more than one scene
   * at the specified location, select the front most.
   * 
   * @param x  X coordinate of location of scene.
   * @param y  Y coordinate of location of scene.
   * @return   Front most scene at specfied location (or null if none).
   */
  private GScene getScene (int x, int y)
  {
    for (int i = scenes_.size()-1; i >= 0; i--) {
      GScene scene = (GScene) scenes_.get (i);
      GViewport viewport = scene.getViewport();
      if (Geometry.isPointInsidePolygon (new int[] {viewport.getX0(),
                                                    viewport.getX1(),
                                                    viewport.getX3(),
                                                    viewport.getX2()},
                                         new int[] {viewport.getY0(),
                                                    viewport.getY1(),
                                                    viewport.getY3(),
                                                    viewport.getY2()},
                                         x, y))
        return scene;
    }
                                                
    return null;
  }



  /**
   * Find a GObject based on specified name. Search depth first.
   * 
   * @param name  Name of object to search for.
   * @return      First object with matching name, or null if none found.
   */
  public GObject find (String name)
  {
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      GObject object = scene.find (name);
      if (object != null) return object;
    }

    return null;
  }
  

  
  /**
   * Find a GObject based on user data. Search depth first.
   * 
   * @param name  User data of object to search for.
   * @return      First object with matching user data, or null if none found.
   */
  public GObject find (Object userData)
  {
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      GObject object = scene.find (userData);
      if (object != null) return object;
    }

    return null;
  }
  

  
  /**
   * Return region of damage since last refresh.
   * 
   * @return  Damages region since last refresh.
   */
  Region getDamageRegion()
  {
    return damageRegion_;
  }
  


  /**
   * Add the specified region to the current damage region.
   * 
   * @param region  Region to add to damage.
   */
  void updateDamageArea (Region region)
  {
    damageRegion_.union (region);

    // It doesn't really matter if the damage region is larger than
    // the actual damage, but we'd like to keep it as small as possible
    // so we affect as few objects as possible during redraw.
    // However, this come as a tradeof with region complexity, and if it
    // becomes to complex we choose to "callapse" it, i.e. exchange it
    // with its outline extent.
    if (damageRegion_.getNRectangles() > 100)
      damageRegion_.collapse();
  }

  

  /**
   * Add the specified rectangle to the current damage region.
   * 
   * @param rectangle  Rectangle to add to damage.
   */
  void updateDamageArea (Rect rectangle)
  {
    updateDamageArea (new Region (rectangle));
  }

  

  /**
   * Install the specified interaction on this window. As a window
   * can administrate only one interaction at the time, the current
   * interaction (if any) is first stopped.
   * 
   * @param interaction  Interaction to install and start.
   */
  public void startInteraction (GInteraction interaction)
  {
    if (interaction_ != null)
      stopInteraction();

    interaction_      = interaction;
    interactionScene_ = null;
  }


  
  /**
   * Stop the current interaction. The current interaction will get
   * an ABORT event so it has the possibility to do cleanup. If no
   * interaction is installed, this method has no effect. 
   */
  public void stopInteraction()
  {
    // Nothing to do if no current interaction
    if (interaction_ == null) return;
    
    interaction_.event (null, ABORT, 0, 0);
    interaction_      = null;
    interactionScene_ = null;    
  }
  

  
  /**
   * Ensure correct regions for all objects. Only objects with its
   * isRegionValid_ flag set to false (and their parents) will be
   * recomputed.
   */
  void computeRegion()
  {
    // This is default setting from window point of view
    int visibilityMask = GObject.DATA_VISIBLE       | 
                         GObject.ANNOTATION_VISIBLE |
                         GObject.SYMBOLS_VISIBLE;

    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      scene.computeRegion (visibilityMask);
    }
  }
  


  /**
   * Force a complete redraw of all visible elements.
   * <p>
   * Normally this method is called automatically when needed
   * (typically on retransformations).
   * A client application <em>may</em> call this method explicitly
   * if some external factor that influence the graphics has been
   * changed. However, beware of the performance overhead of such
   * an approach, and consider calling GObject.redraw() on the
   * affected objects instead.
   */
  public void redraw()
  {
    // This is default setting from window point of view
    int visibilityMask = GObject.DATA_VISIBLE       | 
                         GObject.ANNOTATION_VISIBLE |
                         GObject.SYMBOLS_VISIBLE    |
                         GObject.WIDGETS_VISIBLE;

    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      scene.redraw (visibilityMask);
    }
  }

  

  /**
   * Refresh the graphics scene. Only elements that has been changed 
   * since the last refresh are affected.
   */
  public void refresh()
  {
    // This is default setting from window point of view
    int visibilityMask = GObject.DATA_VISIBLE       | 
                         GObject.ANNOTATION_VISIBLE |
                         GObject.SYMBOLS_VISIBLE    |
                         GObject.WIDGETS_VISIBLE;

    // Check if annotation has changed
    boolean isAnnotationUpdated = false;
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      if (!scene.isAnnotationValid()) {
        isAnnotationUpdated = true;
        break;
      }
    }

    // Return here if nothing has changed
    if (!isAnnotationUpdated && damageRegion_.isEmpty())
      return;

    // Compute positions of all annotations
    computeTextPositions();

    // Compute positions of all integrated AWT components
    computeComponentPositions();

    // Compute region for all elements
    computeRegion();

    // Clip damage to viewport
    Region viewportRegion = new Region();
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      viewportRegion.union (scene.getRegion());
    }
    damageRegion_.intersect (viewportRegion);
    
    // Clear the damaged area in the canvas
    canvas_.setClipArea (damageRegion_);
    canvas_.clear (damageRegion_.getExtent());

    Region allDamage = new Region (damageRegion_);
    
    // Rendering pass 1: DATA
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      damageRegion_ = Region.intersect (allDamage, scene.getRegion());
      canvas_.setClipArea (damageRegion_);
      scene.refreshData (visibilityMask);
    }

    // Rendering pass 2: ANNOTATION
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      damageRegion_ = Region.intersect (allDamage, scene.getRegion());
      canvas_.setClipArea (damageRegion_);
      scene.refreshAnnotation (visibilityMask);
    }

    // Rendering pass 3: COMPONENTS
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      damageRegion_ = Region.intersect (allDamage, scene.getRegion());
      canvas_.setClipArea (damageRegion_);
      scene.refreshComponents (visibilityMask);
    }
    
    canvas_.refresh();
    damageRegion_.clear();
  }



  /**
   * Compute all text positions in entire window.
   */
  void computeTextPositions()
  {
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      if (!scene.isAnnotationValid())
        scene.computeTextPositions();
    }
  }



  /**
   * Compute all component (Swing widgets) positions in entire window.
   */
  void computeComponentPositions()
  {
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      // TODO: if (!scene.isAnnotationValid())
      scene.computeComponentPositions();
    }
  }



  /**
   * Method called when the pointer enters this window. If an interaction
   * is installed, pass a FOCUS_IN event to it.
   * 
   * @param x  X position of mouse.
   * @param y  Y position of mouse.   
   */
  void mouseEntered (int x, int y)
  {
    if (interaction_ == null) return;
    interaction_.event (getScene (x, y), FOCUS_IN, x, y);
  }


  
  /**
   * Method called when the pointer exits this window. If an interaction
   * is installed, pass a FOCUS_OUT event to it.
   * 
   * @param x  X position of mouse.
   * @param y  Y position of mouse.   
   */
  void mouseExited (int x, int y)
  {
    if (interaction_ == null) return;
    interaction_.event (getScene (x, y), FOCUS_OUT, x, y);
  }


  
  /**
   * Method called when a mouse pressed event occurs in this window.
   * If an interaction is installed, pass a BUTTON*_DOWN event to it.
   * 
   * @param buttonEvent  Button event trigging this method.
   * @param x            X position of mouse.
   * @param y            Y position of mouse.   
   */
  void mousePressed (int buttonEvent, int x, int y)
  {
    if (interaction_ == null) return;
    interactionScene_ = getScene (x, y);
    interaction_.event (interactionScene_, buttonEvent, x, y);
  }


  
  /**
   * Method called when a mouse release event occurs in this window.
   * If an interaction is installed, pass a BUTTON*_UP event to it.
   * 
   * @param buttonEvent  Button event trigging this method.
   * @param x            X position of mouse.
   * @param y            Y position of mouse.   
   */
  void mouseReleased (int buttonEvent, int x, int y)
  {
    if (interaction_ == null) return;
    interaction_.event (interactionScene_, buttonEvent, x, y);
  }

  

  /**
   * Method called when the mouse is dragged (moved with button pressed) in
   * this window. If an interaction is installed, pass a BUTTON*_DRAG
   * event to it.
   * 
   * @param buttonEvent  Button event trigging this method.
   * @param x            X position of mouse.
   * @param y            Y position of mouse.   
   */
  void mouseDragged (int buttonEvent, int x, int y)
  {
    if (interaction_ == null) return;
    interaction_.event (interactionScene_, buttonEvent, x, y);
  }


  
  /**
   * Method called when the mouse is moved inside this window.
   * If an interaction is installed, pass a MOTION event to it.
   * 
   * @param x  X position of mouse.
   * @param y  Y position of mouse.   
   */
  void mouseMoved (int x, int y)
  {
    if (interaction_ == null) return;
    interaction_.event (getScene (x, y), MOTION, x, y);
  }


  
  /**
   * Called when the window is resized. Reset the dimension variables
   * and resize scenes accordingly.
   */
  void resize()
  {
    // Get the new window size
    int width  = canvas_.getWidth();
    int height = canvas_.getHeight();

    // Refuse to resize to zero as we cannot possible resize back
    if (width == 0 || height == 0) return;
    
    // Compute resize factors
    double dx = (double) width  / (double) width_;
    double dy = (double) height / (double) height_;

    // Set new window size
    width_  = width;
    height_ = height;

    // Mark entire window as damaged
    damageRegion_.clear();
    Rect allWindow = new Rect (0, 0, width_, height_);
    damageRegion_.union (allWindow);

    // Resize every scene accordingly
    for (Iterator i = scenes_.iterator(); i.hasNext(); ) {
      GScene scene = (GScene) i.next();
      scene.resize (dx, dy);
    }

    // Recompute geometry
    redraw();

    // Render graphics
    refresh();
  }
  
  
  
  /**
   * Print the current image.
   * 
   * @return  True if no exception was caught, false otherwise.
   */
  public boolean print()
  {
    boolean isOk = canvas_.print();
    return isOk;
  }
  


  /**
   * Store the current graphic image as a GIF file.
   * 
   * @param file  File to store in.
   */
  public void saveAsGif (File file)
    throws IOException
  {
    canvas_.saveAsGif (file);
  }
}
