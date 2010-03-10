package no.geosoft.cc.graphics;



import java.util.HashMap;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.FileOutputStream;

import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.LayoutManager;
import java.awt.Paint;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.font.TextLayout;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

import javax.swing.JComponent;
import javax.swing.RepaintManager;

import no.geosoft.cc.geometry.Rect;
import no.geosoft.cc.geometry.Region;
import no.geosoft.cc.io.GifEncoder;



/**
 * G rendering engine.
 * <p>
 * The canvas is the AWT component where the graphics is displayed.
 *
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
class GCanvas extends JComponent
  implements Printable, LayoutManager, MouseListener, MouseMotionListener,
             ComponentListener
{
  private final GWindow         window_;
  private final RepaintManager  repaintManager_;

  private Graphics graphics_;
  private Image    backBuffer_;
  private Area     clipArea_;
  private Rect     cleared_;

  
  /**
   * Create a graphic canvas.
   * 
   * @param window  GWindow of this canvas.
   */
  GCanvas (GWindow window)
  {
    window_ = window;
    
    setLayout (this);
    
    repaintManager_ = RepaintManager.currentManager (this);

    // We handle double buffering manually
    repaintManager_.setDoubleBufferingEnabled (false);

    backBuffer_ = null;
    clipArea_   = null;
    cleared_    = null;

    addMouseListener (this);
    addMouseMotionListener (this);
    addComponentListener (this);
  }

  

  /**
   * Override the JPanel default to indicate that this canvas is
   * double buffered.
   * 
   * @return  True always.
   */
  public boolean isDoubleBuffered()
  {
    return true;
  }

  

  /**
   * Override the JPanel default repaint method and do nothing.
   * TODO: Check this.
   */
  public void repaint()
  {
  }

  

  /**
   * Override the JPanel update method and do nothing.
   * TODO: Check this.
   * 
   * @param graphics  The Graphics2D instance.
   */
  public void update (Graphics graphics)
  {
  }


  
  /**
   * Create a new image back buffer. The back buffes has the size
   * of the cnvas and is recreated each time the canvas size
   * changes (on resize events).
   */
  private void createBackBuffer()
  {
    int width  = getWidth();
    int height = getHeight();
    
    if (width <= 0 || height <= 0) return;
    
    // If component has no parent, null is returned
    backBuffer_ = createImage (width, height);

    // Fill back buffer with background color
    if (backBuffer_ != null) {
      Graphics2D canvas = (Graphics2D) backBuffer_.getGraphics();
      canvas.setColor (getBackground());
      canvas.fillRect (0, 0, width, height);
    }
  }
  

  
  /**
   * Paint this component by copying the back buffer into the
   * front buffer.
   * 
   * @param graphics  The Java2D graphics instance.
   */
  public void paintComponent (Graphics graphics)
  {
    if (backBuffer_ == null || graphics == null || cleared_ == null)
      return;

    graphics_ = graphics;

    // TODO: Is it possible to save the rect from clear() and
    // only copy this part??

    // Copy the current back buffer to the front buffer
    Graphics2D frontBuffer = (Graphics2D) graphics_;
    frontBuffer.drawImage (backBuffer_,
                           cleared_.x, cleared_.y,
                           cleared_.width, cleared_.height,
                           cleared_.x, cleared_.y,
                           cleared_.width, cleared_.height,
                           this);

    // TODO: These work as well. Which method is better?
    // frontBuffer.drawImage (backBuffer_, 0, 0, getWidth(), getHeight(), this);
    // frontBuffer.drawImage (backBuffer_, null, this);
    // frontBuffer.drawImage (backBuffer_, 0, 0, this);    
    
    // Due to a bug in the repaint manager
    // TODO: Check this
    if (repaintManager_.getDirtyRegion (this).isEmpty())
      return;

    // Mark as valid
    repaintManager_.paintDirtyRegions();
    repaintManager_.markCompletelyClean (this);
    cleared_ = null;
  }

  

  /**
   * Clear the specified area in the back buffer.
   * 
   * @param rectangle  Rectangle area to clear in the back buffer.
   */
  void clear (Rect rectangle)
  {
    if (graphics_ == null || backBuffer_ == null)
      return;

    Graphics2D canvas = (Graphics2D) backBuffer_.getGraphics();

    // Clear by filling rectangle with background color
    canvas.setClip (clipArea_);    
    canvas.setColor (getBackground());
    canvas.fillRect (rectangle.x, rectangle.y,
                     rectangle.width, rectangle.height);
  }

  

  /**
   * Refresh this canvas.
   */
  void refresh()
  {
    paintComponent (getGraphics());
  }
  

  
  /**
   * Set clip area for upcomming draw operations.
   * 
   * @param region  Region to use as clip area.
   */
  void setClipArea (Region region)
  {
    clipArea_ = region == null || region.isEmpty() ? null : region.createArea();
  }
  

  

  /**
   * Render the specified polyline into back buffer using the
   * specified style.
   * 
   * @param x      Polyline x coordinates.
   * @param y      Polyline y coordinates.
   * @param style  Style used for rendering.
   */
  void render (int x[], int y[], GStyle style)
  {
    if (backBuffer_ == null)
      createBackBuffer();

    if (backBuffer_ == null)
      return;

    Graphics2D canvas = (Graphics2D) backBuffer_.getGraphics();

    canvas.setRenderingHint (RenderingHints.KEY_ANTIALIASING,
                             style.isAntialiased() ?
                             RenderingHints.VALUE_ANTIALIAS_ON :
                             RenderingHints.VALUE_ANTIALIAS_OFF);
    canvas.setColor (style.getForegroundColor());
    canvas.setStroke (style.getStroke());
    canvas.setClip (clipArea_);
    
    Paint paint = style.getPaint();
    if (paint != null) {
      Paint defaultPaint = canvas.getPaint();
      canvas.setPaint (paint);
      canvas.fill (new Polygon (x, y, x.length));
      canvas.setPaint (defaultPaint);
      if (style.isLineVisible())
        canvas.drawPolyline (x, y, x.length);      
    }
    else {
      if (style.isLineVisible())
        canvas.drawPolyline (x, y, x.length);
    }
  }

  

  /**
   * Render the specified text element into back buffer using the
   * specified style.
   * 
   * @param text   Text to render.
   * @param style  Style used for rendering.
   */
  void render (GText text, GStyle style)
  {
    if (backBuffer_ == null) return;

    String string = text.getText();
    if (string == null || string.length() == 0) return;
    
    Graphics2D  canvas = (Graphics2D) backBuffer_.getGraphics();

    canvas.setRenderingHint (RenderingHints.KEY_TEXT_ANTIALIASING,
                             style.isAntialiased() ?
                             RenderingHints.VALUE_TEXT_ANTIALIAS_ON :
                             RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
    canvas.setFont (style.getFont());

    int fontSize = style.getFont().getSize();
    
    Color foregroundColor = style.getForegroundColor();
    Color backgroundColor = style.getBackgroundColor();

    Rect rectangle = text.getRectangle();
    
    // Draw box
    if (backgroundColor != null) {
      canvas.setColor (backgroundColor);
      canvas.fillRect (rectangle.x, rectangle.y,
                       rectangle.width, rectangle.height);
    }
    
    // Draw text, center it in the rectangle box
    canvas.setColor (foregroundColor);

    TextLayout layout = new TextLayout (text.getText(), style.getFont(),
                                        canvas.getFontRenderContext());
    Rectangle2D bounds = layout.getBounds();

    double textWidth  = bounds.getWidth();
    double textHeight = bounds.getHeight();

    int x = rectangle.x +
            (int) Math.round ((rectangle.width - textWidth) / 2.0) -
            (int) Math.floor (bounds.getX());
    int y = rectangle.y +
            (int) Math.round ((rectangle.height - textHeight) / 2.0) -
            (int) Math.floor (bounds.getY());
            
    layout.draw (canvas, (float) x, (float) y);    
  }

  

  /**
   * Render the specified image into back buffer.
   * 
   * @param image  Image to render.
   */
  void render (GImage image)
  {
    Graphics2D  canvas = (Graphics2D) backBuffer_.getGraphics();

    Rect rectangle = image.getRectangle();
    canvas.drawImage (image.getImage(), rectangle.x, rectangle.y, this);
  }

  
  
  /**
   * Render the specified image at every vertex along the specified
   * polyline.
   *
   * @param x      Polyline x components.
   * @param y      Polyline y components.   
   * @param image  Image to render.
   */
  void render (int[] x, int[] y, GImage image)
  {
    Graphics2D  canvas = (Graphics2D) backBuffer_.getGraphics();

    // The image rectangle x,y holds delta values for positioning
    int dx = image.getRectangle().x;
    int dy = image.getRectangle().y;

    int nPoints = x.length;
    for (int i = 0; i < nPoints; i++)
      canvas.drawImage (image.getImage(), x[i] + dx, y[i] + dy, this);
  }

  

  /**
   * Position the specified AWT component within this JPanel.
   * 
   * @param component  AWT component to position.
   */
  void render (GComponent component)
  {
    Component c = component.getComponent();

    c.setLocation (component.getRectangle().x,
                   component.getRectangle().y);

    if (!isAncestorOf (c))
      add (c);
  }
  
  

  /**
   * Utility method for computing the rectangle bounding box of
   * a rendered string using the specified font.
   * 
   * @param string  Sample string.
   * @param font    Font to use.
   * @return        Rectangle bounding box of rendered string.
   */
  Rect getStringBox (String string, Font font)
  {
    Graphics2D graphics = (Graphics2D) getGraphics();

    // For some reason the graphics is not always set
    if (graphics == null)
      return new Rect (0, 0, 0, 0);

    TextLayout textLayout = new TextLayout (string, font, 
                                            graphics.getFontRenderContext());
    Rectangle2D bounds = textLayout.getBounds();

    int width  = (int) Math.ceil (bounds.getWidth());
    int height = (int) Math.ceil (bounds.getHeight());
    
    return new Rect (0, 0, width, height);
  }



  /**
   * From the Printable interface. Print the present canvas.
   * 
   * @param graphics     The paper graphics.
   * @param pageFormat   Page format (not used).
   * @param pageIndex    Page index (not used).
   * @return             Printable.PAGE_EXISTS.
   */
  public int print (Graphics graphics, PageFormat pageFormat, int pageIndex)
  {
    if (pageIndex > 0) return Printable.NO_SUCH_PAGE;  
    
    Graphics2D paper = (Graphics2D) graphics;
    paper.drawImage (backBuffer_, 0, 0, getWidth(), getHeight(), this);

    return Printable.PAGE_EXISTS;
  }
  


  /**
   * Save the current graphics as a gif file.
   * 
   * @param file  File to store in.
   */
  void saveAsGif (File file)
    throws IOException
  {
    try {
      GifEncoder gifEncoder = new GifEncoder (backBuffer_);
      OutputStream stream = new FileOutputStream (file);
      gifEncoder.write (stream);
      stream.close();
    }
    catch (AWTException exception) {
      throw new IOException ("GIF encoder AWT error");
    }
  }



  /**
   * Print the canvas content.
   * 
   * @return  True if no exception was caught, false otherwise.
   */
  boolean print()
  {
    try {
      PrinterJob printerJob = PrinterJob.getPrinterJob();
      printerJob.setPrintable (this);
      printerJob.print();
      return true;
    }
    catch (PrinterException exception) {
      return false;
    }
  }
  

  
  /**
   * Implied by LayoutManager
   * 
   * @param name       Name of component to add.
   * @param component  Component to add.
   */
  public void addLayoutComponent (String name, Component component)
  {
  }

  
  
  /**
   * Implied by LayoutManager. Layout the specified container. All components
   * are positined at thir specified x, y location as determined by
   * the GAnnotator.
   * 
   * @param container  Container to layout.
   */
  public void layoutContainer (Container container)
  {
    Component[] components = container.getComponents();

    for (int i = 0; i < components.length; i++) {
      Component component = components[i];
      
      int x      = component.getX();
      int y      = component.getY();
      int width  = component.getPreferredSize().width;
      int height = component.getPreferredSize().height;

      component.setBounds (x, y, width, height);
    }
  }


  
  /**
   * Implied by LayoutManager. Return minimum layout size.
   * 
   * @param container  Component to return minimum layout size of.
   * @return           Size of canvas window.
   */
  public Dimension minimumLayoutSize (Container container)
  {
    return new Dimension (getWidth(), getHeight());
  }
  

  
  /**
   * Implied by LayoutManager. Return maximum layout size.
   * 
   * @param container  Component to return maximum layout size of.
   * @return           Size of canvas window.
   */
  public Dimension preferredLayoutSize (Container container)
  {
    return new Dimension (getWidth(), getHeight());    
  }


  
  /**
   * Implied by LayoutManager.
   * 
   * @param component  Component to remove.
   */
  public void removeLayoutComponent (Component component)
  {
  }

  


  /**
   * Implied by MouseListener. Not used.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mouseClicked (MouseEvent event)
  {
  }


  
  /**
   * Method called when the pointer enters this window. If an interaction
   * is installed, pass a FOCUS_IN event to it.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mouseEntered (MouseEvent event)
  {
    window_.mouseEntered (event.getX(), event.getY());
  }


  
  /**
   * Method called when the pointer exits this window. If an interaction
   * is installed, pass a FOCUS_OUT event to it.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mouseExited (MouseEvent event)
  {
    window_.mouseExited (event.getX(), event.getY());
  }


  
  /**
   * Method called when a mouse pressed event occurs in this window.
   * If an interaction is installed, pass a BUTTON*_DOWN event to it.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mousePressed (MouseEvent event)
  {
    int modifiers = event.getModifiers();
    int buttonEvent;

    if      (modifiers == event.BUTTON1_MASK)
      buttonEvent = GWindow.BUTTON1_DOWN;
    else if (modifiers == event.BUTTON2_MASK)
      buttonEvent = GWindow.BUTTON2_DOWN;
    else
      buttonEvent = GWindow.BUTTON3_DOWN;

    window_.mousePressed (buttonEvent, event.getX(), event.getY());
  }


  
  /**
   * Method called when a mouse release event occurs in this window.
   * If an interaction is installed, pass a BUTTON*_UP event to it.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mouseReleased (MouseEvent event)
  {
    int modifiers = event.getModifiers();
    int buttonEvent;

    if      (modifiers == event.BUTTON1_MASK)
      buttonEvent = GWindow.BUTTON1_UP;
    else if (modifiers == event.BUTTON2_MASK)
      buttonEvent = GWindow.BUTTON2_UP;
    else
      buttonEvent = GWindow.BUTTON3_UP;

    window_.mouseReleased (buttonEvent, event.getX(), event.getY());
  }

  

  /**
   * Method called when the mouse is dragged (moved with button pressed) in
   * this window. If an interaction is installed, pass a BUTTON*_DRAG
   * event to it.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mouseDragged (MouseEvent event)
  {
    int modifiers = event.getModifiers();
    int buttonEvent;

    if      (modifiers == event.BUTTON1_MASK)
      buttonEvent = GWindow.BUTTON1_DRAG;
    else if (modifiers == event.BUTTON2_MASK)
      buttonEvent = GWindow.BUTTON2_DRAG;
    else
      buttonEvent = GWindow.BUTTON3_DRAG;

    window_.mouseDragged (buttonEvent, event.getX(), event.getY());
  }


  
  /**
   * Method called when the mouse is moved inside this window.
   * If an interaction is installed, pass a MOTION event to it.
   * 
   * @param event  Mouse event trigging this method.
   */
  public void mouseMoved (MouseEvent event)
  {
    window_.mouseMoved (event.getX(), event.getY());
  }



  /**
   * Implied by ComponentListener. Not used.
   * 
   * @param event  Event trigging this method.
   */
  public void componentHidden (ComponentEvent event) {}



  /**
   * Implied by ComponentListener. Not used.
   * 
   * @param event  Event trigging this method.
   */
  public void componentMoved (ComponentEvent event) {}


  
  /**
   * Implied by ComponentListener. Not used.
   * 
   * @param event  Event trigging this method.
   */
  public void componentShown (ComponentEvent event) {}


  
  /**
   * Called when the AWT component is resized.
   * 
   * @param event  Resize event.
   */
  public void componentResized (ComponentEvent event)
  {
    createBackBuffer();
    cleared_ = new Rect (0, 0, getWidth(), getHeight());

    window_.resize();
  }
}
