package no.geosoft.cc.graphics;



import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.lang.ref.WeakReference;
import java.awt.BasicStroke;
import java.awt.Stroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Paint;
import java.awt.GradientPaint;
import java.awt.TexturePaint;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.FileInputStream;

import javax.imageio.ImageIO;



/**
 * Graphics object apparence properties.
 * <p>
 * GStyle can be set on GObject, GSegment and GPositionals (GText, GImage,
 * GComponent). It is optional, and when unset the style of the parent
 * component is applied. This is also the case for the individual properties
 * of the GStyle.
 * <p>
 * Example:
 *
 * <pre>
 *   // Define a style with foreground and background color set
 *   GStyle style1 = new GStyle();
 *   style1.setForegroundColor (Color.RED);
 *   style1.setBackgroundColor (Color.BLUE);
 *
 *   // Create an object and apply style1.
 *   // It will have foreground color RED and background color BLUE
 *   GObject object1 = new GObject();
 *   object1.setStyle (style1);
 *
 *   // Create a sub object without style. style1 is inherited from parent
 *   // It will have foreground color RED and background color BLUE
 *   GObject object2 = new GObject();
 *   object1.add (object2);
 *
 *   // Define new style with foreground color set
 *   GStyle style2 = new GStyle()
 *   style2.setForegroundColor (Color.YELLOW);
 *
 *   // Create a sub object and apply style2.
 *   // It will have foreground color YELLOW and background color BLUE
 *   GObject object3 = new GObject();
 *   object3.setStyle (style2);
 *   object1.add (object3);
 * </pre> 
 * 
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GStyle
  implements Cloneable
{
  private static final int  MASK_FOREGROUNDCOLOR = 1 << 0;
  private static final int  MASK_BACKGROUNDCOLOR = 1 << 1;  
  private static final int  MASK_LINEWIDTH       = 1 << 2;
  private static final int  MASK_FONT            = 1 << 3;
  private static final int  MASK_FILLPATTERN     = 1 << 4;
  private static final int  MASK_LINESTYLE       = 1 << 5;
  private static final int  MASK_CAPSTYLE        = 1 << 6;
  private static final int  MASK_JOINSTYLE       = 1 << 7;
  private static final int  MASK_ANTIALIAS       = 1 << 8;
  private static final int  MASK_GRADIENT        = 1 << 9;

  public static final int   LINESTYLE_SOLID      = 1;
  public static final int   LINESTYLE_DASHED     = 2;
  public static final int   LINESTYLE_DOTTED     = 3;
  public static final int   LINESTYLE_DASHDOT    = 4;
  public static final int   LINESTYLE_INVISIBLE  = 5;

  public static final int   FILL_NONE       = 0;  
  public static final int   FILL_SOLID      = 1;
  public static final int   FILL_10         = 2;
  public static final int   FILL_25         = 3;
  public static final int   FILL_50         = 4;
  public static final int   FILL_75         = 5;
  public static final int   FILL_HORIZONTAL = 6;
  public static final int   FILL_VERTICAL   = 7;
  public static final int   FILL_DIAGONAL   = 8;              

  private final Collection     listeners_;

  private int            validMask_;
  private Color          foregroundColor_;
  private Color          backgroundColor_;  
  private int            lineWidth_;
  private float          dashPattern_[];
  private float          dashOffset_;
  private boolean        isLineVisible_;
  private Font           font_;
  private BufferedImage  fillPattern_;
  private int            fillWidth_;
  private int            fillHeight_;
  private int            fillData_[];
  private Stroke         stroke_;   // Java2D repr of line width and style
  private Paint          paint_;    // Java2D repr of fill pattern  
  private int            capStyle_;
  private int            joinStyle_;
  private float          miterLimit_;
  private boolean        isAntialiased_;
  private Color          gradientColor1_;
  private Color          gradientColor2_;  

  

  /**
   * Create a new style object. The style is initially empty, individual
   * settings are only valid if explicitly set.
   */
  public GStyle()
  {
    listeners_       = new ArrayList();
    
    // Flag everything setting as invalid
    validMask_       = 0;

    // Default settings for all styles
    foregroundColor_ = Color.black;
    backgroundColor_ = null;
    lineWidth_       = 1;
    font_            = Font.decode ("dialog");
    dashPattern_     = null;
    fillPattern_     = null;
    fillData_        = null;
    paint_           = null;
    capStyle_        = BasicStroke.CAP_ROUND;
    joinStyle_       = BasicStroke.JOIN_ROUND;
    miterLimit_      = (float) 0.0;
    dashOffset_      = (float) 0.0;
    isLineVisible_   = true;
    isAntialiased_   = true;
    gradientColor1_  = null;
    gradientColor2_  = null;
  }


  
  /**
   * Create a new style object based on this.
   * 
   * @return  Clone of this style object.
   */
  public Object clone()
  {
    GStyle style = new GStyle();

    style.validMask_ = validMask_;

    style.foregroundColor_ = null;
    if (foregroundColor_ != null) {
      style.foregroundColor_ = new Color (foregroundColor_.getRed(),
                                          foregroundColor_.getGreen(),
                                          foregroundColor_.getBlue(),
                                          foregroundColor_.getAlpha());
    }
    
    style.backgroundColor_ = null;
    if (backgroundColor_ != null) {
      style.backgroundColor_ = new Color (backgroundColor_.getRed(),
                                          backgroundColor_.getGreen(),
                                          backgroundColor_.getBlue(),
                                          backgroundColor_.getAlpha());
    }

    style.lineWidth_ = lineWidth_;

    style.font_ = null;
    if (font_ != null) {
      style.font_ = new Font (font_.getName(),
                              font_.getStyle(), font_.getSize());
    }

    style.dashPattern_ = null;
    if (dashPattern_ != null) {
      style.dashPattern_ = new float[dashPattern_.length];
      for (int i = 0; i < dashPattern_.length; i++)
        style.dashPattern_[i] = dashPattern_[i];
    }
    
    style.fillPattern_  = null; // Lazy initialized

    style.fillData_ = null;    
    if (fillData_ != null) {
      style.fillData_ = new int[fillData_.length];
      for (int i = 0; i < fillData_.length; i++)
        style.fillData_[i] = fillData_[i];
    }

    style.capStyle_       = capStyle_;
    style.joinStyle_      = joinStyle_;
    style.miterLimit_     = miterLimit_;
    style.dashOffset_     = dashOffset_;
    style.isLineVisible_  = isLineVisible_;
    style.isAntialiased_  = isAntialiased_;
    style.gradientColor1_ = gradientColor1_;
    style.gradientColor2_ = gradientColor2_;
    
    return style;
  }
  
  

  /**
   * Update this style based on specified style. Style elements
   * explicitly set in the specified style is copied to this style,
   * other elements are ignored.
   * 
   * @param style  Style to update with.
   */
  void update (GStyle style)
  {
    if (style.isValid (MASK_FOREGROUNDCOLOR))
      setForegroundColor (style.foregroundColor_);
    if (style.isValid (MASK_BACKGROUNDCOLOR))
      setBackgroundColor (style.backgroundColor_);
    if (style.isValid (MASK_LINEWIDTH))
      setLineWidth (style.lineWidth_);
    if (style.isValid (MASK_FONT))
      setFont (style.font_);
    if (style.isValid (MASK_FILLPATTERN)) {
      if (style.fillData_ != null)
        setFillPattern (style.fillWidth_,
                        style.fillHeight_,
                        style.fillData_);
      else
        setFillPattern (style.fillPattern_);
    }
    if (style.isValid (MASK_LINESTYLE)) {
      setLineStyle (style.dashPattern_);
      isLineVisible_ = style.isLineVisible_;
    }
    if (style.isValid (MASK_CAPSTYLE))
      setCapStyle (style.capStyle_);
    if (style.isValid (MASK_JOINSTYLE))
      setJoinStyle (style.joinStyle_);
    if (style.isValid (MASK_ANTIALIAS))
      setAntialiased (style.isAntialiased_);
    if (style.isValid (MASK_GRADIENT))
      setGradient (style.gradientColor1_, style.gradientColor2_);
  }


  
  /**
   * Set the specified style element to valid.
   * 
   * @param validMask  Identifier of style element to set (MASK_...)
   */
  private void setValid (int validMask)
  {
    validMask_ |= validMask;
  }


  
  /**
   * Invalidate the specified style element.
   * 
   * @param validMask  Identifier of style element to invalidate (MASK_...)
   */
  private void setInvalid (int validMask)
  {
    validMask_ &= ~validMask;
  }
  
  

  /**
   * Check if a specified style element is valid.
   * 
   * @param validMask  Identifier of style element to check (MASK_...)
   * @return           True if it is valid, false otherwise.
   */
  private boolean isValid (int validMask)
  {
    return (validMask_ & validMask) != 0;
  }



  /**
   * Set foreground color of this style.
   * 
   * @param foregroundColor  New foreground color.
   */
  public void setForegroundColor (Color foregroundColor)
  {
    if (foregroundColor == null) foregroundColor = Color.black;
    foregroundColor_ = foregroundColor;
    setValid (MASK_FOREGROUNDCOLOR);

    // The fill pattern might have become invalid
    if (fillData_ != null) fillPattern_ = null;

    // Notify all owners    
    notifyListeners();
  }



  /**
   * Unset foreground color of this style.
   */
  public void unsetForegroundColor()
  {
    foregroundColor_ = null;
    setInvalid (MASK_FOREGROUNDCOLOR);

    // The fill pattern might hav become invalid
    if (fillData_ != null) fillPattern_ = null;

    // Notify all owners
    notifyListeners();
  }
  

  
  /**
   * Return the current foreground color of this style.
   * The element is not in use if it is invalid.
   * 
   * @return  Current foreground color.
   */
  public Color getForegroundColor()
  {
    return foregroundColor_;
  }



  /**
   * Set background color.
   * 
   * @param backgroundColor  New background color.
   */
  public void setBackgroundColor (Color backgroundColor)
  {
    backgroundColor_ = backgroundColor;
    setValid (MASK_BACKGROUNDCOLOR);

    // The fill pattern might hav become invalid
    if (fillData_ != null) fillPattern_ = null;

    // Notify all owners
    notifyListeners();
  }

  

  /**
   * Unset background color.
   */
  public void unsetBackgroundColor()
  {
    backgroundColor_ = null;
    setInvalid (MASK_BACKGROUNDCOLOR);

    // The fill pattern might hav become invalid
    if (fillData_ != null) fillPattern_ = null;

    // Notify all owners
    notifyListeners();
  }

  

  /**
   * Return current background color of this style.
   * The element is not in use if it is invalid.   
   * 
   * @return   Current background color of this style.
   */
  public Color getBackgroundColor()
  {
    return backgroundColor_;
  }



  /**
   * Set line width.
   * 
   * @param lineWidth  New line width.
   */
  public void setLineWidth (int lineWidth)
  {
    if (lineWidth < 1) lineWidth = 1;
    lineWidth_ = lineWidth;
    setValid (MASK_LINEWIDTH);
    stroke_ = null;

    // Notify all owners
    notifyListeners();
  }

  

  /**
   * Unset line width.
   */
  public void unsetLineWidth()
  {
    setInvalid (MASK_LINEWIDTH);
    stroke_ = null;

    // Notify all owners
    notifyListeners();
  }

  

  /**
   * Return current line width of this style.
   * The element is not in use if it is invalid.
   * 
   * @return  Current line width of this style.
   */
  public int getLineWidth()
  {
    return lineWidth_;
  }


  
  /**
   * Set font of this style.
   * 
   * @param font  New font.
   */
  public void setFont (Font font)
  {
    if (font == null) font = Font.decode ("dialog");
    font_ = font;
    setValid (MASK_FONT);

    // Notify all owners
    notifyListeners();
  }

  

  /**
   * Unset font of this style.
   */
  public void unsetFont()
  {
    setInvalid (MASK_FONT);

    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Return current font of this style.
   * The element is not in use if it is invalid.
   * 
   * @return  Current font of this style.
   */
  public Font getFont()
  {
    return font_;
  }

  

  /**
   * Set line end cap style of this style. One of BasicStroke.CAP_ROUND
   * (default), BasicStroke.CAP_BUTT, or BasicStroke.CAP_SQUARE.
   * 
   * @param capStyle  New line end cap style.
   */
  public void setCapStyle (int capStyle)
  {
    capStyle_ = capStyle;
    setValid (MASK_CAPSTYLE);
    stroke_ = null;

    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Unset cap style of this style.
   */
  public void unsetCapStyle()
  {
    setInvalid (MASK_CAPSTYLE);

    // Notify all owners
    notifyListeners();
  }
  

  
  /**
   * Return current line end cap style of this style.
   * The element is not in use if it is invalid.   
   * 
   * @return  Current line end cap style of this style. 
   */
  public int getCapStyle()
  {
    return capStyle_;
  }
  


  /**
   * Set line end join style of this style.
   * One of BasicStroke.JOIN_BEVEL, BasicStroke.JOIN_MITTER or
   * BasicStroke.JOIN_ROUND (default).
   * 
   * @param joinStyle  New join style.
   */
  public void setJoinStyle (int joinStyle)
  {
    joinStyle_ = joinStyle;
    setValid (MASK_JOINSTYLE);
    stroke_ = null;

    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Unset join style of this style.
   */
  public void unsetJoinStyle()
  {
    setInvalid (MASK_JOINSTYLE);

    // Notify all owners
    notifyListeners();
  }
  

  
  /**
   * Return current join style of this style.
   * The element is not in use if it is invalid.   
   * 
   * @return  Current join style of this style.
   */
  public int getJoinStyle()
  {
    return joinStyle_;
  }
  

  
  /**
   * Set antialising flag of this style.
   * 
   * @param isAntialiased  Antialiasing on (true) or off (false) (default).
   */
  public void setAntialiased (boolean isAntialiased)
  {
    isAntialiased_ = isAntialiased;
    setValid (MASK_ANTIALIAS);

    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Unset antialias flag.
   */
  public void unsetAntialias()
  {
    setInvalid (MASK_ANTIALIAS);

    // Notify all owners
    notifyListeners();
  }

  
  
  /**
   * Return current antialiasing setting of this style.
   * The element is not in use if it is invalid.   
   * 
   * @return  True if antialiasing on, false otherwise.
   */
  public boolean isAntialiased()
  {
    return isAntialiased_;
  }

  

  /**
   * TODO: This code is experimental and should not yet be used.
   * 
   * @param color1
   * @param color2
   */
  public void setGradient (Color color1, Color color2)
  {
    gradientColor1_ = color1;
    gradientColor2_ = color2;
    setValid (MASK_GRADIENT);

    paint_ = null;
    
    // Notify all owners
    notifyListeners();
  }


  
  public void unsetGradient()
  {
    setInvalid (MASK_GRADIENT);

    // Notify all owners
    notifyListeners();
  }
  
  
  
  /**
   * Set predefined fill pattern of this style.
   * 
   * @param fillType  New fill pattern.
   */
  public void setFillPattern (int fillType)
  {
    int width;
    int height;

    switch (fillType) {
      case FILL_NONE       : width = 0; height = 0; break;
      case FILL_SOLID      : width = 1; height = 1; break;
      case FILL_10         : width = 3; height = 3; break;
      case FILL_25         : width = 2; height = 2; break;
      case FILL_50         : width = 2; height = 2; break;
      case FILL_75         : width = 2; height = 2; break;
      case FILL_HORIZONTAL : width = 1; height = 2; break;
      case FILL_VERTICAL   : width = 2; height = 1; break;
      case FILL_DIAGONAL   : width = 3; height = 3; break;
      default              : return; // Unknown fill type
    }
    
    int data[] = new int [width * height];
    
    // Set bits specified
    switch (fillType) {
      case FILL_SOLID      :
      case FILL_10         :
      case FILL_25         :
      case FILL_HORIZONTAL :
      case FILL_VERTICAL   : 
        data[0] = 1;
        break;

      case FILL_50 :
        data[0] = 1;
        data[3] = 1;
        break;
        
      case FILL_75 :
        data[1] = 1;
        data[2] = 1;
        data[3] = 1;
        break;

      case FILL_DIAGONAL : 
        data[0] = 1;
        data[4] = 1;
        data[8] = 1;
        break;
    }

    setFillPattern (width, height, data);
  }

  

  /**
   * Set custom fill pattern of this style.
   * 
   * @param width   Tile width.
   * @param height  Tile height.
   * @param data    Pattern data (0s and 1s indicating set/unset).
   */
  public void setFillPattern (int width, int height, int data[])
  {
    fillWidth_  = width;
    fillHeight_ = height;
    
    int size = width * height;
    fillData_ = size > 0 ? new int[size] : null;

    for (int i = 0; i < size; i++)
      fillData_[i] = data != null && data.length > i ? data[i] : 0;

    // Lazily created when needed
    fillPattern_ = null;
    paint_       = null;

    setValid (MASK_FILLPATTERN);    

    // Notify all owners
    notifyListeners();
  }

  

  /**
   * Set image as fill pattern.
   * 
   * @param image  Image to use as fill pattern.
   */
  public void setFillPattern (BufferedImage image)
  {
    fillData_    = null;
    fillPattern_ = image;
    setValid (MASK_FILLPATTERN);    
    paint_ = null;
    
    // Notify all owners
    notifyListeners();
  }



  /**
   * Set image as fill pattern.
   * TODO: Cache the file name and create the image lazy   
   * 
   * @param fileName  File name of image.
   */
  public void setFillPattern (String fileName)
  {
    try {
      InputStream stream = new BufferedInputStream
                           (new FileInputStream (fileName));
      fillPattern_ = ImageIO.read (stream);
      paint_ = null;
      setValid (MASK_FILLPATTERN);      

      // Notify all owners
      notifyListeners();
    }
    catch (Exception exception) {
      exception.printStackTrace();
    }
  }
  
  

  /**
   * Unset fill pattern.
   */
  public void unsetFillPattern()
  {
    setInvalid (MASK_FILLPATTERN);

    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Return current fill pattern.
   * 
   * @return  Current fill pattern.
   */
  public BufferedImage getFillPattern()
  {
    return fillPattern_;
  }
  

  
  /**
   * Set custom line style. Dash pattern consists of and array of leg
   * lengths of line on and line off respectively. 
   * 
   * @param dashPattern  New dash pattern.
   */
  public void setLineStyle (float dashPattern[])
  {
    // Copy dash pattern locally
    if (dashPattern != null) {
      dashPattern_ = new float[dashPattern.length];
      System.arraycopy (dashPattern, 0, dashPattern_, 0, dashPattern.length);
      isLineVisible_ = true;      
    }
    else
      dashPattern_ = null;

    setValid (MASK_LINESTYLE);
    stroke_ = null;  // Create this lazily when needed
    
    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Set predefined line style of this style.
   * 
   * @param lineStyle  New line style.
   */
  public void setLineStyle (int lineStyle)
  {
    float dashPattern[] = null;
    
    switch (lineStyle) {
      case LINESTYLE_SOLID :
        isLineVisible_ = true;
        break;
        
      case LINESTYLE_DASHED :
        dashPattern = new float[2];
        dashPattern[0] = (float) 5.0;
        dashPattern[1] = (float) 5.0;
        break;

      case LINESTYLE_DOTTED :
        dashPattern = new float[2];
        dashPattern[0] = (float) 2.0;
        dashPattern[1] = (float) 5.0;
        break;

      case LINESTYLE_DASHDOT :
        dashPattern = new float[4];
        dashPattern[0] = (float) 8.0;
        dashPattern[1] = (float) 3.0;
        dashPattern[2] = (float) 2.0;
        dashPattern[3] = (float) 3.0;        
        break;

      case LINESTYLE_INVISIBLE :
        isLineVisible_ = false;
        break;
    }
    
    setLineStyle (dashPattern);    
  }
  

  
  /**
   * Unset line style.
   */
  public void unsetLineStyle()
  {
    setInvalid (MASK_LINESTYLE);
    stroke_ = null;  // Create this lazily when needed    

    // Notify all owners
    notifyListeners();
  }


  
  /**
   * Return current line style of this style.
   * The element is not in use if it is invalid.
   * 
   * @return  Current line style of this style.
   */
  public float[] getLineStyle()
  {
    return dashPattern_;
  }
  
  
  
  /**
   * Create a Stroke object based on the line width, cap style, join style,
   * miter limit (0.0), dash pattern and dash offset (0.0) of this style.
   * 
   * @return  Stroke for this style.
   */
  Stroke getStroke()
  {
    // Lazy creation
    if (stroke_ == null) {

      // Dash pattern is created based on a line width = 1. For wider
      // lines we increase the dash accordingly
      float[] dashPattern = null;
      if (dashPattern_ != null) {
        dashPattern = new float[dashPattern_.length];
        for (int i = 0; i < dashPattern_.length; i++)
          dashPattern[i] = dashPattern_[i] * lineWidth_;
      }
        
      stroke_ = new BasicStroke (lineWidth_, capStyle_, joinStyle_,
                                 miterLimit_, dashPattern, dashOffset_);
    }
    
    return stroke_;
  }

  

  /**
   * Create a paint object based on the fill pattern of this style.
   * 
   * @return  Paint object for this style.
   */
  Paint getPaint()
  {
    // Generate the image
    if (fillPattern_ == null && fillData_ != null) {
      
      // Create the image that represent the pattern
      fillPattern_ = new BufferedImage (fillWidth_, fillHeight_,
                                        BufferedImage.TYPE_INT_ARGB);

      // Put the data into the image
      int pointNo = 0;
      for (int i = 0; i < fillHeight_; i++) {
        for (int j = 0; j < fillWidth_; j++) {
          if (fillData_[pointNo] == 0 && backgroundColor_ != null)
            fillPattern_.setRGB (i, j, backgroundColor_.getRGB());
          else if (fillData_[pointNo] != 0)
            fillPattern_.setRGB (i, j, foregroundColor_.getRGB());
          
          pointNo++;
        }
      }

      paint_ = null;
    }

    // Generate the paint
    if (paint_ == null && fillPattern_ != null) {
      paint_ = new TexturePaint (fillPattern_,
                                 new Rectangle (0, 0,
                                                fillWidth_, fillHeight_));
    }

    // TODO: Gradient color is not currently in use
    else if (paint_ == null && gradientColor1_ != null) {
      paint_ = new GradientPaint ((float) -100.0, (float) 0.0, gradientColor1_,
                                  (float) 100.0, (float) 0.0, gradientColor2_,
                                  true);
    }
    else if (backgroundColor_ != null) {
      paint_ = backgroundColor_;
    }
    
    return paint_;
  }

  

  float getMiterLimit()
  {
    return miterLimit_;
  }


  
  /**
   * Check if line is visible. Line is invisible if line style is set
   * to invisible.
   * 
   * @return  True if line is visible, false otherwise.
   */
  boolean isLineVisible()
  {
    return isLineVisible_;
  }


  
  /**
   * Check if objects using this style appaer as filled?
   * 
   * @return  Return true if objects using this style appear as filled,
   *          false otherwise.
   */
  boolean isDefiningFill()
  {
    return fillPattern_     != null ||
           fillData_        != null ||
           backgroundColor_ != null;
  }


  
  /**
   * Notify all listeners about change in specified style.
   * 
   * @param style  Style that has changed.
   */
  private void notifyListeners()
  {
    for (Iterator i = listeners_.iterator(); i.hasNext(); ) {
      WeakReference reference = (WeakReference) i.next();
      GStyleListener listener = (GStyleListener) reference.get();

      // If the listener has been GC'd, remove it from the list
      if (listener == null)
        i.remove();
      else
        listener.styleChanged (this);
    }
  }


  
  /**
   * Add a listener to this style. When the style is changed, a
   * styleChanged() signal is sent to the listener.
   * 
   * @param listener  Style listener to add.
   */
  void addListener (GStyleListener listener)
  {
    // Check if the listener is there already
    for (Iterator i = listeners_.iterator(); i.hasNext(); ) {
      WeakReference reference = (WeakReference) i.next();
      if (reference.get() == listener)
        return;
    }

    // Add the listener
    listeners_.add (new WeakReference (listener));
  }


  
  /**
   * Remove specified listener from this style.
   * 
   * @param listener  Style listener to remove.
   */
  void removeListener (GStyleListener listener)
  {
    for (Iterator i = listeners_.iterator(); i.hasNext(); ) {
      WeakReference reference = (WeakReference) i.next();
      if (reference.get() == listener) {
        i.remove();
        break;
      }
    }
  }
}
