package no.geosoft.cc.graphics;



import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.BufferedInputStream;
import java.awt.Image;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;



/**
 * Wrapper object for images used with GSegments. A GImage represent
 * both predefined images as well as client specified images.
 * <p>
 * Typical usage:
 *
 * <pre>
 *    GImage image = new GImage (new File (imageFileName),
 *                               GPosition.SOUTHEAST);
 *    GSegment anchor = new GSegment();
 *    anchor.setImage (image);
 * <pre>
 *
 * Images can also be associated with every vertex of a polyline.
 * If using one of the predefined images, a typical usage will be:
 *
 * <pre>
 *    GImage image = new GImage (SYMBOL_CIRCLE1);
 *    GSegment segment = new GSegment();
 *    segment.setVertexImage (image);
 * </pre>
 *   
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GImage extends GPositional
{
  // Some predefined images suitable for use as vertex images
  public static final int  SYMBOL_SQUARE1 =  1;
  public static final int  SYMBOL_SQUARE2 =  2;
  public static final int  SYMBOL_SQUARE3 =  3;    
  public static final int  SYMBOL_SQUARE4 =  4;
  public static final int  SYMBOL_CIRCLE1 =  5;  // TODO
  public static final int  SYMBOL_CIRCLE2 =  6;  // TODO
  public static final int  SYMBOL_CIRCLE3 =  7;  // TODO
  public static final int  SYMBOL_CIRCLE4 =  8;  // TODO

  private static final int DEFAULT_POSITION_HINT = GPosition.CENTER |
                                                   GPosition.STATIC;
  
  private Image  image_;
  private int    imageData_[];
  private File   file_;

  

  /**
   * Create image of a predefined type.
   * @see GImage#setPositionHint(int)
   *
   * @param symbolType    Symbol to create.
   * @param positionHint  Position hint.
   */
  public GImage (int symbolType, int positionHint)
  {
    super (positionHint, true);
    
    initialize();
    
    int width;
    int height;

    switch (symbolType) {
      case SYMBOL_SQUARE1 : width =  5; height =  5; break;
      case SYMBOL_SQUARE2 : width =  7; height =  7; break;
      case SYMBOL_SQUARE3 : width =  9; height =  9; break;
      case SYMBOL_SQUARE4 : width = 11; height = 11; break;
      // TODO: Define circles.
      default             : return; // Unknown symbol type
    }
    
    int data[] = new int [width * height];
    
    // Set bits specified
    switch (symbolType) {
      case SYMBOL_SQUARE1 :
      case SYMBOL_SQUARE2 :
      case SYMBOL_SQUARE3 :
      case SYMBOL_SQUARE4 :        
        for (int i = 0; i < data.length; i++)
          data[i] = 1;
    }

    setImage (width, height, data);
  }



  
  /**
   * Create an image of predefined type and with default position hints.
   * 
   * @param symbolType  Predefined symbol type.
   */
  public GImage (int symbolType)
  {
    this (symbolType, DEFAULT_POSITION_HINT);
  }

  
  
  /**
   * Create a image based on specified color data.
   * @see GImage#setPositionHint(int)   
   * 
   * @param width         Width of image.
   * @param height        Height of image.
   * @param data          Color values for image.
   * @param positionHint  Position hint.
   */
  public GImage (int width, int height, int data[], int positionHint)
  {
    super (positionHint, true);
    initialize();
    setImage (width, height, data);
  }


  
  /**
   * Create a image based on specified color data. Use defult position
   * hints.
   * 
   * @param width   Width of image.
   * @param height  Height of image.
   * @param data    Color values for image.
   */
  public GImage (int width, int height, int data[])
  {
    this (width, height, data, DEFAULT_POSITION_HINT);
  }



  private GWindow getWindow()
  {
    return getSegment().getOwner().getWindow();
  }
  
  
  /**
   * Create a GImage from an AWT Image.
   * @see GImage#setPositionHint(int)   
   * 
   * @param image         Image.
   * @param positionHint  Position hint.
   */
  public GImage (Image image, int positionHint)
  {
    super (positionHint, true);
    
    initialize();

    image_ = image;
  }


  
  /**
   * Create a GImage from an AWT Image. Use default position hints.
   * 
   * @param image  Image.
   */
  public GImage (Image image)
  {
    this (image, DEFAULT_POSITION_HINT);
  }
  

  
  /**
   * Create an image from a file. The following formats are supported:
   *
   * <ul>
   * <li>BMP
   * <li>FPX
   * <li>GIF
   * <li>JPEG
   * <li>PNG
   * <li>PNM
   * <li>TIFF
   * </ul>
   *
   * @see GImage#setPositionHint(int)   
   * 
   * @param file          Image file.
   * @param positionHint  Position hint.
   */
  public GImage (File file, int positionHint)
  {
    super (positionHint, true);
    
    initialize();
    file_ = file;
  }


  
  /**
   * Create an image from a file. Use default position hints.
   * 
   * @param file  Image file. 
   */
  public GImage (File file)
  {
    this (file, DEFAULT_POSITION_HINT);
  }
  


  /**
   * Initialize this GImage.
   */
  private void initialize()
  {
    // Set back-end variables to null for laze create
    imageData_ = null;
    image_     = null;
  }



  /**
   * Set image data.
   * 
   * @param width   Width of image.
   * @param height  Height of image.
   * @param data    Color values of image.
   */
  private void setImage (int width, int height, int data[])
  {
    rectangle_.width  = width;
    rectangle_.height = height;

    // Copy the image data locally
    int size = width * height;
    imageData_ = new int[size];
    for (int i = 0; i < size; i++)
      imageData_[i] = data != null && data.length > i ? data[i] : 0;
    
    imageData_ = data;
  }


  
  /**
   * Compute size of this image and update rectangle variable.
   */
  void computeSize()
  {
    if (image_ != null && rectangle_.width == 0 && rectangle_.height == 0) {
      rectangle_.width  = image_.getWidth (getWindow().getCanvas());
      rectangle_.height = image_.getHeight (getWindow().getCanvas());      
    }
    
    // This computation is only done if the image is file based.
    // In this case we realize the image at this point.
    // Otherwise the size is already correctly set.
    else if (image_ == null && file_ != null) {
      try {
        InputStream stream = new BufferedInputStream (new FileInputStream
                                                      (file_.getPath()));
        BufferedImage image = ImageIO.read (stream);
        
        rectangle_.height = image.getHeight();
        rectangle_.width  = image.getWidth();

        image_ = image;
      }
      
      // There is something wrong with the image file. We don't
      // care about telling the client, as this will become evident
      // anyway. Just leave the rectangle as empty.
      catch (Exception exception) {
        exception.printStackTrace();
        image_ = null;
        file_  = null;

        rectangle_.height = 0;
        rectangle_.width  = 0;
      }
    }
  }
  
  

  /**
   * Return the realized image of this GImage.
   * 
   * @return  Image of this GImage.
   */
  Image getImage()
  {
    // Lazy image create
    if (image_ == null && imageData_ != null) {
      int width  = rectangle_.width;
      int height = rectangle_.height;

      int backgroundColor = actualStyle_.getBackgroundColor().getRGB();
      int foregroundColor = actualStyle_.getForegroundColor().getRGB();
      
      // Create the image that represent the pattern
      BufferedImage image = new BufferedImage (width, height,
                                               BufferedImage.TYPE_INT_ARGB);

      // Put the data into the image
      int pointNo = 0;
      for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
          image.setRGB (i, j, imageData_[pointNo] == 0 ? backgroundColor :
                                                         foregroundColor);
          pointNo++;
        }
      }

      image_ = image;
    }
    
    return image_;
  }
}
