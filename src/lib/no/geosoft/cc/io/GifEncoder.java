package no.geosoft.cc.io;



import java.io.*;
import java.awt.*;
import java.awt.image.*;



/**
 * Class for converting images to GIF files.
 *
 * <p>
 * Contribution:
 * <ul>
 *   <li>Sverre H. Huseby (gifsave.c on which this is based)</li>
 *   <li>Adam Doppelt (Initial Java port)</li>
 *   <li>Greg Faron (Initial java port)</li>
 * </ul>
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GifEncoder
{
  private short   imageWidth_, imageHeight_;
  private int     nColors_;
  private byte[]  pixels_ = null;
  private byte[]  colors_ = null;



  /**
   * Constructing a GIF encoder.
   *
   * @param image  The image to encode. The image must be
   *               completely loaded.
   * @throws AWTException  If memory is exhausted or image contains
   *                       more than 256 colors.
   */
  public GifEncoder (Image image)
    throws AWTException
  {
    imageWidth_  = (short) image.getWidth (null);
    imageHeight_ = (short) image.getHeight (null);

    int values[] = new int[imageWidth_ * imageHeight_];
    PixelGrabber grabber = new PixelGrabber (image, 0, 0,
                                             imageWidth_, imageHeight_,
                                             values, 0, imageWidth_);

    try {
      if (grabber.grabPixels() != true)
        throw new AWTException("Grabber returned false: " + grabber.status());
    }
    catch (InterruptedException exception) {
    }

    byte[][] r = new byte[imageWidth_][imageHeight_];
    byte[][] g = new byte[imageWidth_][imageHeight_];
    byte[][] b = new byte[imageWidth_][imageHeight_];

    int index = 0;

    for (int y = 0; y < imageHeight_; y ++) {
      for (int x = 0; x < imageWidth_; x ++, index ++) {
        r[x][y] = (byte) ((values[index] >> 16) & 0xFF);
        g[x][y] = (byte) ((values[index] >> 8)  & 0xFF);
        b[x][y] = (byte) ((values[index] >> 0)  & 0xFF);
      }
    }

    toIndexColor (r, g, b);
  }


  
  /**
   * Create a GIF encoder. r[i][j] refers to the pixel at
   * column i, row j.
   *
   * @param r  Red intensity values.
   * @param g  Green intensity values.
   * @param b  Blue intensity values.
   * @throws AWTException  If memory is exhausted or image contains
   *                       more than 256 colors.
   */
  public GifEncoder (byte[][] r, byte[][] g, byte[][] b)
    throws AWTException
  {
    imageWidth_  = (short) (r.length);
    imageHeight_ = (short) (r[0].length);

    toIndexColor(r, g, b);
  }


  
  /**
   * Write image to GIF file.
   * 
   * @param image  Image to write.
   * @param file   File to erite to.
   */
  public static void writeFile (Image image, File file)
    throws AWTException, IOException
  {
    GifEncoder gifEncoder = new GifEncoder (image);
    gifEncoder.write (new FileOutputStream (file));
  }
  

  
  /**
   * Write AWT/Swing component to GIF file.
   * 
   * @param image  Image to write.
   * @param file   File to erite to.
   */
  public static void writeFile (Component component, File file)
    throws AWTException, IOException
  {
    Image image  = component.createImage (component.getWidth(),
                                          component.getHeight());
    Graphics graphics = image.getGraphics();
    component.printAll (graphics);

    GifEncoder.writeFile (image, file);
  }
  
  
  
  /**
   * Writes the image out to a stream in GIF format.
   * This will be a single GIF87a image, non-interlaced, with no
   * background color.
   *
   * @param  stream       The stream to which to output.
   * @throws IOException  Thrown if a write operation fails.
   */
  public void write (OutputStream stream)
    throws IOException
  {
    writeString (stream, "GIF87a");
    writeScreenDescriptor (stream);

    stream.write (colors_, 0, colors_.length);

    writeImageDescriptor (stream, imageWidth_, imageHeight_, ',');

    byte codeSize = bitsNeeded (nColors_);
    if (codeSize == 1) codeSize++;

    stream.write (codeSize);

    writeLzwCompressed (stream, codeSize, pixels_);
    stream.write(0);

    writeImageDescriptor (stream, (short) 0, (short) 0, ';');
    stream.flush();
    stream.close();
  }


  
  /**
   * Converts rgb desrcription of image to colour
   * number description used by GIF.
   *
   * @param r  Red array of pixels.
   * @param g  Green array of pixels.
   * @param b  Blue array of pixels.
   * @throws   AWTException
   *           Thrown if too many different colours in image.
   */
  private void toIndexColor (byte[][] r, byte[][] g, byte[][] b)
    throws AWTException
  {
    pixels_ = new byte[imageWidth_ * imageHeight_];
    colors_ = new byte[256 * 3];
    int colornum = 0;

    for (int x = 0; x < imageWidth_; x++) {
      for (int y = 0; y < imageHeight_; y++ ) {
        int search;
        for (search = 0; search < colornum; search ++ ) {
          if (colors_[search * 3 + 0] == r[x][y] &&
              colors_[search * 3 + 1] == g[x][y] &&
              colors_[search * 3 + 2] == b[x][y]) {
            break;
          }
        }
        
        if (search > 255)
          throw new AWTException("Too many colors.");

        // Row major order y=row x=col
        pixels_[y * imageWidth_ + x] = (byte) search;
        
        if (search == colornum) {
          colors_[search * 3 + 0] = r[x][y]; // [col][row]
          colors_[search * 3 + 1] = g[x][y];
          colors_[search * 3 + 2] = b[x][y];
          colornum++;
        }
      }
    }
    
    nColors_ = 1 << bitsNeeded (colornum);
    byte copy[] = new byte[nColors_ * 3];
    System.arraycopy (colors_, 0, copy, 0, nColors_ * 3);

    colors_ = copy;
  }



  private byte bitsNeeded (int n)
  {
    if (n-- == 0) return 0;

    byte nBitsNeeded = 1;
    while ((n >>= 1) != 0)
      nBitsNeeded++;

    return nBitsNeeded;
  }

  

  private void writeWord (OutputStream stream, short w)
    throws IOException
  {
    stream.write (w & 0xFF);
    stream.write ((w >> 8) & 0xFF);
  }


  
  private void writeString (OutputStream stream, String string)
    throws IOException
  {
    for (int i = 0; i < string.length(); i ++ )
      stream.write ((byte) (string.charAt (i)));
  }



  private void writeScreenDescriptor (OutputStream stream)
    throws IOException
  {
    writeWord (stream, imageWidth_);
    writeWord (stream, imageHeight_);

    byte flag = 0;

    // Global color table size
    byte globalColorTableSize = (byte) (bitsNeeded (nColors_) - 1);
    flag |= globalColorTableSize & 7;

    // Global color table flag
    byte globalColorTableFlag = 1;
    flag |= (globalColorTableFlag & 1) << 7;

    // Sort flag
    byte sortFlag = 0;
    flag |= (sortFlag & 1) << 3;

    // Color resolution
    byte colorResolution = 7;
    flag |= (colorResolution & 7) << 4;    
    
    byte backgroundColorIndex = 0;
    byte pixelAspectRatio     = 0;

    stream.write (flag);
    stream.write (backgroundColorIndex);
    stream.write (pixelAspectRatio);
  }
  


  private void writeImageDescriptor (OutputStream stream,
                                     short width, short height, char separator)
    throws IOException
  {
    stream.write (separator);

    short leftPosition = 0;
    short topPosition  = 0;
    
    writeWord (stream, leftPosition);
    writeWord (stream, topPosition);
    writeWord (stream, width);
    writeWord (stream, height);

    byte flag = 0;
    
    // Local color table size
    byte localColorTableSize = 0;
    flag |= (localColorTableSize & 7);    

    // Reserved
    byte reserved = 0;
    flag |= (reserved & 3) << 3;    

    // Sort flag
    byte sortFlag = 0;
    flag |= (sortFlag & 1) << 5;    

    // Interlace flag
    byte interlaceFlag = 0;
    flag |= (interlaceFlag & 1) << 6;    

    // Local color table flag
    byte localColorTableFlag = 0;
    flag |= (localColorTableFlag & 1) << 7;    

    stream.write (flag);
  }


  
  private void writeLzwCompressed (OutputStream stream, int codeSize,
                                   byte toCompress[])
    throws IOException
  {
    byte c;
    short index;
    int clearcode, endofinfo, numbits, limit, errcode;
    short prefix = (short) 0xFFFF;

    BitFile bitFile = new BitFile (stream);
    LzwStringTable strings = new LzwStringTable();

    clearcode = 1 << codeSize;
    endofinfo = clearcode + 1;

    numbits = codeSize + 1;
    limit = (1 << numbits) - 1;

    strings.clearTable (codeSize);
    bitFile.writeBits(clearcode, numbits);

    for ( int loop = 0; loop < toCompress.length; loop ++ ) {
      c = toCompress[loop];
      if ( (index = strings.findCharString(prefix, c)) != -1 )
        prefix = index;
      else {
        bitFile.writeBits(prefix, numbits);
        if ( strings.addCharString(prefix, c) > limit ) {
          if ( ++ numbits > 12 ) {
            bitFile.writeBits(clearcode, numbits - 1);
            strings.clearTable (codeSize);
            numbits = codeSize + 1;
          }
          
          limit = (1 << numbits) - 1;
        }
        
        prefix = (short) ((short) c & 0xFF);
      }
    }
    
    if ( prefix != -1 )
      bitFile.writeBits(prefix, numbits);
    
    bitFile.writeBits(endofinfo, numbits);
    bitFile.flush();
  }
  


  /**
   * Used to compress the image by looking for repeating
   * elements.
   */
  private class LzwStringTable
  {
    private final static int    RES_CODES  = 2;
    private final static short  HASH_FREE  = (short) 0xFFFF;
    private final static short  NEXT_FIRST = (short) 0xFFFF;
    private final static int    MAXBITS    = 12;
    private final static int    MAXSTR     = (1 << MAXBITS);
    private final static short  HASHSIZE   = 9973;
    private final static short  HASHSTEP   = 2039;

    private byte   strChr_[];
    private short  strNxt_[];
    private short  strHsh_[];
    private short  nStrings_; 


  
    LzwStringTable()
    {
      strChr_ = new byte[MAXSTR];
      strNxt_ = new short[MAXSTR];
      strHsh_ = new short[HASHSIZE];
    }

  
    int addCharString (short index, byte b)
    {
      int hshidx;
      if ( nStrings_ >= MAXSTR )
        return 0xFFFF;

      hshidx = hash (index, b);
      while ( strHsh_[hshidx] != HASH_FREE )
        hshidx = (hshidx + HASHSTEP) % HASHSIZE;

      strHsh_[hshidx] = nStrings_;
      strChr_[nStrings_] = b;
      strNxt_[nStrings_] = (index != HASH_FREE)?index:NEXT_FIRST;

      return nStrings_++;
    }



    short findCharString(short index, byte b)
    {
      int hshidx, nxtidx;

      if ( index == HASH_FREE )
        return b;

      hshidx = hash (index, b);
      while ( (nxtidx = strHsh_[hshidx]) != HASH_FREE ) {
        if ( strNxt_[nxtidx] == index && strChr_[nxtidx] == b )
          return(short) nxtidx;
        hshidx = (hshidx + HASHSTEP) % HASHSIZE;
      }

      return(short) 0xFFFF;
    }


  
    void clearTable (int codesize)
    {
      nStrings_ = 0;

      for ( int q = 0; q < HASHSIZE; q ++ )
        strHsh_[q] = HASH_FREE;

      int w = (1 << codesize) + RES_CODES;
      for ( int q = 0; q < w; q ++ )
        this.addCharString((short) 0xFFFF, (byte) q);
    }


  
    int hash (short index, byte lastbyte)
    {
      return ((int)((short) (lastbyte << 8) ^ index) & 0xFFFF) % HASHSIZE;
    }
  }



  private class BitFile
  {
    private OutputStream stream_ = null;
    private byte[]       buffer_;
    private int          streamIndex_, bitsLeft_;


  
    BitFile(OutputStream stream)
    {
      stream_      = stream;
      buffer_      = new byte[256];
      streamIndex_ = 0;
      bitsLeft_    = 8;
    }


  
    void flush()
      throws IOException
    {
      int nBytes = streamIndex_ + ((bitsLeft_ == 8) ? 0 : 1);
                                                  
      if (nBytes > 0) {
        stream_.write (nBytes);
        stream_.write (buffer_, 0, nBytes);

        buffer_[0]   = 0;
        streamIndex_ = 0;
        bitsLeft_    = 8;
      }
    }


  
    void writeBits (int bits, int nBits)
      throws IOException
    {
      int nBitsWritten = 0;
      int nBytes       = 255;

      do {
        if ((streamIndex_ == 254 && bitsLeft_ == 0) || streamIndex_ > 254) {
          stream_.write (nBytes);
          stream_.write (buffer_, 0, nBytes);
        
          buffer_[0]   = 0;
          streamIndex_ = 0;
          bitsLeft_    = 8;
        }
      
        if (nBits <= bitsLeft_) {
          buffer_[streamIndex_] |= (bits & ((1 << nBits) - 1)) << (8 - bitsLeft_);
                                                             
          nBitsWritten += nBits;
          bitsLeft_    -= nBits;
          nBits         = 0;
        }

        else {
          buffer_[streamIndex_] |= (bits & ((1 << bitsLeft_) - 1)) <<
                                   (8 - bitsLeft_);
                                                             
          nBitsWritten += bitsLeft_;
          bits >>= bitsLeft_;
          nBits -= bitsLeft_;
          buffer_[++streamIndex_] = 0;
          bitsLeft_ = 8;
        }
      
      } while (nBits != 0);
    }
  }
}
