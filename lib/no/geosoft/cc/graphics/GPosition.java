package no.geosoft.cc.graphics;



/**
 * Collection of position hints. A position hint is an or'ed
 * collection of a line position hint, a point position hint
 * and a position algorithm.
 * <p>
 * Example position interpretation where the segment endpoints are
 * represented by an "o" and the position is represented by an "X".:
 *
 * <pre>
 *  
 *              X  (GPosition.TOP | GPosition.NORTH)
 *              o
 *               \
 *                \
 *                 \
 *                  \
 *                   o X (GPosition.BOTTOM | GPosition.EAST)
 *                  X (Gposition.RIGHT | GPosition.SOUTHWEST)
 * </pre>
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public interface GPosition
{
  //
  // Line position hints
  //
  public static final int FIRST     = 1 << 1;   
  public static final int LAST      = 1 << 2;
  public static final int TOP       = 1 << 3;
  public static final int BOTTOM    = 1 << 4;
  public static final int LEFT      = 1 << 5;
  public static final int RIGHT     = 1 << 6;
  public static final int MIDDLE    = 1 << 7;

  //
  // Point position hints
  //
  public static final int CENTER    = 1 << 8;   
  public static final int NORTHWEST = 1 << 9;
  public static final int NORTH     = 1 << 10;
  public static final int NORTHEAST = 1 << 11;
  public static final int WEST      = 1 << 12;
  public static final int EAST      = 1 << 13;
  public static final int SOUTHWEST = 1 << 14;
  public static final int SOUTH     = 1 << 15;
  public static final int SOUTHEAST = 1 << 16;

  //
  // Positioning algorithm
  //
  public static final int DYNAMIC   = 1 << 17; 
  public static final int STATIC    = 1 << 18;
}
