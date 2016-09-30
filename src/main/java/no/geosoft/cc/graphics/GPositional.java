package no.geosoft.cc.graphics;



import no.geosoft.cc.geometry.Rect;



/**
 * Common base class for objects that can be positioned through
 * hints within their container. 
 *
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public abstract class GPositional
{
  protected GSegment  segment_;
  protected int       positionHint_;
  protected Rect      rectangle_;
  protected boolean   isVisible_;
  protected GStyle    style_;
  protected GStyle    actualStyle_;  // Adjusted for owner inherits

  private final boolean   isAllowingOverlaps_;
  

  
  /**
   * Create a positional instance with specified position hint.
   * @see #setPositionHint(int)
   * 
   * @param positionHint        Position hint for this instance.
   * @param isAllowingOverlaps  True if this instance can be overlapped by
   *                            other positionals, false otherwise.
   */
  GPositional (int positionHint, boolean isAllowingOverlaps)
  {
    rectangle_          = new Rect();
    isVisible_          = false;
    isAllowingOverlaps_ = isAllowingOverlaps;
    segment_            = null;
    style_              = null;
    actualStyle_        = new GStyle();

    setPositionHint (positionHint);
  }
  

  
  /**
   * Set owner segment of this positional.
   * 
   * @param segment  New owner segment of this positional.
   */
  void setSegment (GSegment segment)
  {
    segment_ = segment;
    updateStyle();
  }

  
  
  /**
   * Return owner segment of this positional.
   * 
   * @return  Owner segment of this positional (or null if not
   *          attached to a segment).
   */
  GSegment getSegment()
  {
    return segment_;
  }
  

  
  /**
   * Return graphics object of this positional.
   * 
   * @return  Object of this positional (or null if not attached to an
   *          object).
   */
  protected GObject getObject()
  {
    return segment_ != null ? segment_.getOwner() : null;
  }
  
  

  /**
   * Set position hint for this positional.
   * <p>
   * Position hints is a or'ed list of:
   *
   * <ul>
   * <li>Line position hint  - FIRST, LAST, TOP, BOTTON, LEFT, RIGHT
   * <li>Point position hint - CENTER, NORTH, SOUTH, EAST, WEST, NORTHEAST,
   *                           NORTHWEST, SOUTHEAST, SOUTHWEST
   * <li>Algorithm           - STATIC, DYNAMIC
   * </ul>
   *
   * <p>
   * Line position hints are interpreted as follows:
   *
   * <ul>
   * <li> GPosition.FIRST  - Attach to first polyline point.
   * <li> GPosition.LAST   - Attach to last polyline point.
   * <li> GPosition.TOP    - Attach to top polyline as it appears on screen
   *                         when rendered.
   * <li> GPosition.BOTTOM - Attach to bottom polyline as it appears on screen
   *                         when rendered.
   * <li> GPosition.RIGHT  - Attach to right polyline as it appears on screen
   *                         when rendered.
   * <li> GPosition.LEFT   - Attach to left polyline as it appears on screen
   *                         when rendered.
   * </ul>
   *
   * If a line position hint is not given, the text is attached to the n'th
   * point of the polyline according to when it was added to the segment,
   * i.e, the first text added is attached to the first coordinate, the second
   * text added is attatched to the second coordinate and so on. This is
   * convenient if there is a text associated with each line vertex.
   *
   * <p>
   * For the above, the given point becomes the initial approach for
   * positioning. If this initial position is outside the window (and position
   * hint not explicitly set to GPosition.STATIC) the text is moved along
   * the polyline till it becomes fully visible.
   *
   * <p>
   * If text position hint is GPosition.MIDDLE, the initial position is
   * the center of the polyline bounding box. If this point is not visble
   * the text is not rendered.
   *
   * <p>
   * At this point an anchor point is found for the text. Now the point
   * positioning text hints are examined:
   *
   * <ul>
   * <li> GPosition.CENTER    - Put text centered on top of anchor point.
   * <li> GPosition.NORTH     - Put text above anchor point.
   * <li> GPosition.SOUTH     - Put text below anchor point.
   * <li> GPosition.EAST      - Put text to the right of anchor point.
   * <li> GPosition.WEST      - Put text to the left of anchor point.   
   * <li> GPosition.NORTHEAST - Put text above and right of the anchor point.
   * <li> GPosition.NORTHWEST - Put text above and left of the anchor point.
   * <li> GPosition.SOUTHEAST - Put text below and right of the anchor point.
   * <li> GPosition.SOUTHWEST - Put text below and left of the anchor point.
   * </ul>
   *
   * <p>
   * If a point positioing hint is not given, the defualt is GPosition.CENTER
   * unless line position hint is GPosition.TOP (implying GPosition.NORTH),
   * GPosition.BOTTOM (implying GPosition.SOUTH), GPosition.LEFT (implying
   * GPosition.WEST) or GPosition:RIGHT (implying GPosition.EAST).
   *
   * <p>
   * Now, if the text in this location overlap an already positioned text,
   * it is further adjusted (unless position hint is not explicitly set to
   * GPosition.STATIC as discussed above). It is again moved along the
   * polyline till a free location is found. If this cannot be acheived,
   * the text is not rendered
   *
   * @see GPosition
   * 
   * @param positionHint  Position hint for this positional.
   */
  public void setPositionHint (int positionHint)
  {
    positionHint_ = positionHint;

    // If point hint is not specified, default it according to line hint
    if ((positionHint_ &
         (GPosition.CENTER    |  
          GPosition.NORTHWEST |
          GPosition.NORTH     |
          GPosition.NORTHEAST |
          GPosition.WEST      |
          GPosition.EAST      |
          GPosition.SOUTHWEST |
          GPosition.SOUTH     |
          GPosition.SOUTHEAST)) == 0) {
      if      ((positionHint_ & GPosition.TOP)    != 0)
        positionHint_ |= GPosition.NORTH;
      else if ((positionHint_ & GPosition.BOTTOM) != 0)
        positionHint_ |= GPosition.SOUTH;
      else if ((positionHint_ & GPosition.LEFT)   != 0)
        positionHint_ |= GPosition.WEST;
      else if ((positionHint_ & GPosition.RIGHT)  != 0)
        positionHint_ |= GPosition.EAST;
    }

    // If algorithm is not specified, default to dynamic
    if ((positionHint_ & GPosition.DYNAMIC) == 0 &&
        (positionHint_ & GPosition.STATIC) == 0)
      positionHint_ |= GPosition.DYNAMIC;
  }


  
  /**
   * Return position hint of this positional.
   * @see #setPositionHint(int).
   * 
   * @return  Position hint of this positional.
   */
  public int getPositionHint()
  {
    return positionHint_;
  }


  
  /**
   * Return if this positional is allowed to be overlapped by other positionals.
   * 
   * @return  True if this positional is allowed to be overlapped other
   *          positionals false otherwise.
   */
  boolean isAllowingOverlaps()
  {
    return isAllowingOverlaps_;
  }
  

  
  /**
   * Return true if the position of this positional is along the line
   * (as opposed to be fixed at a point on the line).
   * 
   * @return  True if this poitional is "line positional", false otherwise.
   */
  boolean isLinePositional()
  {
    return (positionHint_ & (GPosition.FIRST  |
                             GPosition.LAST   |
                             GPosition.TOP    |
                             GPosition.BOTTOM |
                             GPosition.LEFT   |
                             GPosition.RIGHT  |
                             GPosition.MIDDLE)) != 0;
  }


  
  /**
   * Return true if this positional is visible after the annotation process.
   * 
   * @return  True if this positional is visible, false otherwise.
   */
  boolean isVisible()
  {
    return isVisible_;
  }



  /**
   * Set visibility of this positional. Visibility is due to positional
   * layout, not visibility settings on the owner object.
   *
   * @param isVisible  True if positional is visible, false otherwise.
   */
  void setVisible (boolean isVisible)
  {
    isVisible_ = isVisible;
  }


  
  /**
   * Return rectangle bounding box of this positional.
   * 
   * @return  Rectangle bounding box of this positional.
   */
  Rect getRectangle()
  {
    return rectangle_;
  }

  

  /**
   * Compute the size (width and height component of the rectangle variable)
   * of this positional.
   */
  abstract void computeSize();
  


  /**
   * Return margin to use when positioning this positional at a point.
   * If point position hint is NORTH, the positional rectangle is put
   * this many pixels above the associated point etc.
   * 
   * @return  Margin for point positioning.
   */
  int getMargin()
  {
    // Default to 0, override in subclass if necessary.
    return 0;
  }
  
  
  
  /**
   * Set new style for this object. Style elements not explicitly
   * set within this GStyle object are inherited from parent objects.
   * Child objects without explicit set style will inherit from this
   * style object.
   * 
   * @param style  Style for this object.
   */
  public void setStyle (GStyle style)
  {
    style_ = style;
    updateStyle();
  }
  


  /**
   * Get style of this GText. This is the style set by setStyle()
   * and not necesserily the style as it appears on screen as unset
   * style elements are inherited from parents.
   * 
   * @return  Style of this object.
   */
  public GStyle getStyle()
  {
    return style_;
  }

  

  /**
   * These are the actual style used for this object when
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
   * Update actualStyle based on this style and style of parent
   * elements. Actual style is how this element is actually rendered
   * on screen.
   */
  void updateStyle()
  {
    // Initialize style
    actualStyle_ = new GStyle();

    // Update with parent style
    if (segment_ != null)
      actualStyle_.update (segment_.getActualStyle());

    // Update (and possible override) with present style
    if (style_ != null)
      actualStyle_.update (style_);

    // Flag owner region and scene annotation as invalid
    GObject object = getObject();
    GScene  scene  = object != null ? object.getScene() : null;

    if (object != null) object.flagRegionValid (false);
    if (scene  != null) scene.setAnnotationValid (false);

    updateDamage();
  }


  
  /**
   * Mark the extent of this positional as damaged.
   */
  void updateDamage()
  {
    GObject object = getObject();
    
    if (isVisible()           &&
        rectangle_ != null    &&
        !rectangle_.isEmpty() &&
        object != null        &&
        object.getWindow() != null)
      object.getWindow().updateDamageArea (rectangle_);
  }
}
