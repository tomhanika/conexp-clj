package no.geosoft.cc.graphics;



import java.awt.Adjustable;
import java.awt.event.AdjustmentListener;
import java.awt.event.AdjustmentEvent;



/**
 * Class for handling window scrollbars.
 * @see GScene.installScrollHandler()
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
class GScrollHandler
  implements AdjustmentListener
{
  private final GScene      scene_;
  private final Adjustable  hScrollBar_;
  private final Adjustable  vScrollBar_;


  
  /**
   * Create a scroll handler for the specified scene and with the
   * specified scroll bars.
   * <p>
   * <b>NOTE:</b> Do not use the scrollbars of a JScrollPane for this purpose.
   * 
   * @param scene       Scene to make scrollable.
   * @param hScrollBar  The horizontal scrollbar (or null if none).
   * @param vScrollBar  The vertical scrollbar (or null if none).
   */
  GScrollHandler (GScene scene,
                  Adjustable hScrollBar, Adjustable vScrollBar)
  {
    scene_ = scene;
    
    hScrollBar_ = hScrollBar;
    vScrollBar_ = vScrollBar;

    hScrollBar_.addAdjustmentListener (this);
    vScrollBar_.addAdjustmentListener (this);

    updateScrollBars();
  }

  

  /**
   * Update the scrollbars according to the current setting of the
   * scene viewport and world extents.
   */
  void updateScrollBars()
  {
    hScrollBar_.removeAdjustmentListener (this);
    vScrollBar_.removeAdjustmentListener (this);    

    GWorldExtent  worldExtent = scene_.getInitialWorldExtent();
    GViewport     viewport    = scene_.getViewport();
    GTransformer  transformer = scene_.getTransformer();
    
    int  viewport0[] = viewport.get (0);
    int  world0[]    = transformer.worldToDevice (worldExtent.get (2));
    int  viewport1[] = viewport.get (1);
    int  world1[]    = transformer.worldToDevice (worldExtent.get (1));
    int  viewport2[] = viewport.get (2);
    int  world2[]    = transformer.worldToDevice (worldExtent.get (0));

    //
    // Set horizontal scrollbar
    //
    if (hScrollBar_ != null) {
      int minimum        = Math.min (viewport0[0], world0[0]);
      int maximum        = Math.max (viewport1[0], world1[0]);      
      int value          = viewport0[0];
      int visibleAmount  = viewport1[0] - viewport0[0];
      int unitIncrement  = (int) Math.max (1, Math.round (visibleAmount * 0.1));
      int blockIncrement = (int) Math.max (1, Math.round (visibleAmount * 0.9));

      hScrollBar_.setMinimum (minimum);
      hScrollBar_.setMaximum (maximum);      
      hScrollBar_.setValue (value);
      hScrollBar_.setVisibleAmount (visibleAmount);
      hScrollBar_.setUnitIncrement (unitIncrement);
      hScrollBar_.setBlockIncrement (blockIncrement);
    }

    //
    // Set vertical scrollbar
    //
    if (vScrollBar_ != null) {
      int minimum        = Math.min (viewport0[1], world0[1]);
      int maximum        = Math.max (viewport2[1], world2[1]);      
      int value          = viewport0[1];
      int visibleAmount  = viewport2[1] - viewport0[1];
      int unitIncrement  = (int)Math.max (1, Math.round (visibleAmount * 0.1));
      int blockIncrement = (int)Math.max (1, Math.round (visibleAmount * 0.9));

      vScrollBar_.setMinimum (minimum);
      vScrollBar_.setMaximum (maximum);      
      vScrollBar_.setValue (value);      
      vScrollBar_.setVisibleAmount (visibleAmount);
      vScrollBar_.setUnitIncrement (unitIncrement);
      vScrollBar_.setBlockIncrement (blockIncrement);
    }

    hScrollBar_.addAdjustmentListener (this);
    vScrollBar_.addAdjustmentListener (this);    
  }


  
  /**
   * Called as a response to scrollbar access. Adjust the world extent
   * according to new scrollbar setting.
   * 
   * @param event  Event trigging this method.
   */
  public void adjustmentValueChanged (AdjustmentEvent event)
  {
    GViewport viewport = scene_.getViewport();

    int x0 = viewport.getX0();
    int y0 = viewport.getY0();
    int x1 = viewport.getX3();
    int y1 = viewport.getY3();

    if (hScrollBar_ != null) {
      int hValue         = hScrollBar_.getValue();
      int hVisibleAmount = hScrollBar_.getVisibleAmount();

      x0 = hValue;
      x1 = hValue + hVisibleAmount;
    }

    if (vScrollBar_ != null) {
      int vValue         = vScrollBar_.getValue();
      int vVisibleAmount = vScrollBar_.getVisibleAmount();
      
      y0 = vValue;
      y1 = vValue + vVisibleAmount;
    }

    scene_.zoom (x0, y0, x1, y1);
  }
}
