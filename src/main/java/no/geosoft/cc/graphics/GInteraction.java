package no.geosoft.cc.graphics;



/**
 * Common interface for all graphics interactions.
 * <p>
 * Typical usage:
 *
 * <pre>
 *    public class DrawInteraction implements GInteraction
 *    {
 *       private GObject   interaction_;
 *       private GSegment  line_;
 *
 *       public void event (GScene scene, int event, int x, int y)
 *       {
 *          switch (event) {
 *             case GWindow.BUTTON1_DOWN :
 *               // Create interaction object and segment and add to scene
 *               break;
 * 
 *             case GWindow.BUTTON1.DRAG :
 *                // extend segment with new x,y
 *                // and refresh graphics
 * 
 *             case ...
 *          }
 *       }
 *    }
 * </pre>
 *
 * The interaction is started by installing it in the GWindow:
 *
 * <pre>
 *    GInteraction drawInteraction = new DrawInteraction();
 *    window.startInteraction (drawInteraction);
 * </pre>
 *
 * The interaction is stopped if another interaction is started or if it
 * is explicitly stopped by <tt>GWindow.stopInteraction()</tt>. Before
 * the interaction is stopped a <tt>GWindow.ABORT</tt> event is passed
 * to the interaction.
 *
 * @see GWindow#startInteraction(GInteraction)
 * @see GWindow#stopInteraction()
 * @see ZoomInteraction
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public interface GInteraction
{
  /**
   * Trigged by mouse events within the canvas.
   * @see no.geosoft.cc.graphics.GWindow
   *
   * @param scene  The scene of this event.
   * @param event  Event type 
   * @param x      X position of cursor.
   * @param y      Y position of cursor.   
   */
  public void event (GScene scene, int event, int x, int y);
}
