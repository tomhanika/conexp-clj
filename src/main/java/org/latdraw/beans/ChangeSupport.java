
package org.latdraw.beans;

import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

/**
 * A class for property changes.
 *
 * @author Ralph Freese
 * @version $Id: ChangeSupport.java,v 1.3 2008/08/04 18:56:26 ralph Exp $
 */
public class ChangeSupport extends PropertyChangeSupport {

  public ChangeSupport(Object source) {
    super(source);
  }

  public static final String DIAGRAM_CHANGED = "diagramChanged";
  public static final String EDGE_PRESSED = "edgePressed";
  public static final String EDGE_RIGHT_PRESSED = "edgeRightPressed";
  public static final String EDGE_MIDDLE_PRESSED = "edgeMiddlePressed";

  public static final String VERTEX_PRESSED = "vertexPressed";
  public static final String VERTEX_PRESSED_SHIFT = "vertexPressedShift";
  public static final String VERTEX_PRESSED_CTRL = "vertexPressedCtrl";
  public static final String VERTEX_PRESSED_ALT = "vertexPressedAlt";

  public static final String VERTEX_RIGHT_PRESSED = "vertexRightPressed";
  public static final String VERTEX_RIGHT_PRESSED_SHIFT 
                                      = "vertexRightPressedShift";
  public static final String VERTEX_RIGHT_PRESSED_CTRL 
                                      = "vertexRightPressedCtrl";
  public static final String VERTEX_RIGHT_PRESSED_ALT 
                                       = "vertexRightPressedAlt";

  public static final String VERTEX_MIDDLE_PRESSED = "vertexMiddlePressed";
  public static final String VERTEX_MIDDLE_PRESSED_SHIFT 
                                       = "vertexMiddlePressedShift";
  public static final String VERTEX_MIDDLE_PRESSED_CTRL 
                                       = "vertexMiddlePressedCtrl";
  public static final String VERTEX_MIDDLE_PRESSED_ALT 
                                       = "vertexMiddlePressedAlt";

  // mouse pressed but missed everyting:
  public static final String NOTHING_PRESSED = "nothingPressed";

}
