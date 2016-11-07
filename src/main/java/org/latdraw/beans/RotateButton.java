/* RotateButton.java (c) Ralph Freese 2003/08/24 */

package org.latdraw.beans;

import org.latdraw.orderedset.*;
import org.latdraw.diagram.*;
import org.latdraw.util.*;
import java.io.*;
import java.util.*;
//import java.net.URL;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

//import java.awt.print.*;


public class RotateButton extends JButton {

  private DrawPanel drawPanel;

  private String stopLabel = "Stop";
  private String rotateLabel = "Rotate";

  public RotateButton(DrawPanel dp) { 
    setText(rotateLabel);
    drawPanel = dp;
    dp.addPropertyChangeListener(DrawPanel.ROTATION, 
                                 new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          if (e.getNewValue().equals(DrawPanel.STARTED)) {
            setText(rotateLabel);
          }
          else {
            setText(stopLabel);
          }
        }
    });
    addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          if (drawPanel.isRotating()) drawPanel.stopRotation();
          else drawPanel.startRotation();
        }
    });
  }

  public RotateButton(DrawPanel dp, String rLabel, String sLabel) { 
    this(dp);
    rotateLabel = rLabel;
    stopLabel = sLabel;
    setText(rotateLabel);
  }

  public void setStopLabel(String s) { stopLabel = s; }
  public String getStopLabel() { return stopLabel; }
  public void setRotateLabel(String s) { rotateLabel = s; }
  public String getRotateLabel() { return rotateLabel; }

}

