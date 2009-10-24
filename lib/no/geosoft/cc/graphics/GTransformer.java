package no.geosoft.cc.graphics;



import no.geosoft.cc.geometry.Matrix4x4;



/**
 * World-to-device transformation object.
 * 
 * @author <a href="mailto:jacob.dreyer@geosoft.no">Jacob Dreyer</a>
 */   
public class GTransformer
{
  private final Matrix4x4  world2DeviceMatrix_;
  private final Matrix4x4  device2WorldMatrix_;
  

  
  /**
   * Create a new transformer instance based on the specified viewport
   * and world extent.
   * 
   * @param viewport     Viewport of transformer.
   * @param worldExtent  World extent of transformer.
   */
  GTransformer (GViewport viewport, GWorldExtent worldExtent)
  {
    world2DeviceMatrix_ = new Matrix4x4();
    device2WorldMatrix_ = new Matrix4x4();
    
    update (viewport, worldExtent);
  }
  

  
  /**
   * Reset this transformer instance based on a specified viewport
   * and world extent.
   * 
   * @param viewport     Viewport.
   * @param worldExtent  World extent.
   */
  void update (GViewport viewport, GWorldExtent worldExtent)
  {
    double[] w0    = worldExtent.get(0);
    double[] w1    = worldExtent.get(1);
    double[] w2    = worldExtent.get(2);

    int     x0     = viewport.getX0();
    int     y0     = viewport.getY0();
    int     width  = (int) Math.round (viewport.getWidth());
    int     height = (int) Math.round (viewport.getHeight());

    world2DeviceMatrix_.setWorld2DeviceTransform (w0, w1, w2, 
                                                  x0, y0, width, height);
    device2WorldMatrix_.set (world2DeviceMatrix_);
    device2WorldMatrix_.invert();
  }



  /**
   * Convert a world coordinate to device.
   * 
   * @param wx  X of world coordinate to convert
   * @param wy  Y of world coordinate to convert.
   * @param wz  Z of world coordinate to convert.
   * @return    Device coordinate [x,y].
   */
  public int[] worldToDevice (double wx, double wy, double wz)
  {
    double[] world = {wx, wy, wz};
    return worldToDevice (world);
  }


  
  /**
   * Convert a world coordinate to device. Ignore Z value.
   * 
   * @param wx  X of world coordinate to convert
   * @param wy  Y of world coordinate to convert.
   * @return    Device coordinate [x,y].
   */
  public int[] worldToDevice (double wx, double wy)
  {
    double[] world = {wx, wy, 0.0};
    return worldToDevice (world);
  }


  
  /**
   * Convert a world coordinate to device.
   * 
   * @param world  World coordinate to convert [x,y,z].
   * @return       Device coordinate [x,y]
   */
  public int[] worldToDevice (double world[])
  {
    double result[] = new double[3];
    result = world2DeviceMatrix_.transformPoint (world);
    int device[] = {(int) Math.round (result[0]),
                    (int) Math.round (result[1])};    
    return device;
  }


  
  /**
   * Convert a device coordinate to world.
   * 
   * @param x  X of device coordinate to convert.
   * @param y  Y of device coordinate to convert.   
   * @return   World coordinate [x,y,z].
   */
  public double[] deviceToWorld (int x, int y)
  {
    int[] device = {x, y};
    return deviceToWorld (device);
  }



  /**
   * Convert a device coordinate to world.
   * 
   * @param device  Device coordinate [x,y].
   * @return        World coordinate [x,y,z].
   */
  public double[] deviceToWorld (int device[])
  {
    double[] d = {device[0], device[1], 1.0};
    return device2WorldMatrix_.transformPoint (d);
  }

  

  // TODO: Add methods for converting collection of points
}
