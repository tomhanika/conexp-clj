/* IntArray.java 2001/06/04 Ralph Freese */


package org.latdraw.partition;

import java.util.*;

/**
 * This class is a wrapper for an array of int's mainly so we can 
 * specify the equals and hashCode methods.
 *
 * @author Ralph Freese
 * @version $Id: IntArray.java,v 1.1 2008/08/01 19:30:57 ralph Exp $
 */
public class IntArray implements Cloneable {

  protected int size;
  protected int[] array;


  public IntArray() {
    this.array = null;
    this.size = -1;
  }

  public IntArray(int[] array) {
    this.array = array;
    this.size = array.length;
  }

  public IntArray(int size) {
    this.size = size;
    this.array = new int[size];
  }

  public final boolean equals(Object obj) {
    if (obj == null) return false;
    try {
      IntArray p = (IntArray)obj;
      return equalIntArrays(p.array, array);
    }
    catch (ClassCastException e) { return false; }
    catch (ArrayIndexOutOfBoundsException e) { return false; }
  }

  // something better ??? Maybe replace 2 with this.size.
/*
  public int hashCode() {
    int ans = 0;
    int power = 1;
    int n = array.length;
    for (int i = 0; i < n; i++) {
      ans = ans + power * (array[i] < 0 ? - array[i] : array[i]);
      power = 2 * power;
    }
    return ans;
  }
*/

  public final int hashCode() {
    int ans = 1;
    int n = array.length;
    for (int i = 0; i < n; i++) {
      //ans = ans * 31 + (array[i] < 0 ? - array[i] : array[i]);
      ans = ans * 31 + array[i];
    }
    return ans;
  }

  public final int[] toArray() {
    return array;
  }


  public final int[] getArray() {
    return array;
  }

  public final int size() {
    return size;
  }

  public final void setIntArray(int[] v) {
    array = v;
  }

  public final int get(int i) {
    return array[i];
  }

  public final void set(int index, int value) {
    array[index] = value;
  }

  public Object clone() {
    IntArray ia = new IntArray(size);
    int[] arr = ia.getArray();
    for (int i = 0; i < size; i++) {
      arr[i] = array[i];
    }
    return ia;
  }

  public String toString() {
    return intArrayToString(array);
  }

  public static final String intArrayToString(int [] array) {
    if (array.length == 0) return "[]";
    StringBuffer sb = new StringBuffer("[");
    for (int i = 0; i < array.length; i++) {
      sb.append(String.valueOf(array[i]));
      sb.append(", ");
    }
    sb.setLength(sb.length() - 2);
    sb.append("]");
    return sb.toString();
  }

  public static final boolean equalIntArrays(int[] u, int[] v) {
    final int n = u.length;
    if (n != v.length) return false;
    for (int i = 0; i < n; i++) {
      if (u[i] != v[i]) return false;
    }
    return true;
  }

}

