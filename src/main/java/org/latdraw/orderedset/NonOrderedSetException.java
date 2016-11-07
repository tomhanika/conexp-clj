package org.latdraw.orderedset;


public class NonOrderedSetException extends Exception {

  public static final String CYCLE_ERROR = 
                 "This is not an ordered set; it contains a cycle.";
  
  public static final String EMPTY_ERROR = 
                 "This is not an ordered set; it is empty.";
  
  public NonOrderedSetException() {
    super(CYCLE_ERROR);
  }

  public NonOrderedSetException(String str) {
    super(str);
  }

}
