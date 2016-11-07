package org.latdraw.orderedset;

import java.util.*;
import org.latdraw.util.*;

public class ChainDecomposition {

  int numChains;	// number of chains
  HashMap chain_ht;	// the keys are the labels of the elements;
			// the values are int's telling what chain 
			// the elem is in.

  /**
   * This is the incomplete chain decomposition given by Listing 11.6
   * in my <it>Free Lattice</it> book.
   *
   * @see "Free Lattices, by Freesem, Jezek, and Nation"
   */
  public ChainDecomposition(OrderedSet poset) {
    
    Integer h = new Integer(0);
    HashMap visited = new HashMap(poset.card());
    HashMap chain_ht = new HashMap(poset.card());
    List elems = poset.univ();
    for (Iterator it = elems.iterator(); it.hasNext(); ) {
      visited.put(((POElem)it.next()).label(), Boolean.FALSE);
    }
    for (Iterator it = elems.iterator(); it.hasNext(); ) {
      POElem x = (POElem)it.next();
      if (Boolean.FALSE.equals(visited.get(x.label()))) {
        visited.put(x.label(), Boolean.TRUE);
	chain_ht.put(x.label(), h);
        for (Iterator uc = x.upperCovers().iterator(); uc.hasNext(); ) {
	  POElem y = (POElem)uc.next(); 
	  if (Boolean.FALSE.equals(visited.get(y.label()))) {
            visited.put(y.label(), Boolean.TRUE);
	    chain_ht.put(y.label(), h);
	    uc = y.upperCovers().iterator();
	    x = y;
	  }
	}
	h = new Integer(1 + h.intValue());
      }
    }
    numChains = h.intValue();
    this.chain_ht = chain_ht;
  }

  public int numChains() {
    return numChains;
  }
  public HashMap getHashMap() {
    return chain_ht;
  }
  public int chainNum(POElem x) {
    return ((Integer)chain_ht.get(x.label())).intValue();
  }
}
    
