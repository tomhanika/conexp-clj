package org.latdraw.orderedset;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class ChainDecomposition {

  private final int numChains;
  /** Maps the label of an element to the number of the chain it lies in. */
  private final Map<String, Integer> chain_ht;

  /**
   * This is the incomplete chain decomposition given by Listing 11.6
   * in my <it>Free Lattice</it> book.
   *
   * @see "Free Lattices, by Freesem, Jezek, and Nation"
   */
  public ChainDecomposition(OrderedSet poset) {
    
    int h = 0;
    Map<String, Boolean> visited = new HashMap<>(poset.card());
    Map<String, Integer> chains = new HashMap<>(poset.card());
    List<POElem> elems = poset.univ();
    for (POElem x : elems) {
      visited.put(x.label(), Boolean.FALSE);
    }
    for (POElem start : elems) {
      POElem x = start;
      if (Boolean.FALSE.equals(visited.get(x.label()))) {
        visited.put(x.label(), Boolean.TRUE);
        chains.put(x.label(), h);
        // the iterator is deliberately replaced inside the loop: this walks up
        // one chain, hopping to the covers of whichever element it just took
        for (Iterator<POElem> uc = x.upperCovers().iterator(); uc.hasNext(); ) {
          POElem y = uc.next();
          if (Boolean.FALSE.equals(visited.get(y.label()))) {
            visited.put(y.label(), Boolean.TRUE);
            chains.put(y.label(), h);
            uc = y.upperCovers().iterator();
            x = y;
          }
        }
        h++;
      }
    }
    numChains = h;
    this.chain_ht = chains;
  }

  public int numChains() {
    return numChains;
  }
  public Map<String, Integer> getHashMap() {
    return chain_ht;
  }
  public int chainNum(POElem x) {
    return chain_ht.get(x.label());
  }
}
    
