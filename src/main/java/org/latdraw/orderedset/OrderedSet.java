package org.latdraw.orderedset;

/* OrderedSet.java	96/20/6

I noted that the lisp code found a linear extension by taking the minimal
elements then the minimal of what remains, etc. I had assumed that this
would order it with increasing ranks (of my rank function).
It doesn't so I calculate the ranks then do a quicksort to rearrange
elems. Then I do a setElemOrder a second time.

The language specification may not guarentee that v.iterator() has
the same order as the Vector v. 
I checked: it does guarentee that it has the right order. 

I noted that the lisp code for finding filter from and edge set E
did not truly implement the fast algorithm of "Free Lattices" since
the uppers of each element were not topologically sorted. The algorithm 
is still correct, just not guaranteed to be as fast. Since this only matters
if E properly contains E_cov this is not important for the lisp
program since that rarely happens. 

A fast way to sort the upper covers lists is to first find a linear
extension of P then covert the upper covers to lower covers and then
back again using the lin ext of P.


*/



import java.util.*;
import java.io.*;
import org.latdraw.util.SimpleList;
import org.latdraw.util.MyInt;


/**
 * 
 *
 * Note: labels are allowed to be any Object, not just Strings.
 */
public class OrderedSet {

  private List<POElem> elems;            // topologically sorted
  private Map<Object, POElem> elems_ht;    // labels to elems
  private Map<Object, Integer> elemOrder;  // labels to their index
  private boolean[][] leqTable;

  private String name = null;

  private OrderedSet(String name) {
    setName(name);
  }

  public OrderedSet(String name, List labels, List ucs) 
                                          throws NonOrderedSetException {
    setName(name);
    if (labels.size() == 0) {
       throw new NonOrderedSetException(NonOrderedSetException.EMPTY_ERROR);
    }
    linExt(labels, ucs);
    setElemOrder();
    setFilters();
    setLeq();
    setIdeals();
    setUpperCoversFromFilters();
    setLowerCovers();
    setRanks();
    Collections.sort(elems);
    /*
    Collections.sort(elems, new Comparator<POElem>() {
        public int compare(POElem a, POElem b) {
          return a.rank() - b.rank();
        }
      });
    */
    setElemOrder();
    setLeq();
    setIncomparables();
  }

  public OrderedSet(InputLattice in) throws NonOrderedSetException {
    this(in.name, in.labels, in.upperCoversList);
  }

  // public methods

  /**
   * Construct an ordered set from its list of elems, which are Objects,
   * and the filter for each label.
   *
   * @param labels   a list of Object's representing the elements.
   * @param filters   a list of Collection's, in the same order as
   *                  <tt>labels</tt> representing the corresponding filter.
   */
  public static OrderedSet orderedSetFromFilters(String name, List labels, 
                               List filters) throws NonOrderedSetException {
    OrderedSet ans = new OrderedSet(name);
    if (labels.size() == 0) {
       throw new NonOrderedSetException(NonOrderedSetException.EMPTY_ERROR);
    }
    List[] arr = ans.linExt(labels, filters, true);
    List ucs = upperCoversFromFilters(arr[0], arr[1]);
    ans.linExt(arr[0], ucs);
    ans.setElemOrder();
    ans.setFilters();
    ans.setLeq();
    ans.setIdeals();
    ans.setUpperCoversFromFilters();
    ans.setLowerCovers();
    ans.setRanks();
    Collections.sort(ans.elems);
    /*
    Collections.sort(ans.elems, new Comparator<POElem>() {
        public int compare(POElem a, POElem b) {
          return a.rank() - b.rank();
        }
      });
    */
    ans.setElemOrder();
    ans.setLeq();
    ans.setIncomparables();
    return ans;    
  }
  

  /**
   * Find the upper covers of all the elements.
   * Labels must be topologically sorted for this to be correct.
   *
   *
   * @see "Free Lattice by Freese, Jezek and Nation, Listing 11.8"
   */
  public static List<List<Object>> upperCoversFromFilters(List<?> labels,
                                                          List<?> filters) {
    final int n = labels.size();
    List<List<Object>> ans = new ArrayList<>(n);
    Map<Object, Set<?>> filtersHM = new HashMap<>(n);
    for (int i = 0; i < n; i++) {
      filtersHM.put(labels.get(i), new HashSet<>((Collection<?>) filters.get(i)));
    }
    SimpleList labels2 = new SimpleList(labels);
    for (Object a : labels) {
      labels2 = labels2.rest();  // pop labels2
      final List<Object> uc = new ArrayList<>();
      for (Iterator<?> it2 = labels2.iterator(); it2.hasNext(); ) {
        Object x = it2.next();
        if (filtersHM.get(a).contains(x)) {
          boolean isCover = true;
          for (Object covered : uc) {
            if (filtersHM.get(covered).contains(x)) {
              isCover = false;
              break;
            }
          }
          if (isCover) uc.add(x);
        }
      }
      ans.add(uc);
    }
    return ans;
  }


  public void setName(String n) { name = n; }
  public String getName() { return name; }

  public int card() {
    return elems.size();
  }

  public List<POElem> univ() {
    return elems;
  }

  public POElem one() {
    return elems.get(card() - 1);
  }

  public POElem zero() {
    return elems.get(0);
  }

  public POElem getElement(Object label) {
    return (POElem)elems_ht.get(label);
  }

  public int elemOrder(POElem elem) {
    return ((Integer)elemOrder.get(elem.getUnderlyingObject())).intValue();
  }

  public boolean leq(POElem x, POElem y) {
    return leqTable[elemOrder(x)][elemOrder(y)];
  }

  public boolean lt(POElem x, POElem y) {
    return elemOrder(x) != elemOrder(y) && leqTable[elemOrder(x)][elemOrder(y)];
  }

  public boolean geq(POElem x, POElem y) {
    return leqTable[elemOrder(y)][elemOrder(x)];
  }

  public boolean gt(POElem x, POElem y) {
    return elemOrder(x) != elemOrder(y) && leqTable[elemOrder(y)][elemOrder(x)];
  }

  //  helpers

  List[] linExt(List labels, List ucs) throws NonOrderedSetException {
    return linExt(labels, ucs, false);
  }

  /**
   * Sorts the elements into a linear extension. If save is true
   * then this returns an array of two List's which are linearly sorted 
   * version of labels and ucs. Otherwise is sets the elements which are
   * a list of POElem's.
   */
  List<?>[] linExt(List<?> labels, List<?> ucs, boolean save)
                                             throws NonOrderedSetException {
    final int n = labels.size();
    /*
      System.out.println("labels has size " + n);
      System.out.println("ucs has size " + ucs.size());
    */
    //Stack S = new Stack();
    SimpleList Z = SimpleList.EMPTY_LIST;
    SimpleList ZNew = SimpleList.EMPTY_LIST;
    List<Object> ans = new ArrayList<>(n);
    Map<Object, Collection<?>> uc = new HashMap<>(n);
    Map<Object, MyInt> in = new HashMap<>(n);
    for (int i = 0; i < n; i++) {
      in.put(labels.get(i), new MyInt(0));
    }
    for (int i = 0; i < n; i++) {
      Object a = labels.get(i);
      Collection<?> upperCovers_a = (Collection<?>) ucs.get(i);
      uc.put(a, upperCovers_a);
      for (Object b : upperCovers_a) {
        if (! a.equals(b)) {
          in.get(b).increment();
        }
      }
    }
    for (Object a : labels) {
      if (0 == in.get(a).value()) {
        Z = Z.cons(a);
      }
    }
    while (true) {
      if (Z.isEmpty()) {
        if (ZNew.isEmpty()) {
          break;
        } else {
          Z = ZNew;
          ZNew = SimpleList.EMPTY_LIST;
        }
      }
      Object a = Z.first();
      Z = Z.rest();
      ans.add(a);
      for (Object b : uc.get(a)) {
        if (! a.equals(b)) {
          in.get(b).decrement();
          if (0 == in.get(b).value()) ZNew = ZNew.cons(b);
        }
      }
    }
    if (ans.size() != n) {
        throw new NonOrderedSetException();
    }
    if (save) {
      List<Collection<?>> ucs2 = new ArrayList<>(n);
      for (Object label : ans) {
        ucs2.add(uc.get(label));
      }
      return new List<?>[] {ans, ucs2};
    }
    else {
      elems = new ArrayList<>(n);
      elems_ht = new HashMap<>();
      for (int i = 0; i < n; i++) {
        Object label = ans.get(i);
        POElem elem = new POElem(label, this);
        elems.add(elem);
        elems_ht.put(label, elem);
      }
      for (POElem elem : elems) {
        // iterated rather than indexed: the covers arrive as a Collection and
        // were cast to List here, which would have failed for anything else
        Collection<?> up_covs = uc.get(elem.getUnderlyingObject());
        List<POElem> ucs2 = new ArrayList<>(up_covs.size());
        for (Object label : up_covs) {
          ucs2.add(elems_ht.get(label));
        }
        elem.setUpperCovers(ucs2);
      }
      return null;
    }
  }

  void setElemOrder() {
    elemOrder = new HashMap<>(this.card());
    int k = 0;
    for (POElem elem : elems) {
      // was label()
      elemOrder.put(elem.getUnderlyingObject(), k++);
    }
  }

/* 
   g is an array of size k (the number of chains in the chain dec).
   Each element of the array is a hashtable. 
   The keys of the hashtables are the labels of the elements of the 
   ordered set.
   The values are a LinkedList with those elements in chain i above x.

   The algorithm for this is Listing 11.5 of "Free Lattices."
*/

  /**
   * This follow Listing 11.5 in my book <it>Freese Lattices</it>.
   *
   * @see "Free Lattices"
   */
  void setFilters() {
    int n = this.card();
    List<POElem> elemsRev = new ArrayList<>(n);
    ChainDecomposition chainDec = new ChainDecomposition(this);
    int k = chainDec.numChains();
    // one map per chain, from an element's label to the part of its filter
    // contributed by that chain
    @SuppressWarnings("unchecked")
    Map<Object, SimpleList>[] g = new Map[k];
    for (int h = 0; h < k; h++) {
      g[h] = new HashMap<>();
    }
    // reverse the order of the elements:
    for (int i = n-1; i >= 0; i--) {
      elemsRev.add(elems.get(i));
    }
    for (POElem x : elemsRev) {
      for (int h = 0; h < k; h++) {
        g[h].put(x.getUnderlyingObject(), SimpleList.EMPTY_LIST);
      }
    }
    for (POElem x : elemsRev) {
      for (POElem y : x.upperCovers()) {
        if (x == y) continue;
        for (int h = 0; h < k; h++) {
          Object xLabel = x.getUnderlyingObject();
          Object yLabel = y.getUnderlyingObject();
          if (g[h].containsKey(yLabel) && ! g[h].get(yLabel).isEmpty()) {
            if (! g[h].containsKey(xLabel) || g[h].get(xLabel).isEmpty()) {
              g[h].put(xLabel, g[h].get(yLabel));
            } else {
              POElem x_h = (POElem) g[h].get(xLabel).first();
              POElem y_h = (POElem) g[h].get(yLabel).first();
              if (elemOrder(y_h) < elemOrder(x_h)) {
                g[h].put(xLabel, g[h].get(yLabel));
              }
            }
          }
        }
      }
      int index = chainDec.chainNum(x);
      g[index].put(x.getUnderlyingObject(),
                   new SimpleList(x, g[index].get(x.getUnderlyingObject())));
    }
    for (POElem x : elemsRev) {
      List<POElem> filter_x = new ArrayList<>();
      // Make sure the chain with x is the first one processed
      // so x is the first element of x.filter.
      int index = chainDec.chainNum(x);
      for (Iterator<?> it = g[index].get(x.getUnderlyingObject()).iterator();
           it.hasNext(); ) {
        filter_x.add((POElem) it.next());
      }
      for (int h = 0; h < k; h++) {
        if (index != h) {
          for (Iterator<?> it = g[h].get(x.getUnderlyingObject()).iterator();
               it.hasNext(); ) {
            filter_x.add((POElem) it.next());
          }
        }
      }
      x.setFilter(filter_x);
    }
  }

  void setLeq () {
    final int n = card();
    final boolean[][] table = new boolean[n][n];
    Iterator list = univ().iterator();
    Iterator list2; 
    POElem x;
    int k,j;
    List filter;
    for (k=0; k < n; k++) {
      for (j=0; j < n; j++) {
	table[k][j] = false;
      }
    }
    while (list.hasNext()) {
      x = (POElem)list.next();
      k = elemOrder(x);
      filter = x.filter();
      list2 = filter.iterator();
      while (list2.hasNext()) {
	j = elemOrder((POElem)list2.next());
	table[k][j] = true;
      }
    }
    leqTable = table;
  }

  void setIdeals() {
    final int n = card();
    List<List<POElem>> a = new ArrayList<>(n);
    for (int i = 0; i < n; i++) {
      a.add(new ArrayList<>());
    }
    for (POElem x : elems) {
      for (POElem y : x.filter()) {
        a.get(elemOrder(y)).add(x);
      }
    }
    for (int i = 0; i < n; i++) {
      elems.get(i).setIdeal(a.get(i));
    }
  }

  void setLowerCovers() {
    final int n = card();
    List<List<POElem>> a = new ArrayList<>(n);
    for (int i = 0; i < n; i++) {
      a.add(new ArrayList<>());
    }
    for (POElem x : elems) {
      for (POElem y : x.upperCovers()) {
        a.get(elemOrder(y)).add(x);
      }
    }
    for (int i = 0; i < n; i++) {
      elems.get(i).setLowerCovers(a.get(i));
    }
  }

  //This follows the Listing 11.8 on page 215 of Free Lattices

// not sure this is ok?  1/11/04
  void setUpperCoversFromFilters () {
    final int n = card();
    List<List<POElem>> ucs = new ArrayList<>(n);
    for (int i = 0; i < n; i++) {
      ucs.add(new ArrayList<>());
    }
    for (int i = 0; i < n; i++) {
      POElem a = elems.get(i);
      int k = elemOrder(a);			// elemOrder of a
      for (int j = i + 1; j < n; j++) {
        POElem x = elems.get(j);
        if (leq(a, x)) {
          List<POElem> uc = ucs.get(k);
          int m = uc.size();
          int p = 0;
          while (p < m && ! leq(uc.get(p), x)) {
            p++;
          }
          if (p == m) uc.add(x);
        }
      }
    }
    for (int i = 0; i < n; i++) {
      elems.get(i).setUpperCovers(ucs.get(i));
    }
  }

  void setRanks () {
    int n = card();
    int[] heights = new int[n];
    int[] depths = new int[card()];
    int height, depth, height2, depth2;
    int max_depth = 0;
    List lcs, ucs;
    Iterator list;
    for(int i=0; i < n; i++) {
      height = 0;
      lcs = ((POElem)elems.get(i)).lowerCovers();
      list = lcs.iterator();
      while (list.hasNext()) {
	height2 = 1 + heights[elemOrder((POElem)list.next())];
	if (height2 > height) height = height2;
      }
      heights[i] = height;
    }
    for(int i=n-1; i >= 0; i--) {
      depth = 0;
      ucs = ((POElem)elems.get(i)).upperCovers();
      list = ucs.iterator();
      while (list.hasNext()) {
	depth2 = 1 + depths[elemOrder((POElem)list.next())];
	if (depth2 > depth) depth = depth2;
      }
      depths[i] = depth;
      if (depth > max_depth) max_depth = depth;
    }
    for(int i=0; i < n; i++) {
      ((POElem)elems.get(i)).setRank(max_depth + heights[i] - depths[i]);
    }
  }

  // x.highIncomparables will be a list of those elements incomparble with x
  // which come after it in the linear order.

  void setIncomparables() {
    int n = elems.size();
    for (int i = 0 ; i < n - 1; i++) {
      POElem x = elems.get(i);
      SimpleList list = SimpleList.EMPTY_LIST;
      for (int j = i + 1; j < n; j++) {
        if (! leq(x, elems.get(j))) list = list.cons(elems.get(j));
      }
      // a SimpleList and not an ArrayList: `cons` prepends, so this list runs
      // backwards through the linear order, and the diagram depends on that
      @SuppressWarnings("unchecked")
      List<POElem> incomparables = (List<POElem>) (List<?>) list;
      x.setHighIncomparables(incomparables);
    }
  }


}

