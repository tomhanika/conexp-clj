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
  private HashMap elems_ht;      // labels to elems
  private HashMap elemOrder;     // labels to Integers
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
  public static List upperCoversFromFilters(List labels, List filters) {
    final int n = labels.size();
    List ans = new ArrayList(n);
    HashMap filtersHM = new HashMap(n);
    for (int i = 0; i < n; i++) {
      filtersHM.put(labels.get(i), new HashSet((Collection)filters.get(i)));
    }
    SimpleList labels2 = new SimpleList(labels);
    for (Iterator it = labels.iterator(); it.hasNext(); ) {
      Object a = it.next();
      labels2 = labels2.rest();  // pop labels2
      final List uc = new ArrayList();
      for (Iterator it2 = labels2.iterator(); it2.hasNext(); ) {
        Object x = it2.next();
        if (((Set)filtersHM.get(a)).contains(x)) {
          boolean isCover = true;
          for (Iterator it3 = uc.iterator(); it3.hasNext(); ) {
            if (((Set)filtersHM.get(it3.next())).contains(x)) {
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
  List[] linExt(List labels, List ucs, boolean save) 
                                             throws NonOrderedSetException {
    final int n = labels.size();
    /*
      System.out.println("labels has size " + n);
      System.out.println("ucs has size " + ucs.size());
    */
    //Stack S = new Stack();
    SimpleList Z = SimpleList.EMPTY_LIST;
    SimpleList ZNew = SimpleList.EMPTY_LIST;
    List ans = new ArrayList(n);
    HashMap uc = new HashMap(n);
    HashMap in = new HashMap(n);
    for (int i = 0; i < n; i++) {
      in.put(labels.get(i),new MyInt(0));
    }
    for (int i = 0; i < n; i++) {
      Object a = labels.get(i);
      Collection upperCovers_a = (Collection)ucs.get(i);
      //int k = upperCovers_a.size();
      uc.put(a, upperCovers_a);
      for (Iterator it = upperCovers_a.iterator(); it.hasNext(); ) {
        Object b = it.next();
	if (! a.equals(b)) {
          ((MyInt)in.get(b)).increment();
	}
      }
      //for (int j=0; j < k; j++) {
	//Object b = upperCovers_a.get(j);
	//if (! a.equals(b)) {
          //((MyInt)in.get(b)).increment();
	//}
      //}
    }
    for (Iterator it = labels.iterator(); it.hasNext(); ) {
      Object a = it.next();
      if (0 == ((MyInt)in.get(a)).value()) {
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
      for (Iterator it = ((Collection)uc.get(a)).iterator(); it.hasNext(); ) {
	Object b = it.next();
	if (! a.equals(b)) {
          ((MyInt)in.get(b)).decrement();
	  if (0 == ((MyInt)in.get(b)).value()) ZNew = ZNew.cons(b);
        }
      }
    }
    if (ans.size() != n) {
        throw new NonOrderedSetException();
    }
    if (save) {
      List ucs2 = new ArrayList(n);
      for (Iterator it = ans.iterator(); it.hasNext(); ) {
        ucs2.add(uc.get(it.next()));
      }
      return new List[] {ans, ucs2};
    }
    else {
      elems = new ArrayList(n);
      elems_ht = new HashMap();   
      for (int i = 0; i < n; i++) {
	Object label = ans.get(i);
        POElem elem = new POElem(label, this);
	elems.add(elem);
	elems_ht.put(label, elem);
      }
      for (int i = 0; i < n; i++) {
	POElem elem = (POElem)elems.get(i);
	List up_covs = (List)uc.get(elem.getUnderlyingObject());
        int k = up_covs.size();
	List ucs2 = new ArrayList(k);
        for (int j = 0; j < k; j++) {
	  ucs2.add(elems_ht.get(up_covs.get(j)));
	}
	elem.setUpperCovers(ucs2);
      }
      this.elems = elems;
      this.elems_ht = elems_ht;
      return null;
    }
  }

  void setElemOrder() {
    elemOrder = new HashMap(this.card());
    int k = 0;
    for (Iterator it = elems.iterator(); it.hasNext(); ) {
      // was label()
      elemOrder.put(((POElem)it.next()).getUnderlyingObject(), 
                                                   new Integer(k++));
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
    Iterator uc_x;
    POElem x,y,x_h,y_h;
    int n = this.card();
    List elemsRev = new ArrayList(n);
    ChainDecomposition chainDec = new ChainDecomposition(this);
    int k = chainDec.numChains();
    HashMap[] g = new HashMap[k];
    Iterator list; 
    SimpleList llist;
    int index;
    Iterator list_xh;
    for (int h = 0; h < k; h++) {
      g[h] = new HashMap();
    }
    // reverse the order of the elements:
    for (int i = n-1; i >= 0; i--) {
      elemsRev.add(elems.get(i));
    }
    list = elemsRev.iterator();
    while (list.hasNext()) {
      x = (POElem)list.next();
      for(int h = 0; h < k; h++) {
	//g[h].put(x.label(), SimpleList.EMPTY_LIST);
	g[h].put(x.getUnderlyingObject(), SimpleList.EMPTY_LIST);
      }
    }
    list = elemsRev.iterator();
    while (list.hasNext()) {
      x = (POElem)list.next();
      uc_x = x.upperCovers().iterator();
      while (uc_x.hasNext()) {
	y = (POElem)uc_x.next();
        if (x == y) continue;
	for(int h=0; h < k; h++) {
	  if (g[h].containsKey(y.getUnderlyingObject()) && 
		(! ((SimpleList)g[h].get(y.getUnderlyingObject())).isEmpty())) {
	    if ((! g[h].containsKey(x.getUnderlyingObject())) || 
		((SimpleList)g[h].get(x.getUnderlyingObject())).isEmpty()) {
	      g[h].put(x.getUnderlyingObject(), g[h].get(y.getUnderlyingObject()));
            } else {
              x_h = (POElem)((SimpleList)g[h].get(x.getUnderlyingObject())).first();
              y_h = (POElem)((SimpleList)g[h].get(y.getUnderlyingObject())).first();
              if (elemOrder(y_h) < elemOrder(x_h)) {
	        g[h].put(x.getUnderlyingObject(), g[h].get(y.getUnderlyingObject()));
              }
	    }
	  }
	}
      }
      index = chainDec.chainNum(x);
      llist = new SimpleList(x, (SimpleList)g[index].get(x.getUnderlyingObject()));
      g[index].put(x.getUnderlyingObject(), llist);
    }
    list = elemsRev.iterator();
    while (list.hasNext()) {
      x = (POElem)list.next();
      List filter_x = new ArrayList();
      // Make sure the chain with x is the first one processed
      // so x is the first element of x.filter.
      index = chainDec.chainNum(x);
      list_xh = 
        ((SimpleList)g[index].get(x.getUnderlyingObject())).iterator();
      while (list_xh.hasNext()) {
        filter_x.add(list_xh.next());
      }
      for(int h = 0; h < k; h++) {
        if (index != h) {
	  list_xh = ((SimpleList)g[h].get(x.getUnderlyingObject())).iterator();
	  while (list_xh.hasNext()) {
	    filter_x.add(list_xh.next());
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
    List[] a = new List[n];
    Iterator list = elems.iterator();
    Iterator list2;
    int k;
    POElem x;
    for (int i = 0; i < n; i++) {
      a[i] = new ArrayList();
    }
    while (list.hasNext()) {
      x = (POElem)list.next();
      list2 = x.filter().iterator();
      while (list2.hasNext()) {
        k =  elemOrder((POElem)list2.next());
        a[k].add(x);
      }
    }
    for(int i = 0; i < n; i++) {
      x = (POElem)elems.get(i);
      x.setIdeal(a[i]);
    }
  }

  void setLowerCovers() {
    final int n = card();
    List[] a = new List[n];
    Iterator list = elems.iterator();
    Iterator list2;
    int k;
    POElem x;
    for (int i = 0; i < n; i++) {
      a[i] = new ArrayList();
    }
    while (list.hasNext()) {
      x = (POElem)list.next();
      list2 = x.upperCovers().iterator();
      while (list2.hasNext()) {
        k =  elemOrder((POElem)list2.next());
        a[k].add(x);
      }
    }
    for(int i = 0; i < n; i++) {
      x = (POElem)elems.get(i);
      x.setLowerCovers(a[i]);
    }
  }

  //This follows the Listing 11.8 on page 215 of Free Lattices

// not sure this is ok?  1/11/04
  void setUpperCoversFromFilters () {
    final int n = card();
    List[] ucs = new List[n];
    List uc;
    Iterator T;
    int k, m;
    POElem a, x;
    for (int i = 0; i < n; i++) { 
      ucs[i] = new ArrayList();
    }
    for (int i = 0; i < n; i++) {
      a = (POElem)elems.get(i);
      k =  elemOrder(a);			// elemOrder of a
      for (int j = i + 1; j < n; j++) {
        x =  (POElem)elems.get(j);
        if (leq(a,x)) {
          uc = ucs[k];
          m = uc.size();
          int p = 0;
          while (p < m && ! leq((POElem)uc.get(p), x)) {
            p++;
          }
          if (p == m) uc.add(x);
        }
      }
    }
    for(int i = 0; i < n; i++) {
      a = (POElem)elems.get(i);
      a.setUpperCovers(ucs[i]);
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
      POElem x = (POElem)elems.get(i);
      SimpleList list = SimpleList.EMPTY_LIST;
      for (int j = i + 1; j < n; j++) {
        if (! leq(x, (POElem)elems.get(j))) list = list.cons(elems.get(j));
      }
      x.setHighIncomparables(list);
    }
  }
	
  public static void main(String args[]) 
		throws FileNotFoundException,IOException {
    if (args.length == 0) {
      List elems = new ArrayList(5);
      List ucs = new ArrayList(5);
      elems.add("0");
      elems.add("a");
      elems.add("b");
      elems.add("c");
      elems.add("1");
      List cov_0 = new ArrayList(2);
      List cov_a = new ArrayList(1);
      List cov_b = new ArrayList(1);
      List cov_c = new ArrayList(1);
      List cov_1 = new ArrayList(0);
      cov_0.add("b");
      cov_0.add("c");
      cov_a.add("1");
      cov_b.add("1");
      cov_c.add("a");
      ucs.add(cov_0);
      ucs.add(cov_a);
      ucs.add(cov_b);
      ucs.add(cov_c);
      ucs.add(cov_1);
      //linExt(elems,ucs);
      OrderedSet poset;
      try {
        poset = new OrderedSet("Test-Main", elems,ucs);
      } catch (NonOrderedSetException e) {
        System.out.println(e.getMessage());
        return;
      }
      System.out.println("\nThe size is " + poset.card());
    } else {
      OrderedSet test = null;
      try {
        test = new OrderedSet(new InputLattice(args[0]));
      } catch (NonOrderedSetException e) {
        System.out.println(e.getMessage());
        return;
      }
      ChainDecomposition chainDec = new ChainDecomposition(test);
      Iterator e = test.univ().iterator();
      while (e.hasNext()) {
	System.out.print(" " + ((POElem)e.next()).getUnderlyingObject());
      }
System.out.println("");
System.out.println("univ: " + test.univ());
System.out.println("chain dec: " + chainDec.numChains());
System.out.println("chain dec: " + chainDec.getHashMap());
System.out.println("chain dec: " + chainDec.getHashMap().size());
System.out.println("elemOrder: " + test.elemOrder);
printElems("Elements: ", test.univ());
System.out.println("testing leq: " + 
test.leq((POElem)test.univ().get(1), (POElem)test.univ().get(2)));
System.out.print("filter of " + ((POElem)test.univ().get(0)).getUnderlyingObject()
        + " is ");
printElems(" ", ((POElem)test.univ().get(0)).filter());
//System.out.print("highIncomparables of " + 
	//((POElem)test.univ().get(2)).getUnderlyingObject() + " are ");
//System.out.println(((POElem)test.univ().get(2)).highIncomparables());
for (int i=0; i < test.card(); i++) {
  System.out.print("highIncomparables of " + 
	((POElem)test.univ().get(i)).getUnderlyingObject() + " are ");
System.out.println(((POElem)test.univ().get(i)).highIncomparables());
}


    }
  }

  static void printElems (String head, List v) {
    System.out.println(head);
    Iterator list = v.iterator();
    while (list.hasNext()) {
      System.out.print(" ");
      POElem x = (POElem)list.next();
      System.out.print(x.getUnderlyingObject());
      System.out.println("\t" + x.rank());
    }
    System.out.println("End");
  }


}

