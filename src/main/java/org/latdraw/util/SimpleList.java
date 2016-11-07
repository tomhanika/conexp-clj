/* 
 * SimpleList.java	98/1/3 Ralph Freese
 */

package org.latdraw.util;

import java.util.*;
import java.lang.reflect.*;
import java.io.*;

/**
 * Simple Linked lists. Java's collection framework has LinkedList's but
 * I need to guarentee that there is a great deal of sharing to save
 * space (memory).
 * For example, the space required to hold all subsets of an 
 * <i>n</i> element set requires space proportional to 
 * (<i>n</i>/2) 2<i><sup>n</sup></i> using vectors, but only
 * 2<i><sup>n</sup></i> with linked lists.
 * <p>
 * This version just has an element and a pointer to the rest, which
 * is another LinkedList. This means push and pop not are supported
 * but rest does not have to make a new object.
 * <p>
 * The rest of the empty list is itself.
 * <p>
 * This class implements <code>java.util.List</code>.
 * <p>
 * <b>Efficiency.</b> size() takes time proportional to the size and
 * get(int i) takes time proportional to i. <b>Moral:</b> do not use 
 * for loops to iterator over the elements; use the iterator. The former
 * uses time proportional to <i>n</i><sup>2</sup> while the latter 
 * uses only <i>n</i>.
 *
 *
 */
public class SimpleList implements List, Serializable {

  protected Object first;
  protected transient SimpleList rest;

  /**
   * The empty list is a class constant
   */
  public static final EmptyList EMPTY_LIST = new EmptyList();

  private SimpleList() {}

  /**
   * Constructs a list with obj followed by list. The same as cons in Lisp.
   *
   * @param obj The Object to be first.
   * @param list The List to be the rest.
   */
  public SimpleList(Object obj, SimpleList list) {
    first = obj;
    rest = list;
  }

  public SimpleList(Collection c) {
    SimpleList tmp = EMPTY_LIST;
    for(Iterator it = c.iterator(); it.hasNext();) {
      tmp = tmp.cons(it.next());
    }
    tmp = tmp.reverse();
    first = tmp.first;
    rest = tmp.rest;
  }

  /**
   * Save the state of the <tt>SimpleList</tt> instance to a stream (that
   * is, serialize it).
   * <p>
   * Following Eric's suggestion: we go down the list 
   * modifing <tt>rest</tt> of each node making it the
   * <tt>EMPTY_LIST</tt>, write it, modify it back. 
   */
  private synchronized void writeObject(ObjectOutputStream s) 
                                                   throws IOException {
    s.defaultWriteObject();
    for (SimpleList lst = rest; !lst.isEmpty(); lst = lst.rest) {
      SimpleList lst2 = lst.rest;      // save lst.rest
      lst.rest = EMPTY_LIST;
      s.writeObject(lst);
      lst.rest = lst2;                 // restore lst.rest
    }
    s.writeObject(EMPTY_LIST);
  }


  /**
   * Reconstitute the <tt>SimpleList</tt> instance from a stream (that is,
   * deserialize it).
   */
  private synchronized void readObject(ObjectInputStream s) 
                              throws IOException, ClassNotFoundException {
    s.defaultReadObject();
    SimpleList lst = this;
    while (!lst.isEmpty()) {
      lst.rest = (SimpleList)s.readObject();
      lst = lst.rest;
    }
  }

  public static SimpleList makeList() {
    return EMPTY_LIST;
  }

  public static SimpleList makeList(Object obj) {
    return EMPTY_LIST.cons(obj);
  }

  public boolean isEmpty() {
    return false;
  }

  /**
   * The size of the list. It is inefficient since it takes time 
   * proportional to the size. 
   */
  public int size() {
    if (isEmpty()) return 0;
    return 1 + rest.size();
  }

  public Object first() {
    return first;
  }

  public SimpleList rest() {
    return rest;
  }

  public SimpleList cons(Object obj) {
    return new SimpleList(obj, this);
  }

  public Enumeration elements() {
    return new EnumerationSimpleList();
  }

  public Iterator iterator() { 
    return new EnumerationSimpleList();
  }

  /**
   * This Iterator will iterate through the list until it reaches
   * <tt>tail</tt> or to the end if tail is not found. Note 
   * <tt>tail</tt> must be == to a tail of the list.
   *
   * @param tail   a list == to a tail of the list.
   */
  public Iterator frontIterator(SimpleList tail) { 
    return new FrontIterator(tail);
  }


/*
  public SimpleList copyList() {
    if (isEmpty()) { 
      return EMPTY_LIST ; 
    } else {
      return new SimpleList (first, rest.copyList()) ;
    }
  }

  public Object clone() {
    return copyList() ;
  }
*/

  /**
   * This corresponds to <code>(APPEND this lst)</code> in lisp.
   */ 
  public SimpleList append(SimpleList lst) {
    if (isEmpty()) return lst;
    return new SimpleList(first(),rest().append(lst));
  }
   
  public SimpleList reverse() {
    return reverse(EMPTY_LIST);
  }

  /**
   * This is revappend in Common Lisp. It produces 
   * <code>
   * (APPEND (REVERSE this) lst) 
   * </code>
   */ 
  public SimpleList reverse(SimpleList lst) {
    if (isEmpty()) return lst;
    SimpleList ans = lst;
    for (Iterator it = iterator(); it.hasNext(); ) {
      ans = new SimpleList(it.next(), ans);
    }
    return ans;
  }

  public String toString() { 
    StringBuffer sb = new StringBuffer("(");
    for(Iterator it = iterator(); it.hasNext(); ) {
      sb.append(it.next());
      if (it.hasNext()) sb.append(" ");
    }
    sb.append(")");
    return sb.toString();
  }

  // methods demanded by List interface

  /**
   * This just throws an UnsupportedOperationException.
   */
  public void add(int index, Object elt) throws 
               UnsupportedOperationException, ClassCastException,
               IllegalArgumentException, IndexOutOfBoundsException {
    throw new UnsupportedOperationException();
  }

  /**
   * This just throws an UnsupportedOperationException.
   */
  public boolean add(Object elt) throws UnsupportedOperationException, 
               ClassCastException, IllegalArgumentException {
    throw new UnsupportedOperationException();
  }

  /**
   * This just throws an UnsupportedOperationException.
   */
  public boolean addAll(int index, Collection c) throws 
               UnsupportedOperationException, ClassCastException,
               IllegalArgumentException, IndexOutOfBoundsException {
    throw new 
        UnsupportedOperationException("SimpleList does not support addAll");
  }

  public boolean addAll(Collection c) throws UnsupportedOperationException, 
               ClassCastException, IllegalArgumentException {
    throw new 
        UnsupportedOperationException("SimpleList does not support addAll");
  }

  /**
   * This just throws an UnsupportedOperationException.
   */
  public void clear() throws UnsupportedOperationException {
    throw new UnsupportedOperationException();
  }

  public boolean contains(Object o) { 
    Iterator it = iterator();
    while(it.hasNext()) {
      if (o.equals(it.next())) return true;
    }
    return false;
  }

  public boolean containsAll(Collection c) {
    Iterator it = c.iterator();
    while(it.hasNext()) {
      if (!contains(it.next())) return false;
    }
    return true;
  }

  public Object get(int i) throws IndexOutOfBoundsException {
    if (i < 0 || i >= size()) throw new IndexOutOfBoundsException();
    Iterator it = iterator();
    int j = 0;
    while(it.hasNext()) {
      if (j++ == i) return it.next();
      it.next();
    }
    return null;		// can't happen
  }

  public int indexOf(Object o) { 
    Iterator it = iterator();
    int j = 0;
    while(it.hasNext()) {
      if (o.equals(it.next())) return j;
      j++;
    }
    return -1;
  }

  public int lastIndexOf(Object o) { 
    Iterator it = iterator();
    int j = 0;
    int ans = -1;
    while(it.hasNext()) {
      if (o.equals(it.next())) ans = j;
      j++;
    }
    return ans;
  }

  public Object remove(int i) throws UnsupportedOperationException { 
    throw new UnsupportedOperationException();
  }

  public boolean remove(Object obj) throws UnsupportedOperationException { 
    throw new UnsupportedOperationException();
  }

  public boolean removeAll(Collection c) throws UnsupportedOperationException { 
    throw new UnsupportedOperationException();
  }

  public boolean retainAll(Collection c) throws UnsupportedOperationException { 
    throw new UnsupportedOperationException();
  }

  public Object set(int i, Object o) throws UnsupportedOperationException { 
    throw new UnsupportedOperationException();
  }

  public Object[] toArray(Object[] a) { 
    int size = size();
    int aSize = a.length;
    if (size > aSize) {
      Class c = a.getClass().getComponentType();
      a = (Object[])Array.newInstance(c, size);
    }
    int i = 0;
    Iterator it = iterator();
    while (it.hasNext()) {
      a[i] = it.next();
      i++;
    }
    for (int j = i; j < aSize; j++) {
      a[j] = null;
    }
    return a;
  }
  
  public Object[] toArray() { 
    //throw new UnsupportedOperationException();
    Iterator it = iterator();
    Object[] ans = new Object[size()];
    int i = 0;
    while (it.hasNext()) {
      ans[i] = it.next();
      i++;
    }
    return ans;
  }

  public java.util.List subList(int i, int j) {
    int k = 0;
    SimpleList ans = EMPTY_LIST;
    Iterator it = iterator();
    while (it.hasNext()) {
      if (i <= k && k < j) ans = ans.cons(it.next());
    }
    return ans.reverse();
  }

  public ListIterator listIterator(int i) {
    return new ListIteratorSimpleList(i);
  }

  public ListIterator listIterator() {
    return new ListIteratorSimpleList();
  }

  
  private static class EmptyList extends SimpleList {

    public EmptyList() {
      first = null;
      rest = this;
    }

    /**
     * Having this insures that the EMPTY_LIST is unique in the JVM.
     */
    private Object readResolve() throws ObjectStreamException {
      return EMPTY_LIST;
    }

    public boolean isEmpty() {
      return true;
    }

  }



  private class ListIteratorSimpleList implements ListIterator {
    private ArrayList alist;
    private ListIterator iter;

    ListIteratorSimpleList() {
      alist = new ArrayList(SimpleList.this);
      iter = alist.listIterator();
    }

    ListIteratorSimpleList(int i) {
      alist = new ArrayList(SimpleList.this);
      iter = alist.listIterator(i);
    }

    public boolean hasNext() {
      return iter.hasNext();
    }

    public Object next() {
      return iter.next();
    }

    public int nextIndex() {
      return iter.nextIndex();
    }

    public boolean hasPrevious() {
      return iter.hasPrevious();
    }

    public Object previous() {
      return iter.previous();
    }

    public int previousIndex() {
      return iter.previousIndex();
    }

    public void add(Object o) {
      throw new UnsupportedOperationException();
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }

    public void set(Object o) {
      throw new UnsupportedOperationException();
    }

  }

  private class FrontIterator implements Iterator {

    private SimpleList llist;
    private SimpleList tail;

    FrontIterator(SimpleList tail) {
      llist = SimpleList.this;
    }

    public boolean hasNext() {
      return ((!(llist != tail)) && (! llist.isEmpty()));
    }

    public Object next() {
      Object obj = llist.first();
      llist = llist.rest();
      return obj;
    }

    public void remove () {
      throw new UnsupportedOperationException();
    }

  }
  
  private class EnumerationSimpleList implements Enumeration, Iterator {
    private SimpleList llist;

    EnumerationSimpleList() {
      llist = SimpleList.this;
    }

    public boolean hasMoreElements() {
      return (! (llist.isEmpty()));
    }

    public boolean hasNext() {
      return (! (llist.isEmpty()));
    }

    public Object nextElement() {
      Object obj = llist.first();
      llist = llist.rest();
      return obj;
    }

    public Object next() {
      Object obj = llist.first();
      llist = llist.rest();
      return obj;
    }

    public void remove () {
      throw new UnsupportedOperationException();
    }

  }

/*
 * Al's slow reverse for time testing.
 */

/*
  public SimpleList rev() {
    if (isEmpty()) {
      return this;
    } else {
      return this.rest.rev().app(new SimpleList(first));
    }
  }
      
  public SimpleList app(SimpleList List) {
    if (isEmpty()) {
      return List ;
    } else {
      return
        rev().rest.rev().app(new SimpleList(rev().first, List));
    }
  }

*/

  public static void main(String[] args) {
    int n;
    if (args.length != 0) {
      n = Integer.parseInt(args[0]);
    } else {
      n = 2;
    }
    SimpleList foo = EMPTY_LIST;
    SimpleList bar = EMPTY_LIST;
    for(int i = 0; i < n; i++) {
      bar = bar.cons(new Integer(i));
      foo = foo.cons(bar);
      //foo = new SimpleList(new Integer(i), foo);
      //foo = foo.cons(new Integer(i));
    }

System.out.println("before: equals? " 
    + (((SimpleList)foo.first()).rest().rest() 
            == ((SimpleList)foo.rest().first()).rest()));


//foo = EMPTY_LIST;
System.out.println("foo is " + foo + ", its identityHC is " + System.identityHashCode(foo));
System.out.println("foo constructed from itself is " + new SimpleList(foo));
//System.out.println("foo.rest() is " + foo.rest());

    n = 4000;		// the readObject1 oveflows at 4000
    SimpleList goo = EMPTY_LIST;
    SimpleList tails = EMPTY_LIST;
    for(int i = 0; i < n; i++) {
      goo = goo.cons(new Integer(i));
      tails = tails.cons(goo);
    }


    try {

      FileOutputStream fileOut = new FileOutputStream("list10");
      ObjectOutputStream out = new ObjectOutputStream(fileOut);
      out.writeObject(foo);
      out.close();

/*
      fileOut = new FileOutputStream("tailsx" + n);
      out = new ObjectOutputStream(fileOut);
      //out.writeObject(new ArrayList(goo));
      out.writeObject(tails);
      //out.writeObject(goo);
      out.close();


      FileInputStream tailsInx = new FileInputStream("tailsx" + n);
      ObjectInputStream inx = new ObjectInputStream(tailsInx);
      SimpleList tailsIn = (SimpleList)inx.readObject();
      System.out.println("tailsx in has size " + tailsIn.size());
*/
      FileInputStream fileIn = new FileInputStream("list10");
      ObjectInputStream in = new ObjectInputStream(fileIn);
      SimpleList fooIn = (SimpleList)in.readObject();
System.out.println("after: fooIn is " + fooIn + " length " + fooIn.size());
//System.out.println("after: tailsIn is " + tailsIn + " length " + tailsIn.size());

System.out.println("after: equals? " 
    + (((SimpleList)fooIn.first()).rest().rest() 
            == ((SimpleList)fooIn.rest().first()).rest()));


    }
    catch(Exception e) {
      e.printStackTrace();
    }

  }

}

