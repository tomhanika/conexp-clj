package org.latdraw.util;

public class MyInt {

  private int x;

  public MyInt(int x) {
    this.x = x;
  }

  public void increment() {
    this.x = this.x + 1;
  }

  public void increment(int n) {
    this.x = this.x + n;
  }

  public void decrement() {
    this.x = this.x - 1;
  }

  public void decrement(int n) {
    this.x = this.x - n;
  }

  public int value() {
    return this.x;
  }

  public boolean equals(Object obj) {
    return x == ((MyInt)obj).value();
  }

  public String toString(MyInt obj) {
    return Integer.toString(x);
  }
}
