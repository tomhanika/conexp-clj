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

  @Override
  public boolean equals(Object obj) {
    return obj instanceof MyInt other && x == other.value();
  }

  @Override
  public int hashCode() {
    return Integer.hashCode(x);
  }

  @Override
  public String toString() {
    return Integer.toString(x);
  }
}
