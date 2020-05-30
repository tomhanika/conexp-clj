import static conexp.contrib.java.Main.*;

public class Test {
  public static void main(String... args) {
    Object context = random_context(10,10,0.5);
    System.out.println(context_to_string(context));
    return;
  }
}

