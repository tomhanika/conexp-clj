As conexp-clj is build in top of Clojure, all Java libraries can be used within conexp-clj
programs.  To allow the other direction as well, i.e. using conexp-clj functionality from
Java code, conexp-clj provides a set of static method declarations in the package
`conexp.contrib.java.Main` which correspond to functions in conexp-clj.

All functions (not macros) which are exported by `conexp.main` are available in
`conexp.contrib.java.Main`.  See the [API documentation](API) for a complete list of all
these functions.

For example, the following java code generates a random 10⨯10 context with "cross
probability" 0.5 and prints it in the usual ASCII art.

```java
import static conexp.contrib.java.Main.*;

public class Test {
  public static void main(String... args) {
    Object context = random_context(10,10,0.5);
    System.out.println(context_to_string(context));
    return;
  }
}
```

Note that since Clojure is (normally) not typed statically, the methods in
`conexp.contrib.java.Main` normally operate on `Object` and return `Object`.  See the file
`src/conexp/contrib/java/Main.java` for the exact specification of these methods.  Note
that this file is not part of the conexp-clj source tree, as it is generated automatically
when the code is compiled.

Of course, some of the functions names exported by `conexp.main` are not legal method
names in Java.  For this, these names are translated into valid Java names in the
following way:
- hyphens are replaced by underscores
- "!" and "?" are only allowed as last characters, and are translated to "\_f" and to
  "\_p" respectively
- \<, \>, = are translated to \_lt\_, \_gt\_ and \_eq\_

For example, the function `graph-of-function?` is translated to `graph_of_function_p`.

Furthermore, all conexp-clj functions expect Clojure data structres as their input, and
thus so do the corresponding static methods.
