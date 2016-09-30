/*
** $Id: InputLattice.java,v 1.4 2005/05/09 07:56:57 ralph Exp $
*/
package org.latdraw.orderedset;
//InputLattice.java  


import java.util.*;
import java.io.*;
import java.net.*;

/**
 * A class for inputting a lattice (or ordered set) from a file, string
 * or network connection.
 * <p>
 * @author Ralph Freese
 * @version $Id: InputLattice.java,v 1.4 2005/05/09 07:56:57 ralph Exp $
 */
public class InputLattice implements Serializable {

  public static final int FILE = 0, STRING = 1, URL = 2;
  //public FileInputStream latFileStream;
  //public InputStream latFileStream;
  //public StringBufferInputStream stringStream;
  //public InputStream stringStream;
  public List labels;
  public List upperCoversList;
  public HashMap edgeColors;

  // rsf: string conversion stuff
  public static final String joinStr = "join";
  public static final String meetStr = "meet";
  public static final String joinSign = "\u2228";
  public static final String meetSign = "\u2227";

  boolean convertJoinMeet = true;


  // AS: a string after the first opening brace will be interpreted as a name.
  public String name; 


  // need this for deserialization due to presence of InputStream
  protected InputLattice() {
    super();
  }

  public InputLattice(String file) throws FileNotFoundException,IOException {
    this(file, true);
  }

  public InputLattice(String file, int type)
      throws FileNotFoundException,IOException {
    this(file, type, true);
  }


  public InputLattice(String file, int type, boolean convertJoinMeet) 
                                 throws FileNotFoundException,IOException {
    //InputStream latStream = null;
    Reader latStream = null;
    labels = new ArrayList();
    upperCoversList = new ArrayList();
    List upperCovers = new ArrayList();
    edgeColors = new HashMap();
    List upperCovering = new ArrayList(); // to put on the Hastable
    int level = 0;
    String str; 
    String currentElt = null; 
    String cover = null;
    boolean first = true;
    StreamTokenizer in;
    switch (type) 
    {
      case FILE:
        latStream = new FileReader(file);
        break;
      case STRING:
        //latStream = new StringBufferInputStream(file);
        latStream = new StringReader(file);
        break;
      case URL:
        String request = "http://www.math.hawaii.edu/loadURL.html?location="
                + file;
        URL url = new URL(request);
        URLConnection connection = url.openConnection();
        //latStream = new DataInputStream(connection.getInputStream());
        latStream = new InputStreamReader(connection.getInputStream());
        break;
    }
    in = new StreamTokenizer(new BufferedReader(latStream));
    // AS: these characters are added to the default constituents of TT_WORD
    in.wordChars('^','_');
    in.wordChars('*','.');     //* + , - .
in.wordChars(']',']');
in.wordChars('[','[');
in.wordChars(':',':');
in.wordChars('<','<');
in.wordChars('>','>');
char uuu = "'".charAt(0);
in.wordChars(uuu, uuu);
    while (in.nextToken() != StreamTokenizer.TT_EOF) {
      if (in.ttype == StreamTokenizer.TT_WORD ||
        in.ttype == StreamTokenizer.TT_NUMBER || in.ttype == '"') {
        if (in.ttype == StreamTokenizer.TT_NUMBER) {
          str = "" + Math.round(in.nval);
        }
        else {
          str = in.sval;
        }
        if (convertJoinMeet && level > 1) {
          str = stringSubstitute(str, joinStr, joinSign);
          str = stringSubstitute(str, meetStr, meetSign);
        }
        //AS: begin code to read the diagram's name
        if (level == 1) {
          name = new String(str);
          //System.out.println("AS: InputLattice: setting name to "+name+"\n");
        }
        if (level == 2) {
          labels.add(str);
          currentElt = str;
        }
        if (level == 3) {
          // only 2 or 3 or 4 should occur
          upperCovers.add(str);
        }
        if (level == 4) {
          // only 2 or 3 or 4 should occur
          if (first) {
            upperCovers.add(str);
            cover = str;
            first = false;
          }
          else {
            //List v = new ArrayList();
            //v.add(currentElt);
            //v.add(cover);
            //edgeColors.put(v, str);
            edgeColors.put(new Edge(currentElt, cover), str);
          }
        }
      }
      if (in.ttype == '(') {
        level++;
        if (level == 3) upperCovers = new ArrayList();
      }
      if (in.ttype == ')') {
        level--;
        if (level == 3) first = true;
        if (level == 2) upperCoversList.add(upperCovers);
        if (level == 0) {
          //System.out.println(labels);
          //System.out.println(upperCoversList);
          if (latStream != null) latStream.close();
          return;
        }
      }
    }
  }


  // When isFile is false file is actually a string with the data.

  public InputLattice(String file, boolean isFile) 
      throws FileNotFoundException,IOException {
    this(file, isFile ? FILE : STRING);
  }

  /** InputLattice from
  * String (name) 
  * List of Strings  (labels)
  * List of lists of strings (upper covers)
  */
  public InputLattice(String nm, List labelvec, List ucovers) {
    //ucovers is a list of lists of ints
    upperCoversList = new ArrayList();
    for (int i=0; i < ucovers.size(); i++) {
      upperCoversList.add(((ArrayList) ucovers.get(i)).clone());
    }
    name = nm;
    // labels is a list of strings.
    labels = new ArrayList();
    for (int i=0; i < labelvec.size(); i++) {
      labels.add(labelvec.get(i));
    }
    return;
  }

  /**
   * Global subsitution. Could be more efficient.
   */
  public String stringSubstitute(String str, String old, String rep) {
    int index = str.indexOf(old);
    if (index == -1) return str;
    StringBuffer sb = new StringBuffer(str.substring(0, index));
    sb.append(rep);
    sb.append(stringSubstitute(str.substring(index + old.length()), old, rep));
    return sb.toString();
  }


}


