
package org.latdraw.partition;

import org.latdraw.orderedset.*;
import java.util.*;
import java.io.*;

public class ReadPartitions {

  public static OrderedSet readPartitionsFile(File file, final int delta)
                               throws IOException, NonOrderedSetException {
    List<Partition> lst = readPartitions(file, delta);
    // if (lst.size() == 0) throw an error ?? 
    final int n = lst.get(0).size();
    final Partition one = BasicPartition.one(n, true);
    final Partition zero = BasicPartition.zero(n, true);
    if (!lst.contains(one)) lst.add(one);
    if (!lst.contains(zero)) lst.add(0, zero);
    List<List<Partition>> filters = new ArrayList<List<Partition>>();
    for (Partition part : lst) {
      List<Partition> filter = new ArrayList<Partition>();
      for (Partition part2 : lst) {
        if (part.leq(part2)) filter.add(part2);
      }
      filters.add(filter);
    }
    return OrderedSet.orderedSetFromFilters(null, lst, filters);
  }

  public static List<Partition> readPartitions(File file, final int delta) 
                                                   throws IOException {
    List<List<List<Integer>>> parts = new ArrayList<List<List<Integer>>>();
    int max = 0;
    BufferedReader in = new BufferedReader(new FileReader(file));
    for (String line = in.readLine(); line != null; line = in.readLine()) {
      line = line.trim();
      if (line.startsWith("{") && line.endsWith("}")) {
        line = line.substring(1,line.length() - 1);
        line = line.trim();
        line = line.substring(1); // delete the first left curly brace
        String[] blocks = line.split("\\{");
        List<List<Integer>> part = new ArrayList<List<Integer>>();
        for (int i = 0; i < blocks.length; i++) {
          String par = blocks[i];
          int index = par.indexOf("}");
          if (index < 0) continue;
          par = par.substring(0, index);
          String[] block = par.split(",");
          List<Integer> blk = new ArrayList<Integer>();
          for (int j = 0; j < block.length; j++) {
            int u = Integer.parseInt(block[j]);
            max = Math.max(max, u);
            blk.add(new Integer(u));
          }
          part.add(blk);
        }
        parts.add(part);
      }
    }
    List<Partition> partitions = new ArrayList<Partition>();
    for (List<List<Integer>> part : parts) {
      int[] arr = new int[max + 1 - delta]; //the partition might be on 1,...,n
      for (List<Integer> blk : part) {
        final int blkSize = blk.size();
        final int root = blk.get(0) - delta; // delta = 1 for 1-based
        arr[root] = - blkSize;
        for (int i = 1; i < blkSize; i++)  {
          arr[blk.get(i) - delta] = root;
        }
      }
      partitions.add(new BasicPartition(arr, true));
    }
    return partitions;
  }

  public static void main(String[] args) {
    try {
      List<Partition> lst = readPartitions(new File("/tmp/f1.txt"), 1);
      for (Partition part : lst) {
        System.out.println(part.toString(Partition.ONE_INDEXED));
      }
    }
    catch (IOException e) { e.printStackTrace(); }
    try {
      OrderedSet foo = readPartitionsFile(new File("/tmp/f1.txt"), 1);
      System.out.println("foo size = " + foo.card());
    }
    catch (Exception e) { e.printStackTrace(); }
  }

}
