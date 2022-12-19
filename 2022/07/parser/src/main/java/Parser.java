import static java.lang.String.format;
import static java.util.Collections.emptySet;
import static java.util.Optional.empty;
import static java.util.stream.Collectors.toSet;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Parser {
  private static final Pattern CD_REGEX = Pattern.compile("\\$ cd (.+)");
  private static final Pattern LS_REGEX = Pattern.compile("\\$ ls");
  private static final Pattern FILE_REGEX = Pattern.compile("([0-9]+|dir) ([a-z\\.]+)");

  private static final Map<Pattern, Function<Matcher, Token>> readers = Map.of(
      CD_REGEX, Parser::readLineCd,
      LS_REGEX, Parser::readLineLs,
      FILE_REGEX, Parser::readLineFile
  );

  private static Node root = new Dir("/", emptySet(), empty());

  private static abstract class Node {
    public Optional<Dir> parent;
    public String name;
    protected Node(String name, Optional<Dir> parent) {
      this.parent = parent;
      this.name = name;
    }

    public void addParent(Dir parent) {
      if (this.parent.isEmpty() || parent.equals(this.parent.get())) {
        this.parent = Optional.of(parent);
      } else {
        throw new IllegalStateException("Parent already exists.");
      }
    }
  }

  private static class Dir extends Node {
    public final Set<Node> children;
    private Dir(String name, Set<Node> children, Optional<Dir> parent) {
      super(name, parent);
      this.children = children;
    }

    private void addChild(Node child) {
      children.add(child);
    }

    @Override
    public String toString() {
      return "Dir{" +
          "children=" + children +
          ", name='" + super.name + '\'' +
          '}';
    }
  }

  private static class File extends Node {
    public final long size;

    public File(String name, long size, Optional<Dir> parent) {
      super(name, parent);
      this.size = size;
    }

    @Override
    public String toString() {
      return "File{" +
          "size=" + size +
          ", name='" + super.name + '\'' +
          '}';
    }
  }

  private interface Token {}
  private record CToken(String cdTarget) implements Token {}
  private record LToken() implements Token {}
  private record NToken(Node n) implements Token {}

  public static void main(String[] args) throws Exception {
    Scanner sc = new Scanner(new FileInputStream("/home/jdl/repos/advent/2022/07/input"));
    List<Token> tokens = new ArrayList<>();
    while (sc.hasNextLine()) {
      tokens.add(readLine(sc.nextLine()));
    }
    Dir root = new Dir("/", new HashSet<>(), empty());
    compileTree(tokens, root, root);
    printTree(root, 0);
    System.out.println(size(root));
    Set<Dir> matchingDirs = new HashSet<>();
    findDirectoriesSizeBelow(root, 100000, matchingDirs);
    System.out.println(matchingDirs.stream().map(n -> n.name).collect(toSet()));
    System.out.println(matchingDirs.stream().mapToLong(Parser::size).sum());
    long unused = 70000000 - size(root);
    System.out.println(unused);
    long deleteNeeded = 30000000 - unused;
    Dir deleteCandidate = findDeleteCandidate(deleteNeeded, root, root);
    System.out.println(size(deleteCandidate));
  }

  public static Dir findDeleteCandidate(long targetSize, Dir currentCandidate, Dir wd) {
    long size = size(wd);
    Dir candidate;
    if (size > targetSize && size < size(currentCandidate)) {
      candidate = wd;
    } else {
      candidate = currentCandidate;
    }
    Optional<Dir> lowerCandidate = wd.children.stream()
        .filter(n -> n instanceof Dir)
        .map(c -> findDeleteCandidate(targetSize, candidate, (Dir) c))
        .sorted(Comparator.comparing(d -> size(d)))
        .findFirst();
    return lowerCandidate.map(c -> size(c) < size(candidate) ? c : candidate).orElse(candidate);
  }

  public static void printTree(Node node, int depth) {
    for (int i=0; i<depth; i++) {
      System.out.print("\t");
    }
    System.out.print("- ");
    if (node instanceof Dir) {
      System.out.println(node.name);
      ((Dir) node).children.stream()
          .sorted(Comparator.comparing(n -> n.name))
          .forEachOrdered(n -> printTree(n, depth+1));
    } else if (node instanceof File) {
      System.out.println(format("%-8s %-8d", node.name, ((File) node).size));
    }
  }

  public static void findDirectoriesSizeBelow(Node n, long size, Set<Dir> acc) {
    if (!(n instanceof Dir)) {
      return;
    }
    long s = size(n);
    if (s < size) {
      acc.add((Dir) n);
    }
    ((Dir) n).children.stream()
        .forEach(c -> findDirectoriesSizeBelow(c, size, acc));
  }

  public static long size(Node n) {
    if (n instanceof File) {
      return ((File) n).size;
    } else if (n instanceof Dir) {
      return ((Dir) n).children.stream()
          .mapToLong(Parser::size)
          .sum();
    } else {
      throw new IllegalStateException("Unknown node type: " + n.getClass());
    }
  }

  public static Token readLine(String s) {
    return readers.entrySet().stream()
        .map(e -> readLineWith(e.getKey(), s, e.getValue()))
        .filter(Optional::isPresent)
        .map(Optional::get)
        .findFirst().orElseThrow(() -> new IllegalStateException("No match found: " + s));
  }

  public static Optional<Token> readLineWith(Pattern p, String s, Function<Matcher, Token> readF) {
    Matcher m = p.matcher(s);
    return m.matches() ?
              Optional.of(readF.apply(m)) :
              empty();
  }

  public static Token readLineCd(Matcher m) {
      return new CToken(m.group(1));
  }

  public static Token readLineLs(Matcher m) {
    return new LToken();
  }

  public static Token readLineFile(Matcher m) {
    return new NToken(m.group(1).equals("dir") ?
               m.group(2).equals("/") ?
                   root :
                   new Dir(m.group(2), new HashSet<>(), empty()) :
               new File(m.group(2), Integer.parseInt(m.group(1)),empty()));
  }

  public static Node compileTree(List<Token> tokens, Dir wd, Dir root) {
    if (tokens.isEmpty()) {
      return wd;
    }
    Token t = tokens.get(0);
    Dir newWd = t instanceof CToken ?
                  compileLine((CToken) t, wd, root) :
                  t instanceof NToken ?
                      compileLine((NToken) t, wd, root) :
                      compileLine((LToken) t, wd, root);
    return compileTree(tokens.subList(1, tokens.size()), newWd, root);
  }

  public static Dir compileLine(CToken c, Dir wd, Dir root) {
    switch (c.cdTarget) {
      case "..":
        return wd.parent.get();
      case "/":
        return root;
      default:
        return wd.children.stream()
            .filter(d -> d.name.equals(c.cdTarget))
            .map(n -> (Dir) n)
            .findAny()
            .orElseThrow(() -> new IllegalStateException("CD target missing: " + c.cdTarget));
    }
  }

  public static Dir compileLine(LToken l, Dir wd, Dir root) {
    return wd;
  }

  public static Dir compileLine(NToken n, Dir wd, Dir root) {
    n.n.addParent(wd);
    wd.addChild(n.n);
    return wd;
  }
}
