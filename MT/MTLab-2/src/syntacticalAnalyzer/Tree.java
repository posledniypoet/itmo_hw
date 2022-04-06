package syntacticalAnalyzer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Tree {
    public String node;
    public List<Tree> children = new ArrayList<>();

    public Tree(String Node) {
        this.node = Node;
    }

    public Tree(String Node, Tree... children) {
        this.node = Node;
        this.children = Arrays.asList(children);
    }
}
