package vizualization;


import syntacticalAnalyzer.Parser;
import syntacticalAnalyzer.Tree;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;

public class GraphVizualiser {
    private static int index = 0;

    public static void createDotGraph(String s, Parser parser, String name) throws ParseException {
        Tree ans = parser.parse(
                new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))
        );
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(name + ".dot")))) {
            writer.write(visualize(ans));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static String visualize(Tree t) {
        StringBuilder sb = new StringBuilder("digraph {");
        dfs(t, -1, 0, sb);
        sb.append("}");
        return sb.toString();
    }

    private static void dfs(Tree t, int prevId, int thisId, StringBuilder sb) {
        if (!t.children.isEmpty()) {
            sb.append(String.format("\t%d [label = \"%s\"]\n", thisId, t.node));

            if (prevId != -1) {
                sb.append(String.format("\t%d -> %d\n", prevId, thisId));
            }
            for (Tree ch : t.children) {
                dfs(ch, thisId, ++index, sb);
            }
        } else {
            sb.append(String.format("\t%d [label = \"%s\"]\n", thisId, t.node));
            sb.append(String.format("\t%s -> %d\n", prevId, thisId));
            index++;
        }
    }
}
