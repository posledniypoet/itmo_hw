
import syntacticalAnalyzer.Parser;


import java.text.ParseException;

import static vizualization.GraphVizualiser.createDotGraph;

public class Main {
    public static void main(String[] args) throws ParseException {
        Parser parser = new Parser();
        String expr = "lambda x: lambda y: lambda z,w:x + y + 1666";
        createDotGraph(expr, parser, "dotsource");
    }
}
