import haskell.HaskellLexer;
import haskell.HaskellParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class JavaTextGenerator {
    public static void translate(Path pathIn, Path pathOut) throws IOException {

        Lexer lex = new HaskellLexer(CharStreams.fromPath(pathIn));
        CommonTokenStream tokens = new CommonTokenStream(lex);
        HaskellParser parser = new HaskellParser(tokens);

        String translation = new HaskellToCConverter().visitFile(parser.file());

        var file = pathOut.getFileName().toString();
        int len = file.length();
        file = file.substring(0, len - 5);

        try (BufferedWriter br = Files.newBufferedWriter(pathOut)) {
            br.write("public class " + file + "{\n");
            br.write(HaskellToCConverter.shiftString(translation));
            br.write("\n}\n");
        }

    }

}
