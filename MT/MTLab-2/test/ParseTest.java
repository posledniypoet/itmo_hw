import lexicalAnalyzer.LexicalAnalyzer;
import lexicalAnalyzer.Token;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import syntacticalAnalyzer.Parser;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.*;

class ParseTest {
    public String lexString(String s) throws ParseException {
        InputStream stream = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8));
        LexicalAnalyzer lexer = new LexicalAnalyzer(stream);
        StringBuilder res = new StringBuilder();
        Token token;
        do {
            lexer.nextToken();
            token = lexer.curToken();
            res.append(token.toString()).append(" ");
        } while (token != Token.END);
        return res.toString();
    }

    private void parseString(String s) throws ParseException {
        InputStream stream = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8));
        new Parser().parse(stream);
    }


    @Test
    public void correctTests() {
        try {
            String s = "lambda n: n + 8";
            Assertions.assertEquals(lexString(s), "LAMBDA VARIABLE COLON VARIABLE PLUS NUM END ");
        } catch (ParseException e) {
            Assertions.fail();
        }

        try {
            String s = "lambda x, y, long_name : long_name * 5 (x + y)";
            Assertions.assertEquals(lexString(s),
                    "LAMBDA VARIABLE COMMA VARIABLE COMMA VARIABLE COLON VARIABLE MULT NUM LPAREN VARIABLE PLUS VARIABLE RPAREN END ");
        } catch (ParseException e) {
            Assertions.fail();
        }

        try {
            String s = "lambda x: x / x";
            Assertions.assertEquals(lexString(s), "LAMBDA VARIABLE COLON VARIABLE DIV VARIABLE END ");
        } catch (ParseException e) {
            Assertions.fail();
        }

        try {
            String s = "lambda x: 100 * 1488 * (88 - x) / 355 + x";
            Assertions.assertEquals(lexString(s),
                    "LAMBDA VARIABLE COLON NUM MULT NUM MULT LPAREN NUM MINUS VARIABLE RPAREN DIV NUM PLUS VARIABLE END ");
        } catch (ParseException e) {
            Assertions.fail();
        }

        try {
            String s = "lambda x: lambda y: x + y";
            Assertions.assertEquals(lexString(s), "LAMBDA VARIABLE COLON LAMBDA VARIABLE COLON VARIABLE PLUS VARIABLE END ");
        } catch (ParseException e) {
            Assertions.fail();
        }

        try {
            String s = "lambda x: lambda y: lambda z,w:x + y + 1666";
            Assertions.assertEquals(lexString(s), "LAMBDA VARIABLE COLON LAMBDA VARIABLE COLON LAMBDA VARIABLE COMMA VARIABLE COLON VARIABLE PLUS VARIABLE PLUS NUM END ");
        } catch (ParseException e) {
            Assertions.fail();
        }
    }


    @Test
    public void incorrectTests() {
        assertThrows(ParseException.class, () -> {
            String s = "x: 267 + 5";               // missing "lambda"
            parseString(s);
        });

        assertThrows(ParseException.class, () -> {
            String s = "lambda x n: 5 * 3";        // missing "," between variables
            parseString(s);
        });

        assertThrows(ParseException.class, () -> {
            String s = "lambba x : x + 5 + 2";     // incorrect word "lambda"
            parseString(s);
        });

        assertThrows(ParseException.class, () -> {
            String s = "lambda x, y: 55 + (6 - y"; // missing ")"
            parseString(s);
        });

        assertThrows(ParseException.class, () -> {
            String s = "lambda x: x *+ 48";      // two operations at the same time
            parseString(s);
        });

        assertThrows(ParseException.class, () -> {
            String s = "lambda x: x + 48 + lambda y: y + 1";  // incorrect place of lambda
            parseString(s);
        });

        assertThrows(ParseException.class, () -> {
            String s = "lambda x , lambda y: x + 48";  //incorrect place of comma
            parseString(s);
        });

    }
}