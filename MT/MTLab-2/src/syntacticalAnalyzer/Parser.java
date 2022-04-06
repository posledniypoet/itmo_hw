package syntacticalAnalyzer;

import lexicalAnalyzer.LexicalAnalyzer;
import lexicalAnalyzer.Token;

import java.io.InputStream;
import java.text.ParseException;

import static lexicalAnalyzer.Token.*;

/*
S -> lambda V:E
V -> letter V' | ε
V' -> , letter V' | ε
E -> FA | S
F -> NM
A -> + FA | ε
A -> - FA | ε
M -> * NM | ε
M -> / NM | ε
N -> letter | num | ( E )
 */
public class Parser {
    LexicalAnalyzer lex;

    public Tree parse(InputStream input) throws ParseException {
        lex = new LexicalAnalyzer(input);
        lex.nextToken();
        return S();
    }

    private void consume(Token token) throws ParseException {
        if (lex.curToken() != token) {
            throw new ParseException("Expected token" + token.toString() + "on the position: ", lex.curPos());
        }
        lex.nextToken();
    }

    private Tree S() throws ParseException {
        if (lex.curToken() == Token.LAMBDA) {
            lex.nextToken();
            Tree variables = V();
            consume(Token.COLON);
            Tree expression = E();
            return new Tree("S", new Tree("lambda"), variables, new Tree(":"), expression);
        } else {
            throw new ParseException("Expected token LAMBDA on position: ", lex.curPos());
        }
    }

    private Tree V() throws ParseException {
        switch (lex.curToken()) {
            case COLON -> {
                return new Tree("V");
            }
            case VARIABLE -> {
                lex.nextToken();
                return new Tree("V", new Tree("letter"), V2());
            }
            default -> {
                throw new ParseException("Unexpected token on position: ", lex.curPos());
            }
        }
    }

    private Tree V2() throws ParseException {
        switch (lex.curToken()) {
            case COMMA -> {
                lex.nextToken();
                lex.nextToken();
                return new Tree("V'", new Tree(","), V2());
            }
            case COLON -> {
                return new Tree("V'");
            }
            default -> throw new ParseException("Unexpected token on the position: ", lex.curPos());
        }
    }

    private Tree E() throws ParseException {
        switch (lex.curToken()) {
            case LAMBDA -> {
                return new Tree("E",S());
            }
        }
        return new Tree("E", F(), A());
    }

    private Tree F() throws ParseException {
        return new Tree("F", N(), M());
    }

    private Tree A() throws ParseException {
        switch (lex.curToken()) {
            case PLUS -> {
                lex.nextToken();
                return new Tree("A", new Tree("+"), F(), A());
            }
            case MINUS -> {
                lex.nextToken();
                return new Tree("A", new Tree("-"), F(), A());
            }
            default -> {
                return new Tree("A");
            }
        }
    }

    private Tree M() throws ParseException {
        switch (lex.curToken()) {
            case MULT -> {
                lex.nextToken();
                return new Tree("M", new Tree("*"), N(), M());
            }
            case DIV -> {
                lex.nextToken();
                return new Tree("M", new Tree("/"), N(), M());
            }
            default -> {
                return new Tree("M");
            }
        }
    }

    private Tree N() throws ParseException {
        switch (lex.curToken()) {
            case VARIABLE -> {
                lex.nextToken();
                return new Tree("N",new Tree("letter"));
            }
            case NUM -> {
                lex.nextToken();
                return new Tree("N", new Tree("number"));
            }
            case LPAREN -> {
                lex.nextToken();
                Tree expression = E();
                consume(Token.RPAREN);
                return new Tree("N", new Tree("("), expression, new Tree(")"));
            }
            default -> throw new ParseException("Unexpected token on the position: ", lex.curPos());
        }
    }
}
