package lexicalAnalyzer;

import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;

public class LexicalAnalyzer {
    InputStream is;
    int curChar;
    int curPos;
    Token curToken;

    public LexicalAnalyzer(InputStream is) throws ParseException {
        this.is = is;
        curPos = 0;
        nextChar();
    }

    private boolean isBlank(int c) {
        return c == ' ' || c == '\r' || c == '\n' || c == '\t';
    }

    private void nextChar() throws ParseException {
        curPos++;
        try {
            curChar = is.read();
        } catch (IOException e) {
            throw new ParseException(e.getMessage(), curPos);
        }
    }

    public void nextToken() throws ParseException {
        while (isBlank(curChar)) {
            nextChar();
        }

        if (curChar == '-') {
            nextChar();
            if (Character.isDigit(curChar)) {
                curToken = Token.NUM;
                while (Character.isDigit(curChar)) {
                    nextChar();
                }
                return;
            }
            curToken = Token.MINUS;
            return;
        } else {
            if (Character.isDigit(curChar)) {
                curToken = Token.NUM;
                while (Character.isDigit(curChar)) {
                    nextChar();
                }
                return;
            }
        }

        switch (curChar) {
            case 'l' -> {
                nextChar();
                if (curChar == 'a') {
                    nextChar();
                    if (curChar == 'm') {
                        nextChar();
                        if (curChar == 'b') {
                            nextChar();
                            if (curChar == 'd') {
                                nextChar();
                                if (curChar == 'a') {
                                    nextChar();
                                    if (isBlank(curChar)) {
                                        curToken = Token.LAMBDA;
                                        nextChar();
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
                while (Character.isLetterOrDigit(curChar) || curChar == '_') {
                    nextChar();
                }
                curToken = Token.VARIABLE;
                nextChar();
            }
            case ':' -> {
                curToken = Token.COLON;
                nextChar();
            }
            case ',' -> {
                curToken = Token.COMMA;
                nextChar();
            }
            case '(' -> {
                curToken = Token.LPAREN;
                nextChar();
            }
            case ')' -> {
                curToken = Token.RPAREN;
                nextChar();
            }
            case '+' -> {
                curToken = Token.PLUS;
                nextChar();
            }
            case '-' -> {
                curToken = Token.MINUS;
                nextChar();
            }
            case '*' -> {
                curToken = Token.MULT;
                nextChar();
            }
            case '/' -> {
                curToken = Token.DIV;
                nextChar();
            }
            case -1 -> {
                curToken = Token.END;
                nextChar();
            }
            default -> curToken = parseWord();
        }
    }

    private Token parseWord() throws ParseException {
        if (Character.isLetterOrDigit(curChar)) {
            while (Character.isLetterOrDigit(curChar) || curChar == '_') {
                nextChar();
            }
            return Token.VARIABLE;
        } else {
            return parseNumber();
        }
    }

    private Token parseNumber() throws ParseException {
        if (Character.isDigit(curChar)) {
            StringBuilder sb = new StringBuilder();
            sb.append(curChar);
            nextChar();
            while (Character.isDigit(curChar)) {
                sb.append(curChar);
                nextChar();
            }
            String res = sb.toString();
            try {
                Integer.parseInt(res);
            } catch (NumberFormatException e) {
                throw new ParseException("Failed during reading number " + res, curPos);
            }
            return Token.NUM;
        } else {
            throw new ParseException("Illegal character" + (char) curChar, curPos);
        }
    }


    public Token curToken() {
        return curToken;
    }

    public int curPos() {
        return curPos;
    }
}
