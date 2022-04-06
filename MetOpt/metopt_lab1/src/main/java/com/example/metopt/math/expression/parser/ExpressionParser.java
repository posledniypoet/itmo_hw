package com.example.metopt.math.expression.parser;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.Constant;
import com.example.metopt.math.expression.operations.Variable;
import com.example.metopt.math.expression.operations.VariableName;
import com.example.metopt.math.expression.parser.exception.ExpressionParserException;

import java.util.List;

import static com.example.metopt.math.expression.parser.BinaryOperation.*;
import static com.example.metopt.math.expression.parser.UnaryOperation.*;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class ExpressionParser {

    private static final List<List<BinaryOperation>> BIN_OP_PRIORITY = List.of(
            List.of(ADD, SUB),
            List.of(MUL, DIV),
            List.of(POW)
    );

    private static final List<UnaryOperation> UN_OP = List.of(
            MINUS,
            SINES,
            COSINE
    );

    private final String expr;
    private int pos;
    private int balance;

    public ExpressionParser(String expr) {
        this.expr = expr.replaceAll("\\s+", "");
        this.pos = 0;
    }

    public static Expression parse(String str) {
        return new ExpressionParser(str).parse();
    }

    private Expression parse() {
        Expression result = parseBinary(0);
        if (!isEnd()) {
            throw new ExpressionParserException("Not everything has been parsed");
        }
        return result;
    }

    private Expression parseBinary(int priority) {
        if (priority >= BIN_OP_PRIORITY.size()) {
            return parseUnary();
        }
        Expression left = parseBinary(priority + 1);

        while (!isEnd()) {
            BinaryOperation operation = parseOperation(priority);
            if (operation == null) {
                return left;
            }
            Expression right = parseBinary(priority + 1);
            left = operation.constructor.apply(left, right);
        }

        return left;
    }

    private Expression parseUnary() {
        for (UnaryOperation operation : UN_OP) {
            if (test(operation.token)) {
                return operation.constructor.apply(parseUnary());
            }
        }
        return parseOperand();
    }

    private Expression parseOperand() {
        for (VariableName name : VariableName.values()) {
            if (test(name.token)) {
                return new Variable(name);
            }
        }
        if (test('(')) {
            return parseParenthesis();
        }
        int index = pos++;
        while ((pos < expr.length()) && (('0' <= expr.charAt(pos) && expr.charAt(pos) <= '9') || (expr.charAt(pos) == '.'))) {
            ++pos;
        }
        return new Constant(Double.parseDouble(expr.substring(index, pos)));
    }

    private Expression parseParenthesis() {
        Expression result = parseBinary(0);
        if (test(')')) {
            return result;
        }
        throw new ExpressionParserException("')' expected on pos " + pos + " but '" + expr.charAt(pos) + "' found");
    }

    private BinaryOperation parseOperation(int priority) {
        for (var operation : BIN_OP_PRIORITY.get(priority)) {
            if (test(operation.token)) {
                return operation;
            }
        }
        return null;
    }

    private boolean test(String str) {
        if (pos < expr.length() && expr.startsWith(str, pos)) {
            pos += str.length();
            return true;
        }
        return false;
    }

    private boolean test(char c) {
        if (expr.charAt(pos) == c) {
            ++pos;
            return true;
        }
        return false;
    }

    private boolean isEnd() {
        return (pos == expr.length());
    }
}
