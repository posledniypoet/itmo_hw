package com.example.metopt.math.expression.operations.unary;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.VariableName;

import java.util.EnumMap;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public abstract class AbstractUnaryOperation implements Expression {

    private final Expression expr;

    public AbstractUnaryOperation(Expression expr) {
        this.expr = expr;
    }

    protected abstract double calc(double expr);

    protected abstract String token();

    @Override
    public double evaluate(EnumMap<VariableName, Double> vars) {
        return calc(expr.evaluate(vars));
    }

    @Override
    public String toString() {
        return "(" + token() + " " + expr.toString() + ")";
    }
}
