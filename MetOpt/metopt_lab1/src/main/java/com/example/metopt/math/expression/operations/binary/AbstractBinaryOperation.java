package com.example.metopt.math.expression.operations.binary;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.VariableName;

import java.util.EnumMap;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public abstract class AbstractBinaryOperation implements Expression {

    private final Expression left;
    private final Expression right;

    public AbstractBinaryOperation(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    protected abstract double calc(double left, double right);

    protected abstract String token();

    @Override
    public double evaluate(EnumMap<VariableName, Double> vars) {
        return calc(left.evaluate(vars), right.evaluate(vars));
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " " + token() + " " + right.toString() + ")";
    }
}
