package com.example.metopt.math.expression.operations.binary;

import com.example.metopt.math.expression.Expression;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class Subtraction extends AbstractBinaryOperation {

    public Subtraction(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected double calc(double left, double right) {
        return left - right;
    }

    @Override
    protected String token() {
        return "-";
    }
}
