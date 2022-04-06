package com.example.metopt.math.expression.operations.unary;

import com.example.metopt.math.expression.Expression;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class Minus extends AbstractUnaryOperation {

    public Minus(Expression expr) {
        super(expr);
    }

    @Override
    protected double calc(double expr) {
        return -expr;
    }

    @Override
    protected String token() {
        return "-";
    }
}
