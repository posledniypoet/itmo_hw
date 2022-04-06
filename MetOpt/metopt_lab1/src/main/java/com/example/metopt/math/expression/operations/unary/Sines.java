package com.example.metopt.math.expression.operations.unary;

import com.example.metopt.math.expression.Expression;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 13.04.2021
 */
public class Sines extends AbstractUnaryOperation {

    public Sines(Expression expr) {
        super(expr);
    }

    @Override
    protected double calc(double expr) {
        return Math.sin(expr);
    }

    @Override
    protected String token() {
        return "sin";
    }
}
