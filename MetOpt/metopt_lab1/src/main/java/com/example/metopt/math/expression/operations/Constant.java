package com.example.metopt.math.expression.operations;

import com.example.metopt.math.expression.Expression;

import java.util.EnumMap;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class Constant implements Expression {

    private final double val;

    public Constant(double val) {
        this.val = val;
    }

    @Override
    public double evaluate(EnumMap<VariableName, Double> vars) {
        return val;
    }

    @Override
    public String toString() {
        return Double.toString(val);
    }
}
