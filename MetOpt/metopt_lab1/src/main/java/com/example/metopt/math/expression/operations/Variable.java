package com.example.metopt.math.expression.operations;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.exception.UndefinedVariableException;

import java.util.EnumMap;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class Variable implements Expression {

    private final VariableName name;

    public Variable(VariableName name) {
        this.name = name;
    }

    @Override
    public double evaluate(final EnumMap<VariableName, Double> vars) {
        Double value = vars.get(name);
        if (value == null) {
            throw new UndefinedVariableException("Variable " + name.name() + " is not defined");
        }
        return value;
    }

    @Override
    public String toString() {
        return name.token;
    }
}
