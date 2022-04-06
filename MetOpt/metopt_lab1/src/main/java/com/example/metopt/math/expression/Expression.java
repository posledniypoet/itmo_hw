package com.example.metopt.math.expression;

import com.example.metopt.math.expression.operations.VariableName;

import java.util.EnumMap;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public interface Expression {

    double evaluate(EnumMap<VariableName, Double> vars);
}
