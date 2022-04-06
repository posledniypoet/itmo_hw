package com.example.metopt.math.minimization;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.VariableName;
import com.example.metopt.math.minimization.entity.MinimizationResult;

import java.util.EnumMap;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public interface MinimizationMethod {

    MinimizationResult minimize(Expression fun, double l, double r, double inaccuracy);
}
