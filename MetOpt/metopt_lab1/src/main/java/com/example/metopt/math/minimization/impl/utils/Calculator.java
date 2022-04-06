package com.example.metopt.math.minimization.impl.utils;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.VariableName;
import com.example.metopt.math.geometry.Point;

public class Calculator {

    private final Expression fun;
    private final Map<Double, Point> memory;
    private final EnumMap<VariableName, Double> vars;

    public Calculator(Expression fun) {
        this.fun = fun;
        this.memory = new HashMap<>();
        this.vars = new EnumMap<>(VariableName.class);
    }

    public Point calc(double x) {
        if (memory.containsKey(x)) {
            return memory.get(x);
        }
        vars.put(VariableName.X, x);
        Point point = new Point(x, fun.evaluate(vars));
        memory.put(x, point);
        return point;
    }

    public int countedPoints() {
        return memory.size();
    }
}
