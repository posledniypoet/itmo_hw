package com.example.metopt.math.minimization.impl;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.MinimizationMethod;
import com.example.metopt.math.minimization.entity.MinimizationResult;
import com.example.metopt.math.minimization.entity.ParabolaIterationInfo;
import com.example.metopt.math.minimization.impl.utils.Calculator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.MAX_ITERATION;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.createFunction;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.createParabola;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.validate;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class Parabola implements MinimizationMethod {

    @Override
    public MinimizationResult minimize(Expression fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        Calculator calc = new Calculator(fun);
        List<ParabolaIterationInfo> parabolaInfos = new ArrayList<>();
        double start_l = l;
        double start_r = r;
        double x1 = l;
        double x2 = (l + r) / 2;
        double x3 = r;
        double xResult = Double.NaN;
        int iteration = 0;
        Point p1 = calc.calc(x1);
        Point p2 = calc.calc(x2);
        Point p3 = calc.calc(x3);
        while (true) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
            double a1 = (p2.y - p1.y) / (x2 - x1);
            double a2 = ((p3.y - p1.y) / (x3 - x1) - a1) / (x3 - x2);
            double newXResult = (x1 + x2 - a1 / a2) / 2;
            List<Double> points = List.of(p1.x, p2.x, p3.x).stream().sorted().collect(Collectors.toList());
            parabolaInfos.add(new ParabolaIterationInfo(
                    points.get(0),
                    points.get(1),
                    points.get(2),
                    calc.calc(newXResult),
                    createParabola(p1.x, p2.x, p3.x, p1.y, p2.y, p3.y),
                    iteration++
            ));
            if (Math.abs(xResult - newXResult) < inaccuracy) {
                xResult = newXResult;
                break;
            }
            xResult = newXResult;
            if (x1 < xResult && xResult < x2) {
                if (calc.calc(xResult).y >= p2.y) {
                    x1 = xResult;
                    p1 = calc.calc(x1);
                } else {
                    x3 = x2;
                    x2 = xResult;
                    p3 = p2;
                    p2 = calc.calc(x2);
                }
            } else if (x1 > xResult || xResult > x2) {
                if (calc.calc(xResult).y <= p2.y) {
                    x1 = x2;
                    x2 = xResult;
                    p1 = p2;
                    p2 = calc.calc(x2);
                } else {
                    x3 = xResult;
                    p3 = calc.calc(x3);
                }
            } else {
                break;
            }
        }
        Point min = calc.calc(xResult);
        return new MinimizationResult(
                min,
                createFunction(fun, start_l, start_r),
                calc.countedPoints(),
                Collections.emptyList(),
                parabolaInfos.stream()
                        .map(expr -> createFunction(expr.getParabola(), start_l, start_r))
                        .collect(Collectors.toList()),
                parabolaInfos
        );
    }
}
