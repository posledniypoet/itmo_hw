package com.example.metopt.math.minimization.impl;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.MinimizationMethod;
import com.example.metopt.math.minimization.entity.MinimizationResult;
import com.example.metopt.math.minimization.entity.SegmentIterationInfo;
import com.example.metopt.math.minimization.impl.utils.Calculator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.MAX_ITERATION;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.createFunction;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.fillPointToDraw;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.validate;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class Dichotomy implements MinimizationMethod {

    @Override
    public MinimizationResult minimize(Expression fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        double start_l = l;
        double start_r = r;
        List<SegmentIterationInfo> points = new ArrayList<>();
        int iteration = 0;
        Calculator calc = new Calculator(fun);
        while ((r - l) / 2 > inaccuracy) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
            double x1 = (r + l - inaccuracy) / 2;
            double x2 = (r + l + inaccuracy) / 2;
            points.add(new SegmentIterationInfo(l, r, calc.calc(x1), calc.calc(x2), null, null, iteration++));
            if (calc.calc(x1).y < calc.calc(x2).y) {
                r = x2;
            } else {
                l = x1;
            }
        }
        Point min = calc.calc((l + r) / 2);
        double minY = min.y;
        double maxY = Math.max(calc.calc(start_l).y, calc.calc(start_r).y);
        return new MinimizationResult(
                min,
                createFunction(fun, start_l, start_r),
                calc.countedPoints(),
                fillPointToDraw(points, minY, maxY),
                Collections.emptyList(),
                Collections.emptyList()
        );
    }
}
