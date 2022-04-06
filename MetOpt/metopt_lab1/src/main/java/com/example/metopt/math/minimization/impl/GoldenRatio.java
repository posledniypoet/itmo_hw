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
 * @author Danil Demintsev (demintsievd@yandex.ru) on 27.02.2021
 */

public class GoldenRatio implements MinimizationMethod {

    private static final double SQRT_5 = 2.236067977499;

    @Override
    public MinimizationResult minimize(Expression fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        Calculator calc = new Calculator(fun);
        double start_l = l;
        double start_r = r;
        List<SegmentIterationInfo> points = new ArrayList<>();
        int iteration = 0;
        double x1 = l + (3 - SQRT_5) * (r - l) / 2;
        double x2 = l + (SQRT_5 - 1) * (r - l) / 2;
        double f1 = calc.calc(x1).y;
        double f2 = calc.calc(x2).y;
        while (((r - l) * Math.pow((SQRT_5 - 1) / 2, iteration)) > inaccuracy) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
            points.add(new SegmentIterationInfo(l, r, calc.calc(x1), calc.calc(x2), null, null, iteration));
            if (f1 < f2) {
                r = x2;
                x2 = x1;
                f2 = f1;
                x1 = r - (r - l) * (SQRT_5 - 1) / 2;
                f1 = calc.calc(x1).y;
            } else {
                l = x1;
                x1 = x2;
                f1 = f2;
                x2 = l + (r - l) * (SQRT_5 - 1) / 2;
                f2 = calc.calc(x2).y;
            }
            ++iteration;
        }
        Point min = calc.calc((r + l) / 2);
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
