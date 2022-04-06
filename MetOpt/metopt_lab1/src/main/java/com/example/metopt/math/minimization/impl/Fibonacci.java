package com.example.metopt.math.minimization.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.MinimizationMethod;
import com.example.metopt.math.minimization.entity.MinimizationResult;
import com.example.metopt.math.minimization.entity.SegmentIterationInfo;
import com.example.metopt.math.minimization.impl.utils.Calculator;

import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.MAX_ITERATION;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.createFunction;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.fillPointToDraw;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.validate;

public class Fibonacci implements MinimizationMethod {

    @Override
    public MinimizationResult minimize(Expression fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        Calculator calc = new Calculator(fun);
        double start_l = l;
        double start_r = r;
        List<SegmentIterationInfo> points = new ArrayList<>();
        double interval = 2 * inaccuracy;
        int n = find_n((r - l) / interval);
        double[] a = new double[n + 1];
        double[] b = new double[n + 1];
        double[] lda = new double[n + 1];
        double[] tta = new double[n + 1];
        a[1] = -0.5;
        b[1] = 0.5;
        lda[1] = a[1] + (fib(n - 2) / fib(n)) * (b[1] - a[1]);
        tta[1] = a[1] + (fib(n - 1) / fib(n)) * (b[1] - a[1]);
        int k = 1;
        int iteration = 0;
        while (k != (n - 1)) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
            if (calc.calc(lda[k]).y > calc.calc(tta[k]).y) {
                a[k + 1] = lda[k];
                b[k + 1] = b[k];
                lda[k + 1] = tta[k];
                tta[k + 1] = a[k + 1] + (fib(n - k - 1) / fib(n - k)) * (b[k + 1] - a[k + 1]);
                if (k == (n - 2)) {
                    lda[n] = lda[n - 1];
                    tta[n] = lda[n] + inaccuracy;
                    if (calc.calc(lda[n]).y == calc.calc(tta[n]).y) {
                        a[n] = lda[n];
                        b[n] = b[n - 1];
                    } else {
                        a[n] = a[n - 1];
                        b[n] = tta[n];
                    }
                    break;
                } else {
                    // todo: iteration values
                    points.add(new SegmentIterationInfo(
                            a[k],
                            b[k],
                            calc.calc(lda[k]),
                            calc.calc(tta[k]),
                            null,
                            null,
                            iteration++));
                    k++;
                }
            } else {
                a[k + 1] = a[k];
                b[k + 1] = tta[k];
                tta[k + 1] = lda[k];
                lda[k + 1] = a[k + 1] + (fib(n - k - 2) / fib(n - k)) * (b[k + 1] - a[k + 1]);
                if (k == (n - 2)) {
                    lda[n] = lda[n - 1];
                    tta[n] = lda[n] + inaccuracy;
                    if (calc.calc(lda[n]).y == calc.calc(tta[n]).y) {
                        a[n] = lda[n];
                        b[n] = b[n - 1];
                    } else {
                        a[n] = a[n - 1];
                        b[n] = tta[n];
                    }
                    break;
                } else {
                    // todo: iteration values
                    points.add(new SegmentIterationInfo(
                            a[k],
                            b[k],
                            calc.calc(lda[k]),
                            calc.calc(tta[k]),
                            null,
                            null,
                            iteration++));
                    k++;
                }
            }
        }
        Point min = calc.calc((a[n] + b[n]) / 2);
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

    private double fib(int n) {
        return (Math.pow((1 + Math.sqrt(5)) / 2, n) - Math.pow((1 - Math.sqrt(5)) / 2, n));
    }

    private int find_n(double k) {
        int n = 0;
        while (fib(n) < k) {
            n++;
        }
        return n;
    }
}
