package com.example.metopt.math.minimization.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.MinimizationMethod;
import com.example.metopt.math.minimization.entity.MinimizationResult;
import com.example.metopt.math.minimization.entity.ParabolaIterationInfo;
import com.example.metopt.math.minimization.entity.SegmentIterationInfo;
import com.example.metopt.math.minimization.impl.utils.Calculator;

import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.MAX_ITERATION;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.createFunction;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.createParabola;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.fillPointToDraw;
import static com.example.metopt.math.minimization.impl.utils.MinimizationUtils.validate;

public class Brent implements MinimizationMethod {

    private static final double K = (3. - Math.sqrt(5)) / 2.;

    @Override
    public MinimizationResult minimize(Expression fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        Calculator calc = new Calculator(fun);
        double start_l = l;
        double start_r = r;
        List<SegmentIterationInfo> points = new ArrayList<>();
        List<ParabolaIterationInfo> parabolaInfos = new ArrayList<>();
        double x = (l + r) / 2.;
        double w = x;
        double v = x;
        double d = r - l;
        double e = r - l;
        double fx = calc.calc(x).y;
        double fv = calc.calc(v).y;
        double fw = calc.calc(w).y;
        int iteration = 0;
        while (true) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
            double g = e;
            e = d;
            double u = x;
            boolean success = false;
            if (diff(x, w, v, inaccuracy) && diff(fx, fw, fv, inaccuracy)) {
                u = parabola(new Point(v, fv), new Point(x, fx), new Point(w, fw));
                if ((l <= u && u <= r) && Math.abs(u - x) < g / 2) {
                    List<Double> sortedPoints =
                            List.of(x, v, w).stream().sorted().collect(Collectors.toList());
                    parabolaInfos.add(new ParabolaIterationInfo(
                            sortedPoints.get(0),
                            sortedPoints.get(1),
                            sortedPoints.get(2),
                            calc.calc(u),
                            createParabola(x, v, w, fx, fv, fw),
                            iteration++
                    ));
                    d = Math.abs(u - x);
                    success = true;
                }
            }
            if (!success) {
                if (x < (r + l) / 2) {
                    u = x + K * (r - x);
                    d = (r - x);
                } else {
                    u = x - K * (x - l);
                    d = x - l;
                }
                points.add(new SegmentIterationInfo(
                        l,
                        r,
                        calc.calc(Math.min(x, u)),
                        calc.calc(Math.max(x, u)),
                        null,
                        null,
                        iteration++));
            }
            if (Math.abs(r - l) < inaccuracy) {
                Point min = calc.calc(x);
                double minY = min.y;
                double maxY = Math.max(calc.calc(start_l).y, calc.calc(start_r).y);
                return new MinimizationResult(
                        min,
                        createFunction(fun, start_l, start_r),
                        calc.countedPoints(),
                        fillPointToDraw(points, minY, maxY),
                        parabolaInfos.stream()
                                .map(expr -> createFunction(expr.getParabola(), start_l, start_r))
                                .collect(Collectors.toList()),
                        parabolaInfos
                );
            }
            double fu = calc.calc(u).y;
            if (fu <= fx) {
                if (u >= x) {
                    l = x;
                } else {
                    r = x;
                }
                v = w;
                w = x;
                x = u;
                fv = fw;
                fw = fx;
                fx = fu;
            } else {
                if (u >= x) {
                    r = u;
                } else {
                    l = u;
                }
                if (fu <= fw || w == x) {
                    v = w;
                    w = u;
                    fv = fw;
                    fw = fu;
                } else if (fu <= fv || v == x || v == w) {
                    v = u;
                    fv = fu;
                }
            }
        }
    }

    double parabola(Point v, Point x, Point w) {
        var list = List.of(v, x, w).stream().sorted(Point.cmpByX).collect(Collectors.toList());
        Point p1 = list.get(0);
        Point p2 = list.get(1);
        Point p3 = list.get(2);
        double a1 = (p2.y - p1.y) / (p2.x - p1.x);
        double a2 = ((p3.y - p1.y) / (p3.x - p1.x) - a1) / (p3.x - p2.x);
        return (p1.x + p2.x - a1 / a2) / 2;
    }

    private boolean diff(double a, double b, double c, double eps) {
        return Math.abs(a - b) > eps && Math.abs(a - c) > eps && Math.abs(b - c) > eps;
    }
}
