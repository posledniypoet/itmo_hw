package com.example.metopt.math.minimization.impl.utils;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.binary.Addition;
import com.example.metopt.math.expression.operations.Constant;
import com.example.metopt.math.expression.operations.binary.Exponentiation;
import com.example.metopt.math.expression.operations.binary.Multiplication;
import com.example.metopt.math.expression.operations.Variable;
import com.example.metopt.math.expression.operations.VariableName;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.entity.Pair;
import com.example.metopt.math.minimization.entity.SegmentIterationInfo;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class MinimizationUtils {

    public static final int MAX_ITERATION = 1000;

    private static Point calc(double x, Expression fun) {
        var vars = new EnumMap<VariableName, Double>(VariableName.class);
        vars.put(VariableName.X, x);
        double f = fun.evaluate(vars);
        return new Point(x, f);
    }

    public static void validate(Expression fun, double l, double r, double inaccuracy) {
        if (fun == null) {
            throw new NullPointerException();
        }
        if (l >= r) {
            throw new IllegalArgumentException();
        }
        if (inaccuracy <= 0) {
            throw new IllegalArgumentException();
        }
    }

    public static List<Point> createFunction(Expression fun, double l, double r) {
        List<Point> list = new ArrayList<>();
        double dx = (r - l) / 1000;
        for (double x = l; x <= r; x += dx) {
            list.add(calc(x, fun));
        }
        return list;
    }

    public static List<SegmentIterationInfo> fillPointToDraw(List<SegmentIterationInfo> list, double minY_, double maxY_) {
        double len = (maxY_ - minY_) / 5;
        double maxY = maxY_ + len;
        double minY = minY_ - len;
        return list.stream()
                .map(__ -> new SegmentIterationInfo(
                        __.getL(),
                        __.getR(),
                        __.getX1Point(),
                        __.getX2Point(),
                        new Pair(new Point(__.getX1Point().x, minY), new Point(__.getX1Point().x, maxY), __.getX1Point()),
                        new Pair(new Point(__.getX2Point().x, minY), new Point(__.getX2Point().x, maxY), __.getX2Point()),
                        __.getIndex()
                )).collect(Collectors.toList());
    }

    public static Expression createParabola(double x1, double x2, double x3, double y1, double y2, double y3) {
        double denom = (x1 - x2) * (x1 - x3) * (x2 - x3);
        double A = (x3 * (y2 - y1) + x2 * (y1 - y3) + x1 * (y3 - y2)) / denom;
        double B = (x3 * x3 * (y1 - y2) + x2 * x2 * (y3 - y1) + x1 * x1 * (y2 - y3)) / denom;
        double C = (x2 * x3 * (x2 - x3) * y1 + x3 * x1 * (x3 - x1) * y2 + x1 * x2 * (x1 - x2) * y3) / denom;
        return new Addition(
                new Multiplication(
                        new Constant(A),
                        new Exponentiation(new Variable(VariableName.X), new Constant(2.))
                ),
                new Addition(
                        new Multiplication(
                                new Constant(B),
                                new Variable(VariableName.X)
                        ),
                        new Constant(C)
                )
        );
    }
}
