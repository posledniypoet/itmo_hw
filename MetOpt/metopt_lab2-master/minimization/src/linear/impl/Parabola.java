package linear.impl;

import linear.geometry.Point;
import linear.MinimizationMethod;
import linear.impl.utils.Calculator;

import java.util.function.Function;

import static linear.impl.utils.MinimizationUtils.MAX_ITERATION;
import static linear.impl.utils.MinimizationUtils.validate;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class Parabola implements MinimizationMethod {

    @Override
    public Point minimize(Function<Double, Double> fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        Calculator calc = new Calculator(fun);
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
            iteration++;
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
        return calc.calc(xResult);
    }
}
