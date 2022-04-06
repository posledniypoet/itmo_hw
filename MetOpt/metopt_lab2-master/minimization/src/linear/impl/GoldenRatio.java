package linear.impl;

import linear.geometry.Point;
import linear.MinimizationMethod;
import linear.impl.utils.Calculator;

import java.util.function.Function;

import static linear.impl.utils.MinimizationUtils.MAX_ITERATION;
import static linear.impl.utils.MinimizationUtils.validate;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 27.02.2021
 */

public class GoldenRatio implements MinimizationMethod {

    private static final double SQRT_5 = 2.236067977499;

    @Override
    public Point minimize(Function<Double, Double> fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        Calculator calc = new Calculator(fun);
        int iteration = 0;
        double x1 = l + (3 - SQRT_5) * (r - l) / 2;
        double x2 = l + (SQRT_5 - 1) * (r - l) / 2;
        double f1 = calc.calc(x1).y;
        double f2 = calc.calc(x2).y;
        while (((r - l) * Math.pow((SQRT_5 - 1) / 2, iteration)) > inaccuracy) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
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
        return calc.calc((r + l) / 2);
    }
}
