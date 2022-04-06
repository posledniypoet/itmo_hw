package linear.impl;

import linear.geometry.Point;
import linear.MinimizationMethod;
import linear.impl.utils.Calculator;

import java.util.function.Function;

import static linear.impl.utils.MinimizationUtils.MAX_ITERATION;
import static linear.impl.utils.MinimizationUtils.validate;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class Dichotomy implements MinimizationMethod {

    @Override
    public Point minimize(Function<Double, Double> fun, double l, double r, double inaccuracy) {
        validate(fun, l, r, inaccuracy);
        int iteration = 0;
        Calculator calc = new Calculator(fun);
        while ((r - l) / 2 > inaccuracy) {
            if (iteration > MAX_ITERATION) {
                throw new IllegalStateException("Infinity cycle");
            }
            double x1 = (r + l - inaccuracy) / 2;
            double x2 = (r + l + inaccuracy) / 2;
            iteration++;
            if (calc.calc(x1).y < calc.calc(x2).y) {
                r = x2;
            } else {
                l = x1;
            }
        }
        return calc.calc((l + r) / 2);
    }
}
