package linear.impl.utils;

import expression.Expression;
import expression.nullary.Variable;
import linear.geometry.Point;

import java.util.EnumMap;
import java.util.function.Function;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class MinimizationUtils {

    public static final int MAX_ITERATION = 1000;

    private static Point calc(double x, Expression fun) {
        var vars = new EnumMap<Variable, Double>(Variable.class);
        vars.put(Variable.X, x);
        double f = fun.evaluate(vars);
        return new Point(x, f);
    }

    public static void validate(Function<Double, Double> fun, double l, double r, double inaccuracy) {
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
}
