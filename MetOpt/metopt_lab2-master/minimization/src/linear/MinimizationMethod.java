package linear;

import linear.geometry.Point;

import java.util.function.Function;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public interface MinimizationMethod {

    Point minimize(Function<Double, Double> fun, double l, double r, double inaccuracy);
}
