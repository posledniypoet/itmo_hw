package linear.impl.utils;

import linear.geometry.Point;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

public class Calculator {

    private final Function<Double, Double> fun;
    private final Map<Double, Point> memory;

    public Calculator(Function<Double, Double> fun) {
        this.fun = fun;
        this.memory = new HashMap<>();
    }

    public Point calc(double x) {
        if (memory.containsKey(x)) {
            return memory.get(x);
        }
        Point point = new Point(x, fun.apply(x));
        memory.put(x, point);
        return point;
    }

    public int countedPoints() {
        return memory.size();
    }
}
