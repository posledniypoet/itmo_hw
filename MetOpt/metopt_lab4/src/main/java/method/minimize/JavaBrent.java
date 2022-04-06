package method.minimize;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JavaBrent {

    private static final double K = (3. - Math.sqrt(5)) / 2.;
    private static final int MAX_ITERATION = 1000;

    public Double minimize(Function<Double, Double> fun, double l, double r, double inaccuracy) {
        double x = (l + r) / 2.;
        double w = x;
        double v = x;
        double d = r - l;
        double e = r - l;
        double fx = fun.apply(x);
        double fv = fun.apply(v);
        double fw = fun.apply(w);
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
                    iteration++;
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
                iteration++;
            }
            if (Math.abs(r - l) < inaccuracy) {
                return fun.apply(x);
            }
            double fu = fun.apply(u);
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
