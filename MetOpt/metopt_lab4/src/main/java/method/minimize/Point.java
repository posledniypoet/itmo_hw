package method.minimize;

import java.util.Comparator;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class Point {

    public static final Comparator<Point> cmpByX =
            (a, b) -> a.x == b.x
                    ? Double.compare(a.y, b.y)
                    : Double.compare(a.x, b.x);

    public static final Comparator<Point> cmpByY =
            (a, b) -> a.y == b.y
                    ? Double.compare(a.x, b.x)
                    : Double.compare(a.y, b.y);

    public final double x;
    public final double y;

    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    @Override
    public String toString() {
        return "Point { " +
                "x = " + x + ", " +
                "y = " + y +
                " }";
    }
}
