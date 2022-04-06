package firstorder.impl;

import firstorder.FirstOrderMinimizationMethod;
import kotlin.Pair;
import linear.LinearMethodEnum;
import math.Function;
import math.Vector;

public class GradientDescent implements FirstOrderMinimizationMethod {

    @Override
    public Pair<Vector, Double> minimize(Function fun, Vector x, double inaccuracy, double alpha, LinearMethodEnum linearMethod) {

        alpha = 2 / fun.maxEigenValue();
        int maxOps = 10000;
        int countOps = 0;
        Vector grad = fun.grad(x);

        while (grad.abs() > inaccuracy && countOps < maxOps) {
            Vector nextX = x.plus(grad.times(-alpha));
            if (fun.calc(nextX) >= fun.calc(x)) {
                alpha /= 2;
            } else {
                x = nextX;
                grad = fun.grad(x);
                ++countOps;
            }
        }
        return new Pair<>(x, fun.calc(x));
    }
}
