package firstorder.impl;

import firstorder.FirstOrderMinimizationMethod;
import kotlin.Pair;
import linear.LinearMethodEnum;
import math.Function;
import math.Vector;

public class ConjugateGradient implements FirstOrderMinimizationMethod {

    @Override
    public Pair<Vector, Double> minimize(Function fun, Vector x, double inaccuracy, double ignored, LinearMethodEnum linearMethod) {
        int maxOps = 10000;
        int countOps = 0;
        Vector prevGrad = fun.grad(x);
        Vector p = prevGrad.times(-1);
        while (prevGrad.abs() > inaccuracy && countOps < maxOps) {
            Vector mul = fun.getA().times(p);
            double alpha = (prevGrad.scalar(prevGrad)) / (mul.scalar(p));
            x = x.plus(p.times(alpha));
            Vector grad = prevGrad.plus(mul.times(alpha));
            double beta = 0;
            if (countOps % x.size() != 0) {
                beta = (grad.scalar(grad)) / (prevGrad.scalar(prevGrad));
            }
            p = (p.times(beta)).minus(grad);
            prevGrad = grad;
            ++countOps;
        }
        return new Pair<>(x, fun.calc(x));
    }
}
