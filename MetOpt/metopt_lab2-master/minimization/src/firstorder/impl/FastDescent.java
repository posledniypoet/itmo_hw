package firstorder.impl;

import firstorder.FirstOrderMinimizationMethod;
import kotlin.Pair;
import linear.LinearMethodEnum;
import math.Function;
import math.Vector;

public class FastDescent implements FirstOrderMinimizationMethod {

    @Override
    public Pair<Vector, Double> minimize(Function fun, Vector x, double inaccuracy, double left,
                                         LinearMethodEnum linearMethod) {
        int maxOps = 10000;
        int countOps = 0;
        double maxAlpha = 2 / fun.maxEigenValue();
        Vector prevX = x;
        Vector grad = fun.grad(x);

        while (fun.grad(x).abs() > inaccuracy &&
                (countOps == 0 || Math.abs(fun.calc(prevX) - fun.calc(x)) > inaccuracy * 0.01) &&
                countOps < maxOps) {
            final Vector finalPoint = x;
            final Vector finalGrad = grad;
            java.util.function.Function<Double, Double> function = (val) ->
                    fun.calc(finalPoint.plus(finalGrad.times(-val)));
            prevX = x;
            double alpha = linearMethod.create().minimize(function, 0, maxAlpha, inaccuracy).x;
            x = x.plus(grad.times(-alpha));
            grad = fun.grad(x);
            ++countOps;
        }
        return new Pair<>(x, fun.calc(x));
    }
}
