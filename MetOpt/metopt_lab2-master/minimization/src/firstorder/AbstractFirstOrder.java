package firstorder;

import firstorder.impl.ConjugateGradient;
import firstorder.impl.FastDescent;
import firstorder.impl.GradientDescent;
import kotlin.Pair;
import linear.LinearMethodEnum;
import math.Function;
import math.Vector;

public class AbstractFirstOrder {
    public Pair<Vector, Double> minimize(MethodEnum type, Function fun, Vector x, double inaccuracy, double alpha, LinearMethodEnum linearMethod) {
        if (type == MethodEnum.FAST_DESCENT) {
            return new FastDescent().minimize(fun, x, inaccuracy, alpha, linearMethod);
        } else if (type == MethodEnum.GRADIENT_DESCENT) {
            return new GradientDescent().minimize(fun, x, inaccuracy, alpha, linearMethod);
        } else if (type == MethodEnum.CONJUGATE_GRADIENT) {
            return new ConjugateGradient().minimize(fun, x, inaccuracy, alpha, linearMethod);
        }
        throw new IllegalArgumentException("Expected MethodEnum: GRADIENT_DESCENT or FAST_DESCENT or CONJUGATE_GRADIENT");
    }
}
