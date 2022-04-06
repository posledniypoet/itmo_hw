package firstorder;

import expression.Expression;

import kotlin.Pair;
import linear.LinearMethodEnum;
import math.Function;
import math.Vector;

public interface FirstOrderMinimizationMethod {
    Pair<Vector, Double> minimize(Function fun, Vector x, double inaccuracy, double alpha, LinearMethodEnum linearMethod);
}
