package method

import math.ScalarFunction
import math.matrix.Vector

abstract class NewtonMethod : MinimizeMethod {
    val MAX_ITERATION = 100_000

    override fun minimize(
        function: ScalarFunction,
        startPoint: Vector,
        inaccuracy: Double
    ): Vector {
        var x = init(function, startPoint, inaccuracy)
        var iter = 0
        do {
            val delta = iterationStep(function, x, inaccuracy)
            x += delta

        } while (delta.norm() > inaccuracy && ++iter < MAX_ITERATION)

        return x
    }

    abstract fun init(
        function: ScalarFunction,
        startPoint: Vector,
        inaccuracy: Double
    ): Vector

    abstract fun iterationStep(
        function: ScalarFunction,
        prevPoint: Vector,
        inaccuracy: Double
    ): Vector
}