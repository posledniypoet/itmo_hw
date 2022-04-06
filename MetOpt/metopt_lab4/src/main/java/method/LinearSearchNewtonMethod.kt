package method

import math.ScalarFunction
import method.minimize.Brent
import math.matrix.Vector
import math.solver.LUInPlaceSolver

class LinearSearchNewtonMethod : NewtonMethod() {
    override fun init(function: ScalarFunction, startPoint: Vector, inaccuracy: Double): Vector {
        return startPoint
    }

    override fun iterationStep(
        function: ScalarFunction,
        prevPoint: Vector,
        inaccuracy: Double
    ): Vector {
        val solver = LUInPlaceSolver()

        val g = function.gradient(prevPoint)
        val h = function.hessian(prevPoint)

        val d = solver.solve(h, -g, inaccuracy)

        val r = Brent().minimize(function, prevPoint, d, inaccuracy)

        return d * r
    }
}