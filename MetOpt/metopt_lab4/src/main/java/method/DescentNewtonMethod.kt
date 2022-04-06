package method

import math.ScalarFunction
import method.minimize.Brent
import math.matrix.Vector
import math.solver.LUInPlaceSolver


class DescentNewtonMethod : NewtonMethod() {
    override fun init(function: ScalarFunction, startPoint: Vector, inaccuracy: Double): Vector {
        val d = -function.gradient(startPoint)
        val r = Brent().minimize(function, startPoint, d, inaccuracy)
        return startPoint + (d * r)
    }

    override fun iterationStep(
        function: ScalarFunction,
        prevPoint: Vector,
        inaccuracy: Double
    ): Vector {
        val solver = LUInPlaceSolver()

        val g = function.gradient(prevPoint)
        val h = function.hessian(prevPoint)

        val s = solver.solve(h, -g, inaccuracy)

        val d = if (s.scalar(g) < 0) {
            s
        } else {
            -g
        }

        val r = Brent().minimize(function, prevPoint, d, inaccuracy)

        return d * r
    }
}
