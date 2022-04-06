package method

import math.ScalarFunction
import math.matrix.Vector
import math.solver.LUInPlaceSolver

class ClassicNewtonMethod : NewtonMethod() {
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

        return solver.solve(h, -g, inaccuracy)
    }
}