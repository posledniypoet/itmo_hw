package method

import math.ScalarFunction
import math.matrix.Vector
import math.matrix.identityMatrix
import math.solver.LUInPlaceSolver

class MarquardtMethod : MinimizeMethod {
    override fun minimize(function: ScalarFunction, startPoint: Vector, inaccuracy: Double): Vector {
        val solver = LUInPlaceSolver()

        var tau0 = 100.0
        val beta = 0.5

        var x = startPoint.copy()
        var fX = function.calc(x)
        var tau = tau0
        var g = function.gradient(x)
        var h = function.hessian(x)
        var s = Vector(emptyList())
        var y = Vector(emptyList())
        var fY = 0.0
        var status = 2
        while (s.norm() > inaccuracy) {
            if (++status == 2) {
                g = function.gradient(x)
                h = function.hessian(x)
                tau = tau0
            }
            if (++status == 3) {
                val kek = h + identityMatrix(x.size()) * tau
                s = solver.solve(kek, -g, inaccuracy)
                y = x + s
                fY = function.calc(y)
                if (fY >= fX) {
                    tau /= beta
                    status = 3
                }
            }

            if (++status == 4) {
                x = y
                fX = fY
                tau0 *= beta
                status = 2
            }
        }

        return x
    }
}