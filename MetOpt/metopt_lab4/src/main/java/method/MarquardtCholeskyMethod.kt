package method

import math.ScalarFunction
import math.matrix.DenseMatrix
import math.matrix.Vector
import math.matrix.identityMatrix
import math.solver.LUInPlaceSolver
import kotlin.math.max
import kotlin.math.sqrt

class MarquardtCholeskyMethod : MinimizeMethod {
    override fun minimize(function: ScalarFunction, startPoint: Vector, inaccuracy: Double): Vector {
        val solver = LUInPlaceSolver()

        var tau0 = 100.0
        val beta = 0.5

        var x = startPoint.copy()

        var tau = 0.0
        var g = function.gradient(x)
        var h = function.hessian(x)

        var kek: DenseMatrix
        var s = Vector(emptyList())
        var y = Vector(emptyList())
        var status = 2
        while (true) {
            if (++status == 2) {
                g = function.gradient(x)
                h = function.hessian(x)
                tau = tau0
            }
            if (++status == 3) {
                kek = h + identityMatrix(x.size()) * tau
                s = solver.solve(kek, -g, inaccuracy)
                y = x + s
                if (!isPositiveCholesky(kek)) {
                    tau = max(tau / beta, 1.0)
                    status = 3
                }
            }
            if (++status == 4) {
                x = y
                tau0 *= beta
                if (s.norm() < inaccuracy) {
                    break
                }
                status = 2
            }
        }

        return x
    }

    private fun isPositiveCholesky(matrix: DenseMatrix): Boolean {
        val n = matrix.size()

        for (k in 0 until n) {
            for (p in 0 until k) {
                matrix[k, k] = matrix[k, k] - matrix[k, p] * matrix[k, p]
            }
            if (matrix[k, k] <= 0.0) {
                return false
            }
            matrix[k, k] = sqrt(matrix[k, k])
            val r = 1.0 / matrix[k, k]
            for (i in k + 1 until n) {
                for (p in 0 until k) {
                    matrix[i, k] = matrix[i, k] - matrix[i, p] * matrix[k, p]
                }
                matrix[i, k] = matrix[i, k] * r
                matrix[k, i] = matrix[k, i] * matrix[i, k]
            }
        }

        return true
    }
}