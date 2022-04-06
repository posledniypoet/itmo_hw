package method

import math.ScalarFunction
import math.matrix.DenseMatrix
import math.matrix.Vector
import math.matrix.identityMatrix
import method.minimize.Brent


class DFPMethod : MinimizeMethod {
    override fun minimize(function: ScalarFunction, startPoint: Vector, inaccuracy: Double): Vector {
        var x1 = startPoint.copy()
        val g = identityMatrix(x1.size())
        var w1 = -function.gradient(x1)
        var p = w1.copy()
        var r = Brent().minimize(function, x1, p, inaccuracy)
        var x2 = x1 + (p * r)
        var delta = x2 - x1
        x1 = x2.copy()
        do {
            val w2 = -function.gradient(x1)
            val deltaW = w2 - w1
            w1 = w2.copy()
            val vk: Vector = g * deltaW
            g -= (DenseMatrix(delta * delta) *
                    (1.0 / (deltaW.scalar(delta)))) + (DenseMatrix(vk * vk) * (1.0 / (vk.scalar(deltaW))))


            p = g * w2
            r = Brent().minimize(function, x1, p, inaccuracy)
            x2 = x1 + (p * r)
            delta = x2 - x1
            x1 = x2.copy()
        } while (delta.norm() > inaccuracy)

        return x2
    }
}