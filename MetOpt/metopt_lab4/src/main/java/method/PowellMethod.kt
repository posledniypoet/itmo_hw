package method

import math.ScalarFunction
import math.matrix.DenseMatrix
import math.matrix.Vector
import math.matrix.identityMatrix
import method.minimize.Brent

class PowellMethod : MinimizeMethod {
    val MAX_ITERATION = 100_000

    override fun minimize(
        function: ScalarFunction,
        startPoint: Vector,
        inaccuracy: Double
    ): Vector {
        var x = startPoint
        var gX = function.gradient(x)
        var d = -gX
        val g = identityMatrix(x.size())

        var iter = 0
        do {
            val r = Brent().minimize(function, x, d, inaccuracy)
            val s = d * r
            x += s

            val gY = gX

            gX = function.gradient(x)
            val p = gX - gY
            val u = s - Vector(g * p.data())
            g += DenseMatrix(u * u) * (1.0 / (u.scalar(p)))
            d = -Vector(g * gX.data())

        } while (s.norm() > inaccuracy && ++iter < MAX_ITERATION)

        return x
    }
}
