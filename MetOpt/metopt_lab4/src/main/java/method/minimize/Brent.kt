package method.minimize

import math.ScalarFunction
import math.matrix.Vector
import java.util.function.Function
import java.util.stream.Collectors

class Brent {
    fun minimize(
        function: ScalarFunction,
        x: Vector,
        d: Vector,
        inaccuracy: Double
    ): Double {
        return JavaBrent().minimize(
            { lambda: Double -> function.calc(x + d * lambda) },
            2 * inaccuracy, 10.0, inaccuracy
        )
    }
}
