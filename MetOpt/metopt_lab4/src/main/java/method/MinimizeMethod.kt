package method

import math.ScalarFunction
import math.matrix.Vector

interface MinimizeMethod {
    fun minimize(
        function: ScalarFunction,
        startPoint: Vector,
        inaccuracy: Double
    ): Vector
}
