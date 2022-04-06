package math.solver

import math.matrix.Matrix
import math.matrix.Vector

interface Solver<T : Matrix> {

    fun solve(matrix: T, vector: Vector, inaccuracy: Double): Vector
}
