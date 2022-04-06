package solver

import matrix.Matrix

interface Solver<T : Matrix> {

    fun solve(matrix: T, b: MutableList<Double>, eps: Double): List<Double>
}
