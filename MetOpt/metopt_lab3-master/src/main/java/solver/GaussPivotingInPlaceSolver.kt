package solver

import matrix.DenseMatrix
import kotlin.math.abs

class GaussPivotingInPlaceSolver : Solver<DenseMatrix> {

    override fun solve(matrix: DenseMatrix, b: MutableList<Double>, eps: Double): List<Double> {
        (0 until matrix.size() - 1).forEach { k ->
            var max = 0.0
            var row = k
            (k until matrix.size()).forEach { i ->
                if (abs(max) < abs(matrix[i, k])) {
                    max = matrix[i, k]
                    row = i
                }
            }
            matrix.swapRows(k, row)
            b[k] = b[row].also { b[row] = b[k] }
            (k + 1 until matrix.size()).forEach { i ->
                val m = matrix[i, k] / max
                (k until matrix.size()).forEach { col ->
                    matrix[i, col] -= matrix[k, col] * m
                }
                b[i] -= b[k] * m
            }
        }

        val ans = MutableList(matrix.size()) { 0.0 }
        (matrix.size() - 1 downTo 0).forEach { i ->
            ans[i] = (b[i] -
                (i + 1 until matrix.size())
                    .sumOf { j -> matrix[i, j] * ans[j] }
                ) / matrix[i, i]
        }
        return ans
    }
}
